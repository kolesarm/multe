build_matrix <- function(Cm, S)  {
    if (nlevels(S) > 1) {
        cbind(stats::model.matrix(~ S), Cm)
    } else {
        cbind(rep(1, NROW(Cm)), Cm)
    }
}

#' Multiple Treatment Effects Regression
#'
#' Compute contamination bias diagnostics for the partially linear (PL)
#' regression estimator with multiple treatments. Also report four alternative
#' estimators:
#' \describe{
#' \item{OWN}{The own treatment effect component of the PL estimator.}
#' \item{ATE}{The unweighted average treatment effect, implemented using
#'            interacted regression.}
#' \item{EW}{Weighted ATE estimator based on easiest-to-estimate weighting (EW)
#'           scheme,
#'           implemented by running one-treatment-at-a-time regressions.}
#' \item{CW}{Weighted ATE estimator using easiest-to-estimate common
#'           weighting (CW) scheme, implemented using weighted regression.}
#' }
#' @param r Fitted model, output of the \code{lm} function.
#' @param treatment_name name of treatment variable
#' @param cluster Factor variable that defines clusters. If \code{NULL} (or not
#'     supplied), the command computes heteroscedasticity-robust standard
#'     errors, rather than cluster-robust standard errors.
#' @param tol Numerical tolerance for computing LM test statistic for testing
#'     variability of the propensity score.
#' @param cw_uniform For the CW estimator, should the target weighting scheme
#'     give all comparisons equal weight (if \code{FALSE}), or should it draw
#'     from the marginal empirical treatment distribution (if \code{TRUE})?
#' @return Returns a list with the following components: \describe{
#'
#' \item{est_f}{Data frame with alternative estimators and standard errors for
#' the full sample}
#'
#' \item{est_o}{Data frame with alternative estimators and standard errors for
#' the overlap sample}
#'
#' \item{cb_f, cb_0}{Data frame with differences between PL and alternative
#' estimators, along with standard errors for the full, and for the overlap
#' sample.}
#'
#' \item{n_f, n_o}{Sample sizes for the full, and for the overlap sample.}
#'
#' \item{k_f, k_o}{Number of controls for the full, and for the overlap sample.}
#'
#' \item{t_f, t_o}{LM and Wald statistic, degrees of freedom, and p-values for
#' the full and for the overlap sample, for testing the hypothesis of no
#' variation in the propensity scores.}
#'
#' \item{Y, X, wgt}{Vector of outcomes, treatments and weights in the overlap
#'                 sample}
#'
#' \item{Zm}{Matrix of controls in the overlap sample}
#' }
#' @references{
#'
#' \cite{Paul Goldsmith-Pinkham, Peter Hull, and Michal Kolesár. Contamination
#' bias in linear regressions. ArXiv:2106.05024, August 2022.}
#' }
#' @examples
#' wbh <- fl[fl$race=="White" | fl$race=="Black" | fl$race=="Hispanic", ]
#' wbh <- droplevels(wbh)
#' r1 <- stats::lm(std_iq_24~race+factor(age_24)+female, weight=W2C0, data=wbh)
#' m1 <- multe(r1, treatment="race")
#' @export
multe <- function(r, treatment_name, cluster=NULL, tol=1e-7, cw_uniform=FALSE) {
    Y <- stats::model.response(r$model)
    wgt <- stats::model.weights(r$model)
    X <- r$model[, treatment_name]
    ## Find factor variable with greatest number of levels
    nl <- vapply(r$model, nlevels, numeric(1))
    nl[treatment_name] <- 0
    if (max(nl) > 0) {
        stratum_name <-  names(which.max(nl))
        S <- r$model[, stratum_name]
    } else {
        stratum_name <- "(Intercept)"
        S <- factor(rep(1, length(X)))
    }
    coefnames <- names(stats::coefficients(r))
    filter <- (coefnames=="(Intercept)") |
        coefnames %in% paste0(treatment_name,
                              levels(r$model[[treatment_name]])) |
        coefnames %in% paste0(stratum_name,
                              levels(r$model[[stratum_name]])) |
        unname(is.na(r$coefficients))
    if (sum(is.na(r$coefficients)) > 0) {
        message("These columns are dropped due to collinearity: ",
                paste(names(r$coefficients)[is.na(r$coefficients)],
                      collapse=", "))
    }
    Cm <- stats::model.matrix(r)[, !filter, drop=FALSE]
    if (NCOL(build_matrix(Cm, S))==1) {
        stop("There are no controls beyond the intercept")
    }

    if (!is.null(wgt) && any(wgt == 0)) {
        ok <- wgt != 0
        Y <- Y[ok]
        X <- X[ok]
        Cm <- Cm[ok, , drop=FALSE]
        S <- droplevels(S[ok])
        wgt <- wgt[ok]
        cluster <- cluster[ok]
    }

    r1 <- decomposition(Y, X, build_matrix(Cm, S), wgt, cluster, tol,
                        cw_uniform)

    n1 <- length(Y)
    k1 <- NCOL(build_matrix(Cm, S))-1L

    ## 1. Drop strata with no overlap
    idx <- vector(length=0)
    if (nlevels(S) > 1) {
        dropstrata <- levels(S)[colSums(table(X, S)==0)>0]
        idx <- S %in% dropstrata
        if (sum(idx)> 0) {
            message("For variable ", stratum_name,
                    " the following levels fail overlap:\n",
                    paste(dropstrata, collapse=", "),
                    "\nDropping observations with these levels")
        }
        Y <- Y[!idx]
        X <- X[!idx]
        Cm <- Cm[!idx, , drop=FALSE]
        S <- droplevels(S[!idx])
        wgt <- wgt[!idx]
        cluster <- cluster[!idx]
    }

    ## 2. Drop controls that don't have within-treatment variation
    Zm <- build_matrix(Cm, S)
    rs <- function(x) {
        qrz <- qr(Zm[X==x, ])
        qrz$pivot[-seq.int(qrz$rank)]
    }
    dropctrl <- unique(unlist(lapply(levels(X), rs)))

    if (sum(dropctrl)> 0) {
        message("\nThe following variables have no within-treatment variation",
                " and are dropped:\n",
                paste(colnames(Zm)[sort(dropctrl)], collapse=", "))
        Zm <- Zm[, -dropctrl]
    }

    r2 <- list()
    n2 <- k2 <- NA

    if (length(Y)==0) {
        message("Overlap sample is empty")
    } else if (sum(dropctrl) > 0 || sum(idx) > 0) {
        r2 <- decomposition(Y, X, Zm, wgt, cluster, tol, cw_uniform)
        n2 <- length(Y)
        k2 <- NCOL(Zm)-1L
    }

    structure(list(est_f=r1$A, est_o=r2$A, cb_f=r1$B, cb_o=r2$B, n_f=n1,
                   n_o=n2, k_f=k1, k_o=k2, t_f=r1$tests, t_o=r2$tests,
                   Y=Y, X=X, Zm=Zm, wgt=wgt),
              class="multe")
}

#' @export
print.multe <- function(x, digits=getOption("digits"), ...) {
    cat("Estimates on full sample:\n")
    oracle <- (seq.int(nrow(x$est_f)/3)-1)*3+3
    rownames(x$est_f)[oracle-1] <- rep("SE", length(oracle))
    print(x$est_f[-oracle, ], digits=digits)
    if (!is.null(x$est_o)) {
        rownames(x$est_o)[oracle-1] <- rep("SE", length(oracle))
        cat("\nEstimates on overlap sample:\n")
        print(x$est_o[-oracle, ], digits=digits)
    }
    cat("\nP-values for null hypothesis of no propensity score variation:\n")
    cat("Wald test:", round(x$t_f$p_W, digits=digits))
    cat(", LM test:", round(x$t_f$p_LM, digits=digits), "\n")

}
