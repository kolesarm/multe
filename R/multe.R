#' Contamination Bias Diagnostics for Multiple Treatments
#'
#' @param r Fitted model, output of the \code{lm} function.
#' @param treatment_name name of treatment variable
#' @param cluster Factor variable that defines clusters. If \code{NULL} (or not
#'     supplied), the command computes heteroscedasticity-robust standard
#'     errors, rather than cluster-robust standard errors.
#' @return Returns a list with the following components: TODO
#' @export
multe <- function(r, treatment_name, cluster=NULL) {
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

    if (!is.null(wgt) && any(wgt == 0)) {
        ok <- wgt != 0
        Y <- Y[ok]
        X <- X[ok]
        Cm <- Cm[ok, , drop=FALSE]
        S <- droplevels(S[ok])
        wgt <- wgt[ok]
        cluster <- cluster[ok]
    }

    build_matrix <- function(Cm, S)  {
        if (nlevels(S) > 1) {
            cbind(stats::model.matrix(~ S), Cm)
        } else {
            cbind(rep(1, NROW(Cm)), Cm)
        }
    }

    r1 <- decomposition(Y, X, build_matrix(Cm, S), wgt, cluster)
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
        r2 <- decomposition(Y, X, Zm, wgt, cluster)
        n2 <- length(Y)
        k2 <- NCOL(Zm)-1L
    }

    structure(list(est_f=r1$A, est_o=r2$A, cb_f=r1$B, cb_o=r2$B, n_f=n1,
                   n_o=n2, k_f=k1, k_o=k2, t_f=r1$tests, t_o=r2$tests),
              class="multe")
}

#' @export
print.multe <- function(x, digits=getOption("digits"), ...) {
    cat("Estimates on full sample:\n")
    print(x$est_f, digits=digits)
    if (!is.null(x$est_o)) {
        cat("\nEstimates on overlap sample:\n")
        print(x$est_o, digits=digits)
    }
}
