## Generalized inverse of a symmetric matrix
ginv <- function(A, tol = (.Machine$double.eps)^(3/5)) {
    e <- eigen(A)
    pos <- e$values >= max(tol * e$values[1L], 0)
    list(inverse=e$vectors[, pos] %*% (1/e$values[pos] * t(e$vectors[, pos])),
         rank=sum(pos))
}
## Compute quadratic form b'A^{+}b
qfp <- function(A, b, tol) {
    Ap <- ginv(A, tol)
    list(qf=sum(b* (Ap$inverse %*% b)), rank=Ap$rank)
}
## Appears to create fewer problems than b'A^{-}b:
## r <- stats::lm.fit(A, b, tol=tol)$coefficients
## list(qf=sum((b*r)[!is.na(r)]), rank=sum(!is.na(r)))


## scale range of vector to [0,1]
scaleRange <- function(x) {
    d <- diff(range(x))
    if (d>0) x <- (x-min(x))/d
    x
}

## Influence function computation, diag(res)*x*(x'x)^{-1}, with qrx=qr(x)
reg_if <- function(res, qrx) {
    ret <- array(dim=dim(qrx$qr))
    Q <- qr.Q(qrx)[, seq.int(qrx$rank)]
    R <- qr.R(qrx)[seq.int(qrx$rank), seq.int(qrx$rank)]
    ret[, qrx$pivot[seq.int(qrx$rank)]] <- t(backsolve(R, t(res*Q)))
    ret
}

Vhat <- function(psi, cluster=NULL) {
    if (!is.null(cluster)) {
        if (!is.factor(cluster))
            stop("Cluster variable must be a factor")
        cluster <- droplevels(cluster)
        nS <- nlevels(cluster)
        psi <- sqrt(nS / (nS-1)) *
            apply(as.matrix(psi), 2, function(x) tapply(x, cluster, sum))
    }
    crossprod(psi)
}

sehat <- function(psi, cluster=NULL) {
    sqrt(diag(Vhat(psi, cluster)))
}


## Hessian for multinomial likelihood
multHessian <- function(ml, Z=stats::model.matrix(ml)) {
    pis <- ml$fitted.values
    if (NCOL(pis)==1) {
        pis <- cbind(1 - pis, pis)
    }
    wgt <- drop(ml$weights)
    L <- NCOL(Z)
    K <- NCOL(pis)-1

    He <- matrix(nrow=K*L, ncol=K*L)
    for (k in seq_len(K)) {
        for (j in seq_len(K)) {
            He[(L * (k-1) + 1):(L*k), (L * (j-1) + 1):(L*j)] <-
                crossprod(wgt*Z, pis[, k+1] * ((k==j)-pis[, j+1])*Z)
        }
    }
    He
}

## matrix of controls Zm, first column is the intercept; X must be a factor,
## first level to be dropped. wgt are weights, and cluster is a factor variable
## signifying cluster membership
decomposition <- function(Y, X, Zm, wgt=NULL, cluster=NULL, tol=1e-7,
                          cw_uniform=FALSE) {
    ## For nnet::multinom, if rhs variables not roughly scaled to [0,1] fit will
    ## be slow or may not converge at all.
    Zm <- apply(Zm, 2, scaleRange)

    ## Drop columns with no variation
    vars <- apply(Zm[, -1, drop=FALSE], 2L, stats::var)
    if (sum(vars==0)>0) {
        message("Dropping these controls, since they have no variation:",
                colnames(Zm[, -1])[sum(vars==0)])
        Zm <- Zm[, c(TRUE, vars>0)]
    }

    K <- nlevels(X)-1
    L <- ncol(Zm)
    estB <- estA <- seO <- seP <- seB <- matrix(ncol=5, nrow=K)
    ws <- if (is.null(wgt)) rep(1L, length(Y)) else sqrt(wgt)

    Xf <- outer(X, levels(X), `==`) + 0    # full X matrix
    rl <- stats::lm(Y ~ 0 + Xf[, -1] + Zm, weights=wgt) # partly linear model
    estA[, 1] <- rl$coefficients[1:K]
    tX <- stats::lm(Xf[, -1] ~ 0 + Zm, weights=wgt)$residuals # Xtilde matrix
    psi_beta <- reg_if(ws*rl$residuals, qr(ws*tX))
    seP[, 1] <- sehat(psi_beta, cluster)

    ## ATE
    ri <- stats::lm(Y~0+Zm:Xf, weights=wgt)             # interactions model
    gam <- matrix(ri$coefficients[-(1:L)], nrow=L)-ri$coefficients[1:L]
    Zb <- apply(Zm, 2, stats::weighted.mean, ws^2) # weighted colMeans(Zb)
    estA[, 3] <- Zb %*% gam
    psi_al <- reg_if(ws*ri$residuals, ri$qr) # psi(alpha)

    psi_ate <- vapply(0:K, function(k) drop(psi_al[, (k*L+1):((k+1)*L)] %*% Zb),
                      numeric(length=NROW(Zm)))
    psi_ate <- psi_ate[, -1]-psi_ate[, 1]
    seO[, 3] <- sehat(psi_ate, cluster)
    psi_po <- psi_ate + ws^2 * (Zm-outer(rep(1, NROW(Zm)), Zb)) %*%
        gam/sum(ws^2)
    seP[, 3] <- sehat(psi_po, cluster)
    seB[, 3] <- sehat(psi_beta-psi_po, cluster)

    for (k in seq_len(K)) {
        rk <- stats::lm(Xf[, k+1]*Zm ~ 0+Xf[, -1]+Zm, weights=wgt)
        deltak <- rk$coefficients[k, ]
        estA[k, 2] <- sum(gam[deltak!=0, k]*deltak[deltak!=0])
        ## doubletilde(X)
        if (K==1) {
            dtX <- tX
        } else {
            dtX <- stats::lm(tX[, k]~0+tX[, -k], weights=wgt)$residuals
        }
        pp <- psi_al[, (k*L+1):((k+1)*L)]- psi_al[, 1:L]
        psi_ownk <- drop(pp[, deltak!=0, drop=FALSE] %*% deltak[deltak!=0]+
                             (ws^2*dtX*rk$residuals/
                                  sum(ws^2*dtX^2))[, deltak!=0, drop=FALSE] %*%
                                 gam[deltak!=0, k])

        seP[k, 2] <- sehat(psi_ownk, cluster)
        seB[k, 2] <- sehat(psi_beta[, k]-psi_ownk, cluster)

        ## 1 at a time
        s <- (X==levels(X)[1] | X==levels(X)[k+1])
        Xhat <- stats::lm(Xf[, k+1] ~ 0+Zm, subset=s, weights=wgt)$residuals
        rk <- stats::lm(Y[s]~0+Xhat+Zm[s, ], weights=wgt[s])

        estA[k, 4] <- rk$coefficients[1]
        psi_k <- ws[s]^2*Xhat/sum(ws[s]^2*Xhat^2)
        seP[k, 4] <- sehat(rk$residuals*psi_k, cluster[s])
        seO[k, 4] <- sehat(ri$residuals[s]*psi_k, cluster[s])

        ## Test against 1 at a time
        psi_1 <- psi_beta[, k]
        psi_1[s] <- psi_1[s]- rk$residuals*psi_k
        seB[k, 4] <- sehat(psi_1, cluster)
    }

    ## Generalized overlap weights + Wald and LM tests

    ## nnet by Brian Ripley. Convergence: ml$convergence==0, ml$value
    ml <- nnet::multinom(X~0+Zm, abstol=1e-12, reltol=1e-12, trace=FALSE,
                         maxit=1e4, weights=wgt, MaxNWts=1e4)
    ml$fitted.values[ml$fitted.values < max(ml$fitted.values)*1e-6] <- 0
    if (NCOL(ml$fitted.values)==1) {
        ml$fitted.values <- cbind(1 - ml$fitted.values, ml$fitted.values)
    }
    pis <- ml$fitted.values
    score <- function(pis) {
        ws^2*matrix(apply((Xf-pis)[, -1, drop=FALSE], 2,
                          function(x) x*Zm), nrow=NROW(Zm))
    }
    Sc <- score(pis)
    He <- multHessian(ml)

    ## Wald and LM tests. Assume first element of Z is intercept
    idx1 <- (0:(K-1))*L+1
    He1112 <- solve(He[idx1, idx1, drop=FALSE],
                    He[idx1, -idx1, drop=FALSE]) ## H11^{-1}H12
    Vu <- Vhat(Sc[, -idx1, drop=FALSE] -
                   Sc[, idx1, drop=FALSE] %*% He1112, cluster)
    th1 <- stats::coef(ml)
    if (is.vector(th1)) th1 <- matrix(th1, nrow=1)
    th <- drop((He[-idx1, -idx1, drop=FALSE]-
                    He[-idx1, idx1, drop=FALSE] %*% He1112) %*%
                   as.vector(t(th1[, -1, drop=FALSE])))
    ## LM, calculate weighted colMeans(Xf)
    pis0 <- outer(rep(1, NROW(Xf)), apply(Xf, 2, stats::weighted.mean, ws^2))
    Scr <- score(pis0)
    Scr1 <- Scr[, idx1, drop=FALSE]
    Scr2 <- Scr[, -idx1, drop=FALSE]

    ml$fitted.values <- pis0
    Her <- multHessian(ml)
    Her1112 <- solve(Her[idx1, idx1, drop=FALSE], Her[idx1, -idx1, drop=FALSE])
    Vr <- Vhat(Scr2 - Scr1 %*% Her1112, cluster)
    testcov <- function(tol) {
        LM <- qfp(Vr, colSums(Scr2), tol=tol)
        Wa <- qfp(Vu, th, tol=tol)
        list(W=Wa$qf, W_df=Wa[[2]], p_W=1-stats::pchisq(Wa$qf, df=Wa[[2]]),
             LM=LM$qf, LM_df=LM[[2]], p_LM=1-stats::pchisq(LM$qf, df=LM[[2]]),
             tol=tol)
    }
    tests <- testcov(tol)
    tests2 <- testcov(tol*1e-3)
    if (max(abs(unlist(tests)-unlist(tests2))[4:6])) {
        warning("LM statistic depends on numerical tolerance.\nAt tol=", tol,
                ", the statistic is: ", round(tests$LM, 2), ", with df: ",
                round(tests$LM_df, 2), "\nAt tol=", tol*1e-3,
                ", the statistic is: ", tests2$LM, ", with df: ", tests2$LM_df)
    }

    ## Generalized overlap weights + standard errors
    if (!cw_uniform)
        vpi <- pis0 * (1-pis0)
    else
        vpi <- 1
    lam <- 1/rowSums(vpi/pis)
    lam[lam<max(lam)*1e-6] <- 0 # round to exact zero
    ipi <- 1/pmax(pis, 1e-10)    # don't divide by zero
    cw <- lam/rowSums(Xf*pis)

    if (sum(lam)>0) {
        ro <- stats::lm(Y~X, weights=cw*ws^2)
        estA[, 5] <- ro$coefficients[-1]
        psi_or <- ws^2*lam/sum(ws^2*lam) * (Xf[, -1]*ipi[, -1]-Xf[, 1]*ipi[, 1])
        seO[, 5] <- sehat(ri$residuals*psi_or, cluster)

        ## Final step: calculate se_po for overlap weights. model.matrix rather
        ## than ml$residuals to account for rounding
        f1 <- function(x) colSums(x*ws^2*cw*ro$residuals*Zm)
        M0 <- as.vector(apply(lam * (vpi*ipi)[, -1, drop=FALSE]*Xf[, 1], 2, f1))
        M <- matrix(nrow=K*L, ncol=K) # M_k(theta)
        for (k in seq_len(K)) {
            M[, k] <- as.vector(apply((lam* (vpi*ipi)[, -1]-rep(1, nrow(Zm)) %o%
                                           ((1:K)==k)) * Xf[, k+1], 2, f1))
        }
        ## TODO: Drop the collinear columns, then decrease tol to ensure it
        ## inverts
        qHe <- qr(He)
        idx <- qHe$pivot[seq.int(qHe$rank)]
        a <- t(crossprod((M-M0)[idx, ], qr.solve(He[idx, idx], t(Sc[, idx]),
                                                 tol=1e-10))) / sum(ws^2*lam)

        seP[, 5] <- sehat(ro$residuals*psi_or+a, cluster)
        seB[, 5] <- sehat(psi_beta-ro$residuals*psi_or-a, cluster)

    } else {
        message("Sample for efficient common weights is empty.")
    }

    estB[, -1] <- estA[, 1]-estA[, -1]

    rownames(estA) <- rownames(estB) <- levels(X)[-1]
    colnames(estA) <- colnames(estB) <- c("PL", "OWN", "ATE", "EW", "CW")
    rownames(seP) <- rownames(seB) <- rep("pop_se", nrow(estB))
    rownames(seO) <- rep("oracle_se", nrow(estB))

    f2 <- function(k) {
        rbind(estA[k, , drop=FALSE], seP[k, , drop=FALSE],
              seO[k, , drop=FALSE])
    }
    T1 <- do.call(rbind, lapply(1:K, f2))
    T2 <- rbind(estB, seB)[rep(seq_len(K), each=2) + c(0, K), ]
    list(A=T1, B=T2, tests=tests,
         pscore_sd=sqrt(diag(stats::cov.wt(pis, ws^2, method="ML")$cov)))
}
