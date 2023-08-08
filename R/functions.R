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


## Generalized inverse of a symmetric matrix
ginv <- function(A, tol = (.Machine$double.eps)^(3/5)) {
    e <- eigen(A)
    pos <- e$values >= max(tol * e$values[1L], 0)
    list(inverse=e$vectors[, pos] %*% (1/e$values[pos] * t(e$vectors[, pos])),
         rank=sum(pos))
}


## matrix of controls Zm, first column is the intercept; X must be a factor,
## first level to be dropped. wgt are weights, and cluster is a factor variable
## signifying cluster membership
decomposition <- function(Y, X, Zm, wgt=NULL, cluster=NULL) {
    ## Drop columns with no variation
    vars <- apply(Zm[, -1, drop=FALSE], 2L, stats::var)
    if (sum(vars==0)>0) {
        message("Dropping these controls, since they have no variation:",
                colnames(Zm[, -1])[sum(vars==0)])
        Zm <- Zm[, c(TRUE, vars>0)]
    }

    K <- nlevels(X)-1
    L <- ncol(Zm)
    estA <- seO <- seP <- seB <- matrix(ncol=5, nrow=K)
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
    Zb <- if (is.null(wgt)) colMeans(Zm) else apply(Zm, 2, stats::weighted.mean,
                                                    wgt)
    estA[, 3] <- Zb %*% gam
    psi_al <- reg_if(ws*ri$residuals, ri$qr)

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
        dtX <- stats::lm(tX[, k]~0+tX[, -k], weights=wgt)$residuals
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

    ## Generalized overlap weights
    Zm <- scale(Zm, center=FALSE) # otherwise numerical issues
    ml <- nnet::multinom(X~0+Zm, abstol=1e-12, reltol=1e-12, trace=FALSE,
                         maxit=1e3, weights=wgt, , MaxNWts=10000)
    pis <- ml$fitted.values
    pis[pis<max(pis)*10e-6] <- 0
    lam <- 1/rowSums(1/pis)
    ipi <- 1/pmax(pis, 1e-10)    # don't divide by zero
    lam[lam<max(lam)*10e-6] <- 0 # round to exact zero
    cw <- lam/rowSums(Xf*pis)

    tests <- list(W=NA, df=NA, p_W=NA, LM=NA, df=NA, p_LM=NA)

    Sc <- ws^2*matrix(apply((Xf-pis)[, -1], 2,
                            function(x) x*Zm), nrow=NROW(Zm))
    Hessian <- function(pis) {
        He <- matrix(nrow=K*L, ncol=K*L)
        for (k in seq_len(K)) {
            for (j in seq_len(K)) {
                He[(L * (k-1) + 1):(L*k), (L * (j-1) + 1):(L*j)] <-
                    crossprod(ws*Zm, pis[, k+1] * ((k==j)-pis[, j+1])*ws*Zm)
            }
        }
        He
    }
    He <- Hessian(pis)

    ## Wald and LM tests. First restricted score and Hessian, assume first
    ## element of Z is intercept
    Scr <- ws^2*matrix(apply(scale(Xf[, -1], center=TRUE, scale=FALSE), 2,
                             function(x) x*Zm), nrow=NROW(Zm))
    idx1 <- (0:(K-1))*L+1
    Scr2 <- Scr[, -idx1]
    Scr1 <- Scr[, idx1]
    Her <- Hessian(outer(rep(1, NROW(Xf)), colMeans(Xf)))
    Her11 <- Her[idx1, idx1]
    Her12 <- Her[idx1, -idx1]
    Vr <- Vhat(Scr2 - Scr1 %*% solve(Her11, Her12), cluster)
    invVr <- ginv(Vr)
    LM <- drop(crossprod(colSums(Scr2), invVr$inverse %*% colSums(Scr2)))

    theta2 <- as.vector(t(stats::coef(ml)[, -1]))
    Sc2 <- Sc[, -idx1]
    Sc1 <- Sc[, idx1]
    He11 <- He[idx1, idx1]
    He12 <- He[idx1, -idx1]
    He21 <- He[-idx1, idx1]
    He22 <- He[-idx1, -idx1]
    Vu <- Vhat(Sc2 - Sc1 %*% solve(He11, He12), cluster)
    invVu <- ginv(Vu)
    th <- drop((He22-He21 %*% solve(He11, He12)) %*% theta2)
    Wa <- drop(crossprod(th, invVu$inverse %*% th))
    tests <- list(W=Wa, df=invVu$rank, p_W=1-stats::pchisq(Wa, df=invVu$rank),
                  LM=LM, df=invVr$rank, p_LM=1-stats::pchisq(LM, df=invVr$rank))

    ## Generalized overlap weights: standard errors
    if (sum(lam)>0) {
        ro <- stats::lm(Y~X, weights=cw*ws^2)
        estA[, 5] <- ro$coefficients[-1]
        psi_or <- ws^2*lam/sum(ws^2*lam) * (Xf[, -1]*ipi[, -1]-Xf[, 1]*ipi[, 1])
        seO[, 5] <- sehat(ri$residuals*psi_or, cluster)

        ## Final step: calculate se_po for overlap weights. First score and
        ## Hessian. model.matrix rather than ml$residuals to account for
        ## rounding
        f1 <- function(x) colSums(x*ws^2*cw*ro$residuals*Zm)
        M0 <- as.vector(apply(lam*ipi[, -1]*Xf[, 1], 2, f1))
        M <- matrix(nrow=K*L, ncol=K) # M_k(theta)
        for (k in seq_len(K)) {
            M[, k] <- as.vector(apply((lam*ipi[, -1]-rep(1, nrow(Zm)) %o%
                                           ((1:K)==k)) * Xf[, k+1], 2, f1))
        }
        ## Drop the collinear columns, then decrease tol to ensure it inverts
        qHe <- qr(He)
        idx <- qHe$pivot[seq.int(qHe$rank)]
        a <- t(crossprod((M-M0)[idx, ], qr.solve(He[idx, idx], t(Sc[, idx]),
                                                 tol=1e-10))) / sum(ws^2*lam)

        seP[, 5] <- sehat(ro$residuals*psi_or+a, cluster)
        seB[, 5] <- sehat(psi_beta-ro$residuals*psi_or-a, cluster)

    } else {
        message("Sample for efficient common weights is empty.")
    }

    estB <- cbind(estA[, 1]*NA, estA[, 1]-estA[, -1])

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
    list(A=T1, B=T2, tests=tests)
}


write_rd_table <- function(a, col=2, file, digits=3) {
    T1 <- a$est_f
    T2 <- a$cb_f
    ns <- c(a$n_f, NA, NA, NA, NA)
    ks <- c(a$k_f, NA, NA, NA, NA)
    ## TODO: treat n and DOF as integers
    tests <- rbind(statistic=c(a$t_f$W, a$t_f$LM, NA, NA, NA),
                   DOF=c(a$t_f[[2]], a$t_f[[5]], NA, NA, NA),
                   "p-value"=c(a$t_f[[3]], a$t_f[[6]], NA, NA, NA))

    if (col==2) {
        T1 <- cbind(T1, a$est_o)
        T2 <- cbind(T2, a$cb_o)
        tests <- cbind(tests, rbind(statistic=c(a$t_o$W, a$t_o$LM, NA, NA, NA),
                                    DOF=c(a$t_o[[2]], a$t_o[[5]], NA, NA, NA),
                                    "p-value"=c(a$t_o[[3]], a$t_o[[6]], NA,
                                                NA, NA)))
        ks <- c(ks, c(a$k_o, NA, NA, NA, NA))
        ns <- c(ns, c(a$n_o, NA, NA, NA, NA))
    }
    pop <- (seq.int(nrow(T1)/3)-1)*3+1
    odd <- (seq.int(nrow(T2)/2)-1)*2+1

    star1 <- (abs(T2[odd, ]/T2[odd+1, ]) > stats::qnorm(0.95)) +
        (abs(T2[odd, ]/T2[odd+1, ]) > stats::qnorm(0.975)) +
        (abs(T2[odd, ]/T2[odd+1, ]) > stats::qnorm(0.995))
    star1[is.na(star1)] <- 0L

    format_int <- function(x) format(x, nsmall=0, big.mark=",")
    format_re <- function(x) formatC(x, format="f", digits=digits)
    t1 <- ifelse(is.na(T1), "", format_re(T1))
    t1[pop+1, ] <- paste0("(", paste(t1[pop+1, ], sep=","), ")")
    t1[pop+2, ] <- paste0("[", paste(t1[pop+2, ], sep=","), "]")
    t1 <- ifelse(t1=="()", "", t1)
    t1 <- ifelse(t1=="[]", "", t1)
    t1[pop, ] <- ifelse(star1>2, paste0(t1[pop, ], "$^{***}$"), t1[pop, ])
    t1[pop, ] <- ifelse(star1==2, paste0(t1[pop, ], "$^{**}$"), t1[pop, ])
    t1[pop, ] <- ifelse(star1==1, paste0(t1[pop, ], "$^{*}$"), t1[pop, ])
    ks <- ifelse(is.na(ks), "", format_int(ks))
    ns <- ifelse(is.na(ns), "", format_int(ns))
    t1t <- rbind(statistic=ifelse(is.na(tests[1, ]), "",
                                  formatC(tests[1, ], format="g",
                                          digits=digits+1)),
                 "p-value"=ifelse(is.na(tests[3, ]), "", format_re(tests[3, ])),
                 DOF=ifelse(is.na(tests[2, ]), "", format_int(tests[2, ])))
    t1 <- rbind(t1, t1t, ks, ns)

    t2 <- ifelse(is.na(T2), "", format_re(T2))
    t2[odd+1, ] <- paste0("(", paste(t2[odd+1, ], sep=","), ")")
    t2 <- ifelse(t2=="()", "", t2)

    rownames(t2)[odd+1] <- ""
    rownames(t1)[c(pop+1, pop+2)] <- ""

    rownames(t1)[NROW(t1)-1] <- "Number of controls"
    rownames(t1)[NROW(t1)] <- "Sample size"

    t1 <- cbind(rownames(t1), t1)
    t2 <- cbind(rownames(t2), t2)

    t1 <- tidyr::unite(data=as.data.frame(t1), col="z", sep = " & ")
    t1 <- unlist(tidyr::unite(data=cbind(t1, c(rep("\\\\", nrow(t1)-1), "")),
                              col="z", sep=""))
    t2 <- tidyr::unite(data=as.data.frame(t2), col="z", sep = " & ")
    t2 <- unlist(tidyr::unite(data=cbind(t2, c(rep("\\\\", nrow(t2)-1), "")),
                              col="z", sep=""))
    write(unname(t1), file=paste0(file, "A.tex"))
    write(unname(t2), file=paste0(file, "B.tex"))
}
