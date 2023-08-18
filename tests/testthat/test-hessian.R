test_that("Test Hessian calculations", {
    r1 <- nnet::multinom(race~factor(age_24)+female, data=fl, trace=FALSE)
    expect_lt(max(abs(multHessian(r1)-nnet:::multinomHess(r1))), 1e-8)

    K <- 3
    D <- factor(c(rep(c(2, 1, 1, 0, 0, 0), K), rep(c(2, 0, 0, 0, 0, 1), K)))
    S <- factor(rep(1:(length(D)/4), each=4))
    r2 <- nnet::multinom(D~S, weights=seq_along(S), trace=FALSE)
    r3 <- nnet::multinom(race~factor(age_24)+female, weight=W2C0, data=fl,
                         trace=FALSE)
    expect_lt(max(abs(multHessian(r2)-nnet:::multinomHess(r2))), 1e-8)
    expect_lt(max(abs(multHessian(r3)-nnet:::multinomHess(r3))), 1e-8)
    r4 <- nnet::multinom(I(race=="White")~factor(age_24)+female, weight=W2C0,
                         data=fl, trace=FALSE)
    expect_lt(max(abs(multHessian(r4)-nnet:::multinomHess(r4))), 1e-8)
})


## test_that("Test LM can be big", {
##     D <- fl$race[fl$interviewer_ID_24==1372]
##     X <- outer(D, levels(D)[-1], "==")
##     pi0 <- as.numeric(table(fl$race)/length(fl$race))
##     Sc <- X-outer(rep(1, NROW(X)), pi0[-1])
##     hatpi <- as.numeric(table(D)/length(D))

##     n <- length(D)
##     tpi <- (hatpi-pi0)[-1]
##     V0 <- n*(diag(hatpi[-1])-outer(hatpi[-1], hatpi[-1])+outer(tpi, tpi))
##     G <- sum(tpi*solve(diag(hatpi[-1])-outer(hatpi[-1], hatpi[-1]), tpi))
##     n*G/(G+1)
##     n^2*sum(tpi*solve(V0, tpi))

##     pis <- t(table(fl$race, fl$interviewer_ID_24))
##     ns <- rowSums(pis)
##     pi0 <- as.numeric(table(fl$race)/length(fl$race))
##     hatpi <- t((1/ns)*pis)
##     cs <- colSums((hatpi-pi0)^2/hatpi)
##     cs <- ns*cs/(1+cs)
##     sum(cs[!is.na(cs)])

## })

## set.seed(42)
## X <- matrix(rnorm(80), ncol=4)
## A <- X[, sample(1:4, size=8, replace=TRUE)]
## b <- rnorm(NROW(A))


## qra <- qr(A)
## idx <- qra$pivot[seq.int(qra$rank)]



## ## SOLVE A^{-}b
## R11 <- qr.R(qra)[seq.int(qra$rank), seq.int(qra$rank)]
## sol <- c(drop(backsolve(R11, crossprod(Q1, b))),
##          rep(NA, length(qra$pivot)-qra$rank))
## sol[qra$pivot] <- sol


## qr.coef(qr, y) ## fitted coeffs, R^{-1}
## qr.qy(qr, y) ## Qy
## qr.qty(qr, y) ## Q'y
## qr.resid(qr, y)
## qr.fitted(qr, y, k = qr$rank)
## qr.solve(a, b, tol = 1e-7)






## qHe <- qr(He)
## a <- t(crossprod((M-M0)[idx, ], qr.solve(He[idx, idx], t(Sc[, idx]),
