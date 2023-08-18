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
