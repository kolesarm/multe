test_that("Test Fryer and Levitt", {

    ## Base controls only
    r1 <- stats::lm(std_iq_24~race+factor(age_24)+female, weight=W2C0, data=fl)
    m1 <- multe(r1, "race", cluster=NULL)

    test_stata <- function(x, est, se, chi, cluster=FALSE) {
        rd <- function(a, b) max(abs((a-b)/a))
        estidx <- 0:((NROW(x$est_f)/3-1))*3+1
        ## Convert to HC1 errors
        if (cluster)
            HC1 <- sqrt((x$n_f-1)/(x$n_f-x$k_f-1))
        else
            HC1 <- sqrt(x$n_f/(x$n_f-x$k_f-1))
        testthat::expect_identical(unname(x$t_f[[2]]), chi[[2]])
        c(rd(x$est_f[estidx, 1], est), rd(HC1*x$est_f[estidx+1, 1], se),
          rd(x$t_f$W, chi[[1]]), abs(x$t_f[[3]]- chi[[3]]))
    }

    ## Test we match stata: 1 non-strata control, overlap
    t1 <- test_stata(m1, -c(0.382148509, 0.431527804, 0.215241637, 0.236742438),
                     c(0.027199988, 0.024173529, 0.035306552, 0.038614609),
                     list(44.380661, 16L, 0.00017), cluster=TRUE)
    testthat::expect_identical(t1 <= c(1e-8, 4e-4, 2e-4, 5e-5),
                               rep(TRUE, 4))
    ## TODO: Check ATE one at a time. Clustering, not weighted, no controls, no overlap
})
