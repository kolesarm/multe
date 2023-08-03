test_that("Test Fryer and Levitt", {

    ## Base controls only
    r1 <- stats::lm(std_iq_24~race+factor(age_24)+female, weight=W2C0, data=fl)
    m1 <- multe(r1, "race", cluster=NULL)

    test_stata <- function(x, est, se, chi, cluster=FALSE, estATE=NULL,
                           seATE=NULL, estEW=NULL, seEW=NULL, ns=NULL) {
        rd <- function(a, b) max(abs((a-b)/a))
        estidx <- (0:(NROW(x$est_f)/3-1))*3+1
        ## Convert to HC1 errors
        K <- NROW(x$est_f)/3
        p <- x$k_f + 1 + K
        pa <- (x$k_f + 1)*K + K # p for ATE
        pe <- x$k_f + 2 # for EW
        if (cluster) {
            HC1 <- sqrt((x$n_f-1)/ (x$n_f-p))
            AC1 <- sqrt((x$n_f-1)/ (x$n_f-pa))
            EC1 <- sqrt((ns-1)/ (ns-pe))
        } else {
            HC1 <- sqrt(x$n_f/ (x$n_f-p))
            AC1 <- sqrt(x$n_f/ (x$n_f-pa))
            EC1 <- sqrt(ns/ (ns-pe))
        }
        ATE <- ifelse(!is.null(estATE), rd(x$est_f[estidx, 3], estATE), NA)
        EW <- ifelse(!is.null(estEW), rd(x$est_f[estidx, 4], estEW), NA)
        seATE <- ifelse(!is.null(seATE), rd(AC1*x$est_f[estidx+2, 3], seATE),
                        NA)
        seEW <- ifelse(!is.null(seEW), rd(EC1*x$est_f[estidx+1, 4], seEW), NA)

        testthat::expect_identical(unname(x$t_f[[2]]), chi[[2]])
        c(rd(x$est_f[estidx, 1], est), rd(HC1*x$est_f[estidx+1, 1], se),
          rd(x$t_f$W, chi[[1]]), abs(x$t_f[[3]]- chi[[3]]), ATE, seATE, EW,
          seEW)
    }

    ## Test we match stata: 1 non-strata control, overlap
    e1 <- -c(0.38214850861, 0.43152780424, 0.21524163702, 0.23674243795)
    a1 <- -c(0.38224046376, 0.43220093709, 0.21494788553, 0.23918436629)
    s1 <- c(0.02719998765, 0.02417352945, 0.03530655165, 0.03861460940)
    b1 <- c(0.02711806252, 0.02410508539, 0.03521886966, 0.03811401253)
    o1 <- -c(0.38148645623, 0.43108713219, 0.21425083309, 0.23674282596)
    p1 <- c(0.02723553124, 0.02420140436, 0.03532753684, 0.03846944309)
    ns <- table(r1$model$"race")[-1]+table(r1$model$"race")[1]
    t1 <- test_stata(m1, e1, s1, list(44.380661, 16L, 0.00017), estATE=a1,
                     seATE=b1, estEW=o1, seEW=p1, ns=ns)
    testthat::expect_identical(t1 <= c(1e-10, 1e-9, 2e-4, 1e-5, 1e-8,
                                       1e-4, 1e-10, 1e-10),
                               rep(TRUE, 8))
    ## Alternative:
    ## l1 <- lmtest::coeftest(r1, vcov = sandwich::vcovHC(r1, type = "HC0"))
    ## print(l1[2:5, 2], digits=11)

    ## Unweighted
    r2 <- stats::lm(std_iq_24~race+factor(age_24)+female, data=fl)
    m2 <- multe(r2, "race", cluster=NULL)
    e2 <- -c(0.40307948873, 0.37190206761, 0.02956472195, 0.14465188134)
    a2 <- -c(0.40288196461, 0.37246500815, 0.03029817036, 0.14366763851)
    s2 <- c(0.02255093624, 0.01986720365, 0.02849451882, 0.02519070865)
    b2 <- c(0.02261904824, 0.01988929344, 0.02850617942, 0.02521682508)
    o2 <- -c(0.40219798717, 0.37128881274, 0.02883618306, 0.14445474649)
    p2 <- c(0.02260901078, 0.01990755363, 0.02852013390, 0.02519154792)

    t2 <- test_stata(m2, e2, s2, list(55.247999, 16L, 3.237e-06),
                     estATE=a2, seATE=b2, seEW=p2, estEW=o2, ns=ns)
    testthat::expect_identical(t2 <= c(1e-9, 1e-9, 2e-4, 1e-5, 1e-7, 1e-4,
                                       1e-10, 1e-9),
                               rep(TRUE, 8))

    ## Only strata controls, overlap sample
    r3 <- stats::lm(std_iq_24~race+days_premature, data=fl)
    testthat::expect_message(m3 <- multe(r3, "race",
                                         cluster=factor(fl$interviewer_ID_24)))
    m3$est_f <- m3$est_o
    m3$n_f <- m3$n_o
    m3$k_f <- m3$k_o
    m3$t_f <- m3$t_o
    e3 <- -c(0.33333797225, 0.37248246561, 0.11613548296, 0.17710303864)
    s3 <- c(0.03687299020, 0.03515234694, 0.04527212475, 0.03041754158)
    ## a2 <- -c(0.40288196461, 0.37246500815, 0.03029817036, 0.14366763851)
    ## b2 <- c(0.02261904824, 0.01988929344, 0.02850617942, 0.02521682508)
    ## o2 <- -c(0.40219798717, 0.37128881274, 0.02883618306, 0.14445474649)
    ## p2 <- c(0.02260901078, 0.01990755363, 0.02852013390, 0.02519154792)
    t3 <- test_stata(m3, e3, s3, list(431.75865, 44L, 3.973e-65), cluster=TRUE)
    testthat::expect_identical(t3[1:4] <= c(1e-9, 1e-9, 1e-5, 1e-10),
                               rep(TRUE, 4))

    ## TODO: printCoefmat
})
