test_stata <- function(x, est, se, chi, cluster=FALSE, estATE=NULL,
                       seATE=NULL, estEW=NULL, seEW=NULL, ns=NULL) {
    rd <- function(a, b) max(abs((a-b)/a))
    estidx <- (0:(NROW(x$est_f)/3-1))*3+1
    ## Convert to HC1 errors
    K <- NROW(x$est_f)/3
    p <-   x$k_f + 1 + K
    pa <-  x$k_f * (K+1) + K+1 # p for ATE
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
    c(beta=rd(x$est_f[estidx, 1], est),
      beta.se=rd(HC1*x$est_f[estidx+1, 1], se),
      W=rd(x$t_f$W, chi[[1]]),
      pW=abs(x$t_f[[3]]- chi[[3]]),
      ATE=ATE,
      ATE.se=seATE, EW=EW, EW.se=seEW)
}


test_that("Test Fryer and Levitt", {

    ## Base controls only
    r1 <- stats::lm(std_iq_24~race+factor(age_24)+female, weight=W2C0, data=fl)
    m1 <- multe(r1, "race", cluster=NULL)
    out1 <- capture.output(print(m1, digits=4))
    testthat::expect_equal(out1[16], "Full sample: 0.0434")

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
    testthat::expect_identical(sum(t1 <= c(1e-10, 1e-9, 2e-4, 1e-5, 1e-8,
                                           1e-8, 1e-10, 1e-10)), 8L)
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
    testthat::expect_identical(sum(t2 <= c(1e-9, 1e-9, 2e-4, 1e-5, 1e-7, 1e-9,
                                           1e-10, 1e-9)), 8L)

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
    a3 <- -c(0.33471482887, 0.37256507935, 0.08299865304, 0.19372967175)
    b3 <- c(0.03738957252, 0.03527493811, 0.04533840296, 0.03025915812)
    o3 <- -c(0.33240359957, 0.37355435179, 0.11988633341, 0.17712550552)
    p3 <- c(0.03685028050, 0.03512567769, 0.04576031374, 0.03003805330)
    ns <- table(fl$race[fl$days_premature != "56"])
    ns <- ns[-1]+ns[1]
    t3 <- test_stata(m3, e3, s3, list(431.75865, 44L, 3.973e-65), estATE=a3,
                     seATE=b3, estEW=o3, seEW=p3, ns=ns, cluster=TRUE)
    testthat::expect_identical(sum(t3 <= c(1e-9, 1e-9, 1e-5, 1e-10,
                                           1e-7, 1e-8, 1e-10, 1e-9)), 8L)
    ## Single continuous control
    r4 <- stats::lm(std_iq_24~race+parent_score, data=fl)
    testthat::expect_silent(m4 <- multe(r4, "race"))
    expect_identical(m4$est_o, NULL)
})

test_that("No controls", {
    r1 <- stats::lm(std_iq_24~race, weight=W2C0, data=fl)
    expect_error(m1 <- multe(r1, "race"),
                 "There are no controls beyond the intercept")
})

test_that("Test binomial treatment", {
    r1 <- stats::lm(std_iq_24~factor(I(race=="White")) + factor(age_24)+female,
                    weight=W2C0, data=fl)
    m1 <- multe(r1, treatment_name="factor(I(race == \"White\"))", cluster=NULL)

    expect_equal(unname(m1$cb_f[, 2]), c(0, 0))

    t1 <- test_stata(m1, est=0.38495380524, se=0.01955413026,
                     chi=list(36.994275, 4L, 1.806e-07), cluster=FALSE,
                     estATE=0.38504401394, seATE=0.01954185224,
                     estEW=0.38495380524, seEW=0.01955413026, ns=nobs(r1))
    testthat::expect_identical(sum(t1 <= c(1e-10, 1e-9, 2e-4, 1e-9, 1e-9, 1e-8,
                                           1e-10, 1e-9)), 8L)
    expect_lt(max(abs(m1$est_f[, 4]-m1$est_f[, 5])), 1e-6)
})

test_that("Simple examples", {
    ## Lack of overlap, Example 4 in notes
    W <- c(rep(0, 12), rep(1, 12))
    D <- c(rep(0:2, 4), rep(1:2, 6))
    Y <- (D==2) + D*W
    d1 <- decomposition(Y, as.factor(D), cbind(1, W))
    testthat::expect_identical(sum(is.na(d1$A[, 2:3])), 12L)
    ## TODO: LM and Wald
    s <- 10/9
    beta <- c(((s-1/3)*0+1/3*1-2/3)/s, ((s-1/3)*1+1/3*2)/s)
    testthat::expect_equal(unname(d1$A[c(1, 4), 1]), beta)

    ## Lack of overlap, Example 5, also in the slides
    W <- c(rep(0, 12), rep(1, 12))
    D <- c(rep(0:2, 4), rep(0:1, 6))
    Y <- (D==2) + D*W
    d2 <- decomposition(Y, as.factor(D), cbind(1, W))
    ra <- range(cbind(d2$A[-c(3, 6), 1:2], d2$B[, c(2, 4)])-
                    rbind(c(6/10, 6/10, 0, 0),
                          c(0.1095445115, 1.095445115e-01, 0, 0),
                          c(13L/10L, 1L, 3/10, 3/10),
                          c(0.1193733639, 0, 1.193733639e-01, 1.193733639e-01)))
    testthat::expect_lt(diff(ra), 1e-10)

    ## Simple LM test
    gr <- expand.grid(S=c(6, 10), p=c(2, 3, 10))
    for (j in seq_len(NROW(gr))) {
        D <- rep(c(rep(0:1, gr$S[j]/2), rep(0, gr$S[j])), gr$p[j])
        W <- factor(rep(1:(2*gr$p[j]), each=gr$S[j]))
        r1 <- lm(0*D~factor(D)+W)
        msg <- paste0("For variable W the following levels fail overlap:\n",
                      paste(2 * (1:gr$p[j]), collapse=", "))
        expect_message(m1 <- multe(r1, treatment="factor(D)"),
                       msg)
        expect_lt(m1$t_f[[1]], 1e-6)
        expect_equal(unname(unlist(m1$t_f))[c(2, 4:6)],
                     c(gr$p[j]-1, 2*gr$p[j]*gr$S[j]/3, 2*gr$p[j]-1,
                       1-pchisq(2*gr$p[j]*gr$S[j]/3, df=2*gr$p[j]-1)))
    }

    ## Simplest drop
    r3 <- lm(std_iq_24~race+ I(poly(mom_age, 5))+mom_age_NA, data=fl)
    expect_message(m3 <- multe(r3, "race"),
                   "variation and are dropped:\nmom_age_NATRUE")
    expect_equal(m3$t_f$LM, 807.346338688)

    ## Simplest LM/Wald instability, not weighting solves it
    r4 <- lm(std_iq_24~race+ region + I(poly(mom_age, 5))+mom_age_NA,
             weight=W2C0, data=fl)
    expect_warning(m4 <- decomposition(stats::model.response(r4$model),
                                       r4$model[, "race"],
                                       stats::model.matrix(r4)[, -c(2:5)],
                                       stats::model.weights(r4$model)),
                   "statistic is: 979.46, with df: 35")

    ## Adding observations with zero weights should make no difference
    fl1 <- rbind(fl, fl)
    fl1$W2C0[(NROW(fl)+1):(2*NROW(fl))] <- 0
    r5 <- lm(std_iq_24~race+ region + I(poly(mom_age, 5))+mom_age_NA,
             weight=W2C0, data=fl1)
    expect_warning(m5 <- multe(r5, "race"),
                   "statistic is: 979.46, with df: 35")
    dd <- m5$est_f-m4$A
    expect_lt(max(abs(dd[!is.na(dd)])), 1e-12)
    dd <- m5$cb_f-m4$B
    expect_lt(max(abs(dd[!is.na(dd)])), 1e-12)
    m0 <- capture.output(print(m5, digits=4))
    testthat::expect_equal(m0[[27]],
                           "Full sample: 0.1424, Overlap sample: 0.1422")

    expect_equal(m0[[3]],
                 "Black    -0.34365 -0.34146 -0.34207 -0.34585 -0.35662")
    expect_equal(m0[[21]],
                 "SE        0.04100  0.04093  0.04296  0.04114  0.04337")

})


test_that("Input checks", {
    wbh <- fl[fl$race=="White" | fl$race=="Black" | fl$race=="Hispanic", ]
    wbh <- droplevels(wbh)
    r1 <- stats::lm(std_iq_24~race+factor(age_24)+female, weight=W2C0, data=wbh)
    testthat::expect_error(multe(r1, treatment="race",
                                 cluster=wbh$interviewer_ID_24))

})

## TODO: Test LM, own, and CW.
