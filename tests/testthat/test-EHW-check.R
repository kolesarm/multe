test_that("Test Fryer and Levitt", {

    ## Table 3 Col 4
    rr <- lm(std_iq_24~race+factor(age_24)+female+SES_quintile+
                 factor(siblings)+family_structure+
                 region + I(poly(mom_age, 5))+mom_age_NA+
                 I(poly(parent_score, 5))+parent_score_NA+
                 birthweight+ days_premature + multiple_birth +
                 factor(interviewer_ID_24),
             weight=W2C0, data=fl)
    rd <- function(a, b) max(abs((a-b)/a))
    testthat::expect_equal(nobs(rr), 8806L)
    testthat::expect_lt(rd(rr$coefficients[c(2:9)],
                           c(-0.213161196, -0.248588843, -0.294283692,
                             -0.131691091, 0.164976004, 0.246441805,
                             0.501252334, 0.247066373)), 1e-5)

    ## 2. Decomposition
    ## m1 <- multe(rr, "race", cluster=NULL)

    ## write_rd_table(a3, col=2, file="output/fryer_levitt", digits=3)

    ## ## 3. Test we match stata
    ## testthat::expect_lt(rd(a3$a1[c(1, 4, 7, 10), 1],
    ##                        -c(0.213161708, 0.248588638, 0.294283378,
    ## 0.131691169)),
    ##                     11e-8)
    ## ## Convert to HC1 errors
    ## testthat::expect_lt(rd(sqrt(a3$n1/(a3$n1-a3$k1-1))*
    ##                        a3$a1[c(2, 5, 8, 11), 1],b
    ##                        c(0.032049388, 0.028500968,
    ##                          0.035581831, 0.038432955)),
    ##                     3e-4)


})
