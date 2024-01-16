## ----include=FALSE, cache=FALSE-----------------------------------------------
library("knitr")
knitr::opts_knit$set(self.contained = FALSE)
knitr::opts_chunk$set(tidy = TRUE, collapse=TRUE, comment = "#>",
                      tidy.opts=list(blank=FALSE, width.cutoff=55))

## ----setup--------------------------------------------------------------------
library("multe")
## Regression of IQ at 24 months on race indicators and baseline controls
r1 <- stats::lm(std_iq_24~race+factor(age_24)+female+SES_quintile,
                weight=W2C0, data=fl)
## Compute alternatives estimates free of contamination bias
m1 <- multe(r1, "race", cluster=NULL)
print(m1, digits=3)

## ----r2-----------------------------------------------------------------------
r2 <- stats::lm(std_iq_24~race+factor(age_24)+female+SES_quintile+
                    factor(siblings)+family_structure, weight=W2C0, data=fl)
m2 <- multe(r2, treatment="race")
print(m2, digits=3)

## ----r3-----------------------------------------------------------------------
table(fl$race[fl$siblings==6])

## ----r4-----------------------------------------------------------------------
print(m2$cb_f, digits=3)
print(m2$cb_o, digits=3)

## ----r5-----------------------------------------------------------------------
print(m1$est_f, digits=3)

## ----r6-----------------------------------------------------------------------
## cluster in interviewer ID
m1alt <- multe(r1, "race", cluster=factor(factor(fl$interviewer_ID_24)))
print(m1alt, digits=3)

