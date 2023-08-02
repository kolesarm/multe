## The original dta file, has been downloaded from
## https://doi.org/10.3886/E112609V1 Then run fryer_levitt.do

## 1. Read data,  Convert 0/1 to logical
fl <- readstata13::read.dta13("mental.dta", generate.factors=TRUE)
attr(fl, "expansion.fields") <- NULL
attr(fl, "datalabel") <- NULL
attr(fl, "formats") <- NULL
attr(fl, "types") <- NULL
attr(fl, "val.labels") <- NULL
attr(fl, "label.table") <- NULL
attr(fl, "orig.dim") <- NULL

for (j in seq_len(ncol(fl))) {
    if (identical(sort(unique(fl[, j])), c(0L, 1L)))
        fl[, j] <- as.logical(fl[, j])
}

usethis::use_data(fl, overwrite=TRUE, internal=FALSE)
