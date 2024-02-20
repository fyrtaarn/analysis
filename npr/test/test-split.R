library(tinytest)
## vignette("using_tinytest", package="tinytest")

## Valide codes S00 - T78
data1 <- readRDS("~/Git-fhi/analysis/npr/test-data/split.RDS")

expect_equal(get_valid_codes(data.table::copy(data1), "hoveddiagnoser", "hovdiag"),
             readRDS("~/Git-fhi/analysis/npr/test-data/split-hoved.RDS"))

expect_equal(get_valid_codes(data.table::copy(data1), "bidiagnoser", "bidiag", sep = " "),
             readRDS("~/Git-fhi/analysis/npr/test-data/split-bidiag.RDS"))
