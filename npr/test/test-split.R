library(tinytest)
## vignette("using_tinytest", package="tinytest")

## Valide codes S00 - T78
data1 <- readRDS("~/Git-fhi/analysis/npr/test-data/split.RDS")
hovedOut <- readRDS("~/Git-fhi/analysis/npr/test-data/split-hoved.RDS")
bidiagOut <- readRDS("~/Git-fhi/analysis/npr/test-data/split-bidiag.RDS")

expect_equal(get_valid_codes(data1, "hoveddiagnoser", "hovdiag"), hovedOut)
expect_equal(get_valid_codes(data1, "bidiagnoser", "bidiag", split = " "), bidiagOut)
