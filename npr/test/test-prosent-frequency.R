
## Frequency and percent
kb <- readRDS("~/Git-fhi/analysis/npr/test-data/kodebok.RDS")
skadeDT <- readRDS("~/Git-fhi/analysis/npr/test-data/show-pro-dt.RDS")
skadeMek <- readRDS("~/Git-fhi/analysis/npr/test-data/skademek-pro-out.RDS")
ageGRP <- readRDS("~/Git-fhi/analysis/npr/test-data/skadegrp-pro-out.RDS")
kjonnPRO <- readRDS("~/Git-fhi/analysis/npr/test-data/kjonn-pro.RDS")

expect_equal(show_pro(data.table::copy(skadeDT), "skadeMekanisme", kb), skadeMek)
expect_equal(show_pro(data.table::copy(skadeDT), "GRP"), ageGRP)
expect_equal(show_pro(data.table::copy(skadeDT), "kjonn", kb), kjonnPRO)
