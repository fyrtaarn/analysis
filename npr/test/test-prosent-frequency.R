
## Frequency and percent
kb <- readRDS("~/Git-fhi/analysis/npr/test-data/kodebok.RDS")
skadeDT <- readRDS("~/Git-fhi/analysis/npr/test-data/show-pro-dt.RDS")
skadeMek <- readRDS("~/Git-fhi/analysis/npr/test-data/skademek-pro-out.RDS")
skadeAge <- readRDS("~/Git-fhi/analysis/npr/test-data/skadegrp-pro-out.RDS")
kjonnPRO <- readRDS("~/Git-fhi/analysis/npr/test-data/kjonn-pro.RDS")

expect_equal(show_pro(data.table::copy(skadeDT), "skadeMekanisme", kb), skadeMek)
expect_equal(show_pro(data.table::copy(skadeDT), "GRP"), skadeAge)
expect_equal(show_pro(data.table::copy(skadeDT), "kjonn", kb), kjonnPRO)


## Age grouping
ageGRP <- readRDS("~/Git-fhi/analysis/npr/test-data/age-grp.RDS")
ageGRPnull <- readRDS("~/Git-fhi/analysis/npr/test-data/age-grp-null.RDS")

expect_equal(do_agegroup(skadeDT, "age", c(0, 18, 25, 45, 65, Inf), new = "Group"), ageGRP)
expect_equal(do_agegroup(skadeDT, "age", c(0, 18, 25, 45, 65, Inf)), ageGRPnull)
