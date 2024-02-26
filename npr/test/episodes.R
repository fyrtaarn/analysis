
epiDT <- readRDS("~/Git-fhi/analysis/npr/test-data/days-injury-dt.RDS")
epiOut <- readRDS("~/Git-fhi/analysis/npr/test-data/days-injury-out.RDS")

expect_equal(find_episode(epiDT, 2022, acute = TRUE, days = 3), epiOut)
