# Count days from previous to the following injury when codes are similar
dt.check <- readRDS("~/Git-fhi/analysis/npr/test-data/check-code-dt.RDS")
out.check <- readRDS("~/Git-fhi/analysis/npr/test-data/check-code-out.RDS")

expect_equal(check_codes(dt.check, "lopenr", "hoveddiagnoser", 3), out.check)

# Injuries with possible duplicate within 3 days
epiDT <- readRDS("~/Git-fhi/analysis/npr/test-data/days-injury-dt.RDS")
epiOut <- readRDS("~/Git-fhi/analysis/npr/test-data/days-injury-out.RDS")

expect_equal(find_episode(d=epiDT, acute = TRUE, days = 3), epiOut)
