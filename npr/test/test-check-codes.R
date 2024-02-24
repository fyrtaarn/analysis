
dt.check <- readRDS("~/Git-fhi/analysis/npr/test-data/check-code-dt.RDS")
out.check <- readRDS("~/Git-fhi/analysis/npr/test-data/check-code-out.RDS")

expect_equal(check_codes(dt.check, "lopenr", "hoveddiagnoser", 3), out.check)
