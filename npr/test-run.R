library(tinytest)

root <- "~/Git-fhi/analysis/npr"
fx <- list.files(file.path(root, "functions"))
for (i in fx)
  source(file.path(root, "functions", i))

# Test all files
run_test_dir(file.path(root, "test"))

# Test specific files
run_test_file(file.path(root, "test/test-split.R"))
run_test_file(file.path(root, "test/test-prosent-frequency.R"))
run_test_file(file.path(root, "test/test-check-codes.R"))
run_test_file(file.path(root, "test/test-case.R"))
