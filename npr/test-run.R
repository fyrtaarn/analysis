library(tinytest)

root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))

run_test_file(file.path(root, "test.R"))
