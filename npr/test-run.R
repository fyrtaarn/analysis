library(tinytest)

root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))

run_test_dir(file.path(root, "test"))

run_test_file(file.path(root, "test/test-split.R"))
