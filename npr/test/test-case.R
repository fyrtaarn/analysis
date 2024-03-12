
dt.case <- readRDS("~/Git-fhi/analysis/npr/test-data/dt-case.RDS")
dt.somatik <- readRDS("~/Git-fhi/analysis/npr/test-data/dt-somatikk.RDS")
out.dup.rhf <- readRDS("~/Git-fhi/analysis/npr/test-data/dt-dup-rhf-out.RDS")
out.rhf.default <- readRDS("~/Git-fhi/analysis/npr/test-data/out-rhf-default.RDS")
somDX <- is_dup_rhf(data.table::copy(dt.case), "lopenr", "skadeDato", "helseforetak_nr")
out.case <- readRDS("~/Git-fhi/analysis/npr/test-data/out-find-case.RDS")

expect_equal(is_dup_rhf(data.table::copy(dt.case), "lopenr", "skadeDato", "helseforetak_nr"), out.dup.rhf)
expect_equal(is_rhf(d1 = data.table::copy(somDX),
                    d2 = dt.somatik,
                    id = "lopenr",
                    skade = "skadeDato",
                    rhf = "helseforetak_nr"), out.rhf.default)
expect_equal(find_case(data.table::copy(dt.case), data.table::copy(dt.somatik)), out.case)
