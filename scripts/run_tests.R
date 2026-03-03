library(testthat)
library(metamet)

# test_local("metamet")

testthat::test_path()
test_local(testthat::test_path())
testthat::test_file(testthat::test_path("test_metamet_read.R"))
testthat::test_file(testthat::test_path("test_metamet_read_ebu.R"))
testthat::test_file(testthat::test_path("test_change_convention.R"))
testthat::test_file(testthat::test_path("test_restrict.R"))
testthat::test_file(testthat::test_path("test_subset.R"))

testthat::test_file(testthat::test_path("test_metamet_read_whm.R"))
testthat::test_file(testthat::test_path("test_apply_qc.R"))
testthat::test_file(testthat::test_path("test_impute.R"))
testthat::test_file(testthat::test_path("test_time_average_5mins.R"))
testthat::test_file(testthat::test_path("test_join_sites.R"))
