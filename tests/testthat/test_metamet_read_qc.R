# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("reading metamet from files with QC works", {
  fname_dt1 <- testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_dt_2025.csv")
  fname_qc1 <- testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_qc_2025.csv")
  fname_dt2 <- testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_dt_2026.csv")
  fname_qc2 <- testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_qc_2026.csv")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  # half-hourly data
  mm_hh_1 <- metamet(
    dt = fname_dt1,
    dt_meta = fname_meta,
    dt_site = fname_site,
    dt_qc = fname_qc1,
    site_id = "UK-AMO"
  )
  mm_hh_2 <- metamet(
    dt = fname_dt2,
    dt_meta = fname_meta,
    dt_site = fname_site,
    dt_qc = fname_qc2,
    site_id = "UK-AMO"
  )

  mm1 <- time_average(mm_hh_1, avg.time = "1 day", extra_rows = 1)
  mm2 <- time_average(mm_hh_2, avg.time = "1 day", extra_rows = 1)

  mm_joined <- join(mm1, mm2)

  # dim(mm1$dt)
  # dim(mm2$dt)
  # mm_hh_1$dt[1:5, 1:5]
  # mm_hh_2$dt[1:5, 1:5]
  # mm1$dt[1:5, 1:5]
  # mm2$dt[1:5, 1:5]
  # dim(mm_joined$dt)

  time_name <- mm_joined$dt_meta[type == "time", name_dt]

  expect_s3_class(mm_joined, "metamet")
  expect_s3_class(mm_joined$dt_qc, "data.table")
  expect_equal(sum(is.na(mm_joined$dt_qc[, ..time_name])), 0)
  expect_equal(nrow(mm1$dt) + nrow(mm2$dt), nrow(mm_joined$dt_qc))
  # qc loses the extra validator column when averaged, so should be same
  expect_equal(ncol(mm_joined$dt), ncol(mm_joined$dt_qc) - 1)
  # should not be any duplicate times
  expect_equal(nrow(mm_joined$dt[duplicated(mm_joined$dt[, ..time_name]), ]), 0)
  # do we see the same precip qc codes in the original and joined data?
  expect_setequal(
    unique(c(mm_hh_1$dt_qc$P_12_1_1, mm_hh_2$dt_qc$P_12_1_1)),
    unique(mm_joined$dt_qc$P_12_1_1)
  )
})
