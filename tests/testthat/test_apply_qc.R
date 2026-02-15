# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("applying QC works", {
  fname_dt <- testthat::test_path("data-raw/UK-AMO_BM_dt_2026.csv")
  fname_qc <- testthat::test_path("data-raw/UK-AMO_BM_qc_2026.csv")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  fname_era5 <- testthat::test_path("data-raw/dt_era5.csv")
  # half-hourly data
  mm0 <- metamet(
    dt = fname_dt,
    dt_meta = fname_meta,
    dt_site = fname_site,
    dt_qc = fname_qc,
    site_id = "UK-AMO"
  )

  mm0 <- add_era5(
    mm0,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  mm0_after <- apply_qc(mm0)

  # try on the built-in mm objects
  mm1_after <- apply_qc(mm1)
  mm2_after <- apply_qc(mm2)
  mm3_after <- apply_qc(mm3)

  # and a repeat
  mm2_after_two <- apply_qc(mm2_after)
  time_name <- mm2$dt_meta[type == "time", name_dt]

  # there will be more NAs after QC check - unless a very small or perfect data
  expect_gt(sum(is.na(mm0_after$dt)), sum(is.na(mm0$dt)))
  expect_gt(sum(is.na(mm1_after$dt)), sum(is.na(mm1$dt)))
  expect_gt(sum(is.na(mm2_after$dt)), sum(is.na(mm2$dt)))
  # this a very small and perfect data:
  expect_gte(sum(is.na(mm3_after$dt)), sum(is.na(mm3$dt)))
  # apply QC again and there should be no change
  expect_identical(sum(is.na(mm2_after$dt)), sum(is.na(mm2_after_two$dt)))
  # should not be any duplicate times
  expect_equal(nrow(mm2_after$dt[duplicated(mm2_after$dt[, ..time_name]), ]), 0)
  # qc codes for "missing" should match the missing obs
  expect_identical(sum(is.na(mm0$dt)), sum(mm0$dt_qc == 1))
  expect_identical(sum(is.na(mm1$dt)), sum(mm1$dt_qc == 1))
  expect_identical(sum(is.na(mm2$dt)), sum(mm2$dt_qc == 1))
  expect_identical(sum(is.na(mm3$dt)), sum(mm3$dt_qc == 1))
})
