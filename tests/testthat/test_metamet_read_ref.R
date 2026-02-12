# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("reading metamet from files with QC works", {
  fname_dt <- testthat::test_path("data-raw/UK-AMO_BM_dt_2026.csv")
  fname_qc <- testthat::test_path("data-raw/UK-AMO_BM_qc_2026.csv")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  fname_era5 <- testthat::test_path("data-raw/dt_era5.csv")
  # half-hourly data
  mm <- metamet(
    dt = fname_dt,
    dt_meta = fname_meta,
    dt_site = fname_site,
    dt_qc = fname_qc,
    site_id = "UK-AMO"
  )

  mm$dt_meta$standard_name_era5
  time_name <- mm$dt_meta[type == "time", name_dt]

  mm2 <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_era5_to_obs = TRUE,
    restrict_obs_to_era5 = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  dim(mm2$dt)
  dim(mm2$dt_era5)
  mm$dt[1:5, 1:5]
  mm2$dt_era5[1:5, 1:5]
  mm$dt[(.N - 3):.N, 1:5]
  mm2$dt_era5[(.N - 3):.N, 1:5]
  # does not seem to work
  # data.table::setnafill(mm2$dt_era5[, -c("DATECT", "site")], type = "nocb")
  # mm2$dt_era5[1, -c("DATECT", "site")]  <- mm2$dt_era5[first_nona, -c("DATECT", "site")]

  expect_s3_class(mm, "metamet")
  expect_s3_class(mm$dt_qc, "data.table")
  expect_equal(sum(is.na(mm$dt_qc[, ..time_name])), 0)
  # qc loses the extra validator column when averaged, so should be same
  expect_equal(ncol(mm$dt), ncol(mm$dt_qc) - 1)
  # should not be any duplicate times
  expect_equal(nrow(mm$dt[duplicated(mm$dt[, ..time_name]), ]), 0)
})
