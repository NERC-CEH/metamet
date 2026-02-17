test_that("subsetting by date works", {
  # mm without qc or ref
  dim(mm1$dt)
  dim(mm1$dt_qc)
  dim(mm1$dt_ref)

  mm_s1 <- subset_by_date(
    mm1,
    start_date = "2025-08-22 06:30:00",
    end_date = "2025-08-22 07:00:00"
  )

  dim(mm_s1$dt)
  dim(mm_s1$dt_qc)
  dim(mm_s1$dt_ref)

  # read a year's data with qc and ref data
  fname_dt <- testthat::test_path("data-raw/UK-AMO_BM_dt_2025.csv")
  fname_qc <- testthat::test_path("data-raw/UK-AMO_BM_qc_2025.csv")
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

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = TRUE,
    report_end_interval = TRUE,
    extra_rows = 3
  )
  time_name <- mm$dt_meta[type == "time", name_dt]

  mm_s <- subset_by_date(
    mm,
    start_date = "2025-06-01 00:30:00",
    end_date = "2025-06-02 00:00:00"
  )

  dim(mm$dt)
  dim(mm$dt_qc)
  dim(mm$dt_ref)

  dim(mm_s$dt)
  dim(mm_s$dt_qc)
  dim(mm_s$dt_ref)

  mm$dt[1:5, 1:5]
  mm$dt_qc[1:5, 1:5]
  mm$dt_ref[1:5, 1:5]
  mm$dt[(.N - 3):.N, 1:5]
  mm$dt_ref[(.N - 3):.N, 1:5]

  expect_equal(nrow(mm_s1$dt), 31)
  expect_null(mm_s1$dt_qc)
  expect_null(mm_s1$dt_ref)

  expect_s3_class(mm_s, "metamet")
  expect_s3_class(mm_s$dt_qc, "data.table")
  expect_equal(nrow(mm_s$dt), 48)
  expect_equal(nrow(mm_s$dt_qc), 48)
  expect_equal(nrow(mm_s$dt_ref), 48)

  # should not be any duplicate times
  expect_equal(nrow(mm_s$dt[duplicated(mm_s$dt[, ..time_name]), ]), 0)
})
