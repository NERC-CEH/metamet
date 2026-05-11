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
  fname_dt <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2025.csv")
  fname_qc <- pkg_extdata("UK-AMO/UK-AMO_BM_qc_2025.csv")
  fname_era5 <- pkg_extdata("dt_era5.csv")
  # half-hourly data
  mm <- metamet(
    dt = fname_dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
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

test_that("subset_by_date works on long-format single-site data", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))

  mm_s <- subset_by_date(
    mm_long,
    start_date = "2023-06-01 00:30:00",
    end_date   = "2023-06-02 00:00:00"
  )

  expect_s3_class(mm_s, "metamet")
  expect_equal(attr(mm_s, "format"), "long")
  expect_true("TIMESTAMP" %in% names(mm_s$dt))
  expect_true(all(mm_s$dt$TIMESTAMP >= as.POSIXct("2023-06-01 00:30:00")))
  expect_true(all(mm_s$dt$TIMESTAMP <= as.POSIXct("2023-06-02 00:00:00")))
  expect_null(mm_s$dt_qc)
  expect_null(mm_s$dt_ref)
})

test_that("subset_by_date works on long-format multi-site data", {
  mm <- readRDS(pkg_extdata("mm_amo_ebu_whm_2023.rds"))
  attr(mm, "format") <- "long"

  start <- as.POSIXct("2023-06-01 00:30:00")
  end   <- as.POSIXct("2023-06-02 00:00:00")

  mm_s <- subset_by_date(mm, start_date = start, end_date = end)

  expect_s3_class(mm_s, "metamet")
  expect_equal(attr(mm_s, "format"), "long")
  expect_true(nrow(mm_s$dt) > 0)
  expect_true(nrow(mm_s$dt) < nrow(mm$dt))
  expect_true(all(mm_s$dt$TIMESTAMP >= start))
  expect_true(all(mm_s$dt$TIMESTAMP <= end))
  expect_null(mm_s$dt_qc)
  expect_null(mm_s$dt_ref)
})
