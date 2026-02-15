# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("applying imputing works for more files", {
  fname_dt_32 <- testthat::test_path("data-raw/UK-AMo_BM_20250822_L03_F02.dat")
  fname_dt_41 <- testthat::test_path("data-raw/UK-AMo_BM_20250822_L04_F01.dat")
  fname_dt_42 <- testthat::test_path("data-raw/UK-AMo_BM_20250822_L04_F02.dat")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  fname_era5 <- testthat::test_path("data-raw/dt_era5.csv")
  # half-hourly data
  mm_32 <- metamet(
    dt = fname_dt_32,
    dt_meta = fname_meta,
    dt_site = fname_site,
    site_id = "UK-AMO"
  )
  mm_41 <- metamet(
    dt = fname_dt_41,
    dt_meta = fname_meta,
    dt_site = fname_site,
    site_id = "UK-AMO"
  )
  mm_42 <- metamet(
    dt = fname_dt_42,
    dt_meta = fname_meta,
    dt_site = fname_site,
    site_id = "UK-AMO"
  )

  mm_32 <- time_average(mm_32, avg.time = "30 min")
  mm_41 <- time_average(mm_41, avg.time = "30 min")
  mm_42 <- time_average(mm_42, avg.time = "30 min")

  dim(mm_32$dt)
  dim(mm_41$dt)
  dim(mm_42$dt)

  mm <- join(mm_32, mm_41)
  mm <- join(mm, mm_42)
  dim(mm$dt)

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )
  dim(mm$dt_ref)

  mm <- apply_qc(mm)

  summary(mm$dt)
  mm$dt[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = names(mm$dt)]
  mm$dt_qc[,
    sapply(.SD, function(x) sum(as.numeric(x == 1))),
    .SDcols = names(mm$dt_qc)
  ]

  n_na_before_sw <- as.integer(sum(is.na(mm$dt$SW_IN_7_1_1)))
  n_qc_missing_sw <- as.integer(sum(mm$dt_qc$SW_IN_7_1_1 == 1))
  n_na_before_g <- as.integer(sum(is.na(mm$dt$G_8_1_1)))
  n_qc_missing_g <- as.integer(sum(mm$dt_qc$G_8_1_1 == 1))

  mm <- impute(
    v_y = NULL,
    mm = mm,
    method = NULL,
    fit = TRUE,
    plot_graph = FALSE
  )

  n_na_after_sw <- as.integer(sum(is.na(mm$dt$SW_IN_7_1_1)))
  n_qc_era5_sw <- as.integer(sum(mm$dt_qc$SW_IN_7_1_1 == 7))
  n_na_after_g <- as.integer(sum(is.na(mm$dt$G_8_1_1)))
  n_qc_era5_g <- as.integer(sum(mm$dt_qc$G_8_1_1 == 7))

  expect_identical(n_na_before_sw, n_qc_missing_sw)
  expect_identical(n_na_before_sw, n_qc_era5_sw)
  expect_identical(n_na_before_g, n_qc_era5_g)
  expect_identical(n_na_after_sw, 0L)
  expect_identical(n_na_after_g, 0L)
})
