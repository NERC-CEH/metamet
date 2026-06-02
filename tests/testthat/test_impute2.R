test_that("applying imputing works for more files", {
  fname_dt_32 <- pkg_extdata("UK-AMO/UK-AMo_BM_20250822_L03_F02.dat")
  fname_dt_41 <- pkg_extdata("UK-AMO/UK-AMo_BM_20250822_L04_F01.dat")
  fname_dt_42 <- pkg_extdata("UK-AMO/UK-AMo_BM_20250822_L04_F02.dat")
  fname_era5 <- pkg_extdata("dt_era5.csv")

  mm_32 <- metamet(
    dt = fname_dt_32,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  )
  mm_41 <- metamet(
    dt = fname_dt_41,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  )
  mm_42 <- metamet(
    dt = fname_dt_42,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  )

  mm_32 <- time_average(mm_32, avg.time = "30 min")
  mm_41 <- time_average(mm_41, avg.time = "30 min")
  mm_42 <- time_average(mm_42, avg.time = "30 min")

  mm <- join(mm_32, mm_41)
  mm <- join(mm, mm_42)

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  # apply_qc converts to long format — no further reshape needed
  mm <- apply_qc(mm)
  expect_equal(attr(mm, "format"), "long")

  n_na_before_sw <- as.integer(sum(is.na(mm$dt[
    var_name == "SW_IN_7_1_1",
    value
  ])))
  n_qc_missing_sw <- as.integer(mm$dt[var_name == "SW_IN_7_1_1" & qc == 1, .N])
  n_na_before_g <- as.integer(sum(is.na(mm$dt[var_name == "G_8_1_1", value])))
  n_qc_missing_g <- as.integer(mm$dt[var_name == "G_8_1_1" & qc == 1, .N])

  mm <- impute(
    v_y = NULL,
    mm = mm,
    method = NULL,
    fit = TRUE,
    plot_graph = FALSE
  )

  n_na_after_sw <- as.integer(sum(is.na(mm$dt[
    var_name == "SW_IN_7_1_1",
    value
  ])))
  n_qc_era5_sw <- as.integer(mm$dt[var_name == "SW_IN_7_1_1" & qc == 7, .N])
  n_na_after_g <- as.integer(sum(is.na(mm$dt[var_name == "G_8_1_1", value])))
  n_qc_era5_g <- as.integer(mm$dt[var_name == "G_8_1_1" & qc == 7, .N])

  expect_identical(n_na_before_sw, n_qc_missing_sw)
  expect_identical(n_na_before_sw, n_qc_era5_sw)
  expect_identical(n_na_before_g, n_qc_era5_g)
  expect_identical(n_na_after_sw, 0L)
  expect_identical(n_na_after_g, 0L)
})
