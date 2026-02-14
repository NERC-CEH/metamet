# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("applying imputing works", {
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

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  mm <- apply_qc(mm)

  summary(mm$dt)
  mm$dt[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = names(mm$dt)]
  mm$dt_qc[,
    sapply(.SD, function(x) sum(as.numeric(x == 1))),
    .SDcols = names(mm$dt_qc)
  ]

  n_na_before_ppfd <- sum(is.na(mm$dt$PPFD_DIF))
  n_qc_missing_ppfd <- sum(mm$dt_qc$PPFD_DIF == 1)
  n_na_before_rh <- sum(is.na(mm$dt$RH_4_1_1))
  n_qc_missing_rh <- sum(mm$dt_qc$RH_4_1_1 == 1)
  n_na_before_rg <- sum(is.na(mm$dt$RG_4_1_0))
  n_qc_missing_rg <- sum(mm$dt_qc$RG_4_1_0 == 1)

  mm <- impute(
    # v_y = c("NDVI_649IN_5_1_1", "D_SNOW", "RG_4_1_0"),
    v_y = c("PPFD_DIF", "RH_4_1_1", "RG_4_1_0"),
    mm = mm,
    method = NULL,
    fit = TRUE,
    plot_graph = TRUE
  )

  n_na_after_ppfd <- sum(is.na(mm$dt$PPFD_DIF))
  n_qc_era5_ppfd <- sum(mm$dt_qc$PPFD_DIF == 7)
  n_na_after_rh <- sum(is.na(mm$dt$RH_4_1_1))
  n_qc_era5_rh <- sum(mm$dt_qc$RH_4_1_1 == 7)
  n_na_after_rg <- sum(is.na(mm$dt$RG_4_1_0))
  n_qc_era5_rg <- sum(mm$dt_qc$RG_4_1_0 == 7)

  expect_identical(n_na_before_ppfd, n_qc_missing_ppfd)
  expect_identical(n_na_before_ppfd, n_qc_era5_ppfd)
  expect_identical(n_na_before_rh, n_qc_era5_rh)
  expect_identical(n_na_before_rg, n_qc_era5_rg)
  expect_identical(
    n_na_after_ppfd,
    n_na_after_rh,
    n_na_after_rg,
    n_qc_era5_ppfd,
    0
  )
})
