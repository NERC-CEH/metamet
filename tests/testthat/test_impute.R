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

  time_name <- mm$dt_meta[type == "time", name_dt]

  n_na_before <- sum(is.na(mm$dt$PPFD_DIF))
  n_qc_missing <- sum(mm$dt_qc$PPFD_DIF == 1)

  mm <- impute(
    y = "PPFD_DIF",
    mm = mm,
    method = "era5",
    date_field = time_name,
    fit = TRUE,
    plot_graph = FALSE
  )

  expect_identical(n_na_before, n_qc_missing)
  expect_identical(n_na_before, sum(mm$dt_qc$PPFD_DIF == 7))
})
