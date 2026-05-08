# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("applying imputing works", {
  fname_dt <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv")
  fname_qc <- pkg_extdata("UK-AMO/UK-AMO_BM_qc_2026.csv")
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
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  mm <- apply_qc(mm)
  mm <- metamet_reshape(mm, "long")

  # in long format, count NAs and QC codes via the value/qc columns
  n_na_before_ppfd <- sum(is.na(mm$dt[name_icos == "PPFD_DIF", value]))
  n_na_before_rh   <- sum(is.na(mm$dt[name_icos == "RH",       value]))
  n_na_before_rg   <- sum(is.na(mm$dt[name_icos == "RG",       value]))

  # v_y must use ICOS names (name_icos), not local names with replicate suffixes
  mm <- impute(
    v_y = c("PPFD_DIF", "RH", "RG"),
    mm = mm,
    method = NULL,
    fit = TRUE,
    plot_graph = FALSE
  )

  n_na_after_ppfd <- sum(is.na(mm$dt[name_icos == "PPFD_DIF", value]))
  n_na_after_rh   <- sum(is.na(mm$dt[name_icos == "RH",       value]))
  n_na_after_rg   <- sum(is.na(mm$dt[name_icos == "RG",       value]))

  expect_equal(n_na_after_ppfd, 0L)
  expect_equal(n_na_after_rh,   0L)
  expect_equal(n_na_after_rg,   0L)
  # imputed rows should have fewer NAs than before
  expect_lte(n_na_after_ppfd, n_na_before_ppfd)
  expect_lte(n_na_after_rh,   n_na_before_rh)
  expect_lte(n_na_after_rg,   n_na_before_rg)
})
