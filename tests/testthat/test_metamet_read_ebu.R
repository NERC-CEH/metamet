# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("reading metamet from files from EBU works", {
  # paths should be testthat::test_path for testing - usr a shortcut to point
  # back to data-raw?
  fname_dt <- testthat::test_path("data-raw/UK-EBU/UK-EBU_BM_dt_2023.csv")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  fname_era5 <- testthat::test_path("data-raw/UK-AMO/dt_era5.csv")

  dt <- fread(fname_dt)
  dt_meta <- setDT(readxl::read_excel(fname_meta))
  dt_site <- fread(fname_site)

  mm <- metamet(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-EBU"
  )
  summary(mm$dt)
  dim(mm$dt)
  names(mm$dt)

  mm <- change_naming_convention(mm, name_convention = "name_icos")

  detect_gaps(mm$dt, expected_interval_mins = 30, time_name = "TIMESTAMP")

  mm$dt <- pad_data(mm$dt, time_name = "TIMESTAMP")
  detect_gaps(mm$dt, expected_interval_mins = 30, time_name = "TIMESTAMP")

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = TRUE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  dim(mm$dt)
  dim(mm$dt_ref)
  names(mm$dt)
  names(mm$dt_ref)

  sum(is.na(mm$dt))
  mm <- apply_qc(mm)
  sum(is.na(mm$dt))

  summary(mm$dt)
  mm$dt[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = names(mm$dt)]

  mm <- impute(
    v_y = c("PA_1_1_1"),
    mm = mm,
    method = "time",
    fit = TRUE,
    plot_graph = FALSE
  )

  saveRDS(mm, file = here::here("data-raw/UK-EBU/UK-EBU_BM_mm_2023.rds"))

  time_name <- mm$dt_meta[type == "time", name_dt]

  expect_s3_class(mm, "metamet")
  expect_s3_class(mm$dt_qc, "data.table")
  expect_equal(sum(is.na(mm$dt_qc[, ..time_name])), 0)
  expect_equal(nrow(mm$dt_ref), nrow(mm$dt_qc))
  # qc loses the extra validator column when averaged, so should be same
  expect_equal(ncol(mm$dt), ncol(mm$dt_qc) - 1)
  # should not be any duplicate times
  expect_equal(nrow(mm$dt[duplicated(mm$dt[, ..time_name]), ]), 0)
})
