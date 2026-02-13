# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("metamet time averaging 1-min data to 5-mins works", {
  fname_dt <- testthat::test_path("data-raw/UK-AMo_BM_20250822_L03_F02.dat")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  fname_era5 <- testthat::test_path("data-raw/dt_era5.csv")
  mm <- metamet(
    dt = fname_dt,
    dt_meta = fname_meta,
    dt_site = fname_site,
    site_id = "UK-AMO"
  )
  time_name <- mm$dt_meta[type == "time", name_dt]

  # convert precip "P_12_1_1" in mm to a rate mm/s
  mm$dt <- convert_sum_to_rate(
    mm$dt,
    v_var_to_convert = "P_12_1_1",
    time_name = time_name
  )

  mm$dt_meta$standard_name_era5

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  dim(mm$dt)
  dim(mm$dt_ref)
  mm$dt[1:5, 1:4]
  mm$dt_ref[1:5, 1:4]
  mm$dt[(.N - 3):.N, 1:4]
  mm$dt_ref[(.N - 3):.N, 1:4]

  mm <- time_average(mm, avg.time = "5 min", report_end_interval = TRUE)
  dim(mm$dt)
  head(mm$dt)
  tail(mm$dt)

  expect_equal(sum(is.na(mm$dt_ref[, ..time_name])), 0)
  expect_s3_class(mm, "metamet")
  expect_s3_class(mm$dt_ref, "data.table")
  expect_equal(nrow(mm$dt_ref), 288)
  expect_equal(ncol(mm$dt_ref), 4)
})
x <- c(
  1.471,
  1.474,
  1.458,
  1.452,
  1.440
)
mean(x)
