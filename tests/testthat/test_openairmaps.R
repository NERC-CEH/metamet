# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("reading metamet from files from Whim works", {
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  dt_site <- fread(fname_site)
  x <- network_map(dt_site)

  fname_dt <- testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_dt_2026.csv")
  fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  # half-hourly data
  mm <- metamet(
    dt = fname_dt,
    dt_meta = fname_meta,
    dt_site = fname_site,
    site_id = "UK-AMO"
  )
  y <- polar_map(mm, var_name = "PA_4_1_1")

  expect_s3_class(x, "leaflet")
  expect_s3_class(y, "leaflet")
})
