# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("metamet reading from files works", {
  fname_dt <- testthat::test_path("data-raw/UK-AMo_BM_20260126_L03_F02.dat")
  fname_meta <- testthat::test_path("data-raw/dt_meta.csv")
  fname_site <- testthat::test_path("data-raw/dt_site.csv")
  mm_t1 <- metamet(
    dt = fname_dt,
    dt_meta = fname_meta,
    dt_site = fname_site
  )
  fname_dt <- testthat::test_path("data-raw/UK-AMO_BM_dt_2026.csv")
  mm_t2 <- metamet(
    dt = fname_dt,
    dt_meta = fname_meta,
    dt_site = fname_site
  )
  time_name_t1 <- mm_t1$dt_meta[type == "time", name_dt]
  time_name_t2 <- mm_t2$dt_meta[type == "time", name_dt]

  expect_equal(sum(is.na(mm_t1$dt[, ..time_name_t1])), 0)
  expect_equal(sum(is.na(mm_t2$dt[, ..time_name_t2])), 0)
  expect_s3_class(mm_t1, "metamet")
  expect_s3_class(mm_t1$dt, "data.table")
  expect_s3_class(mm_t2, "metamet")
  expect_s3_class(mm_t2$dt, "data.table")
  expect_equal(nrow(mm_t1$dt), 59)
  expect_equal(ncol(mm_t1$dt), 3)
  expect_equal(nrow(mm_t2$dt), 1488)
  expect_equal(ncol(mm_t2$dt), 38)
})
