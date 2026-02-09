test_that("cbinding works", {
  mm_combined <- cbind(mm1, mm2)
  expect_equal(nrow(mm_combined$dt), nrow(mm1$dt))
  expect_equal(ncol(mm_combined$dt), ncol(mm1$dt) + ncol(mm2$dt) - 1) # minus 1 to account for the shared
  expect_equal(
    nrow(mm_combined$dt_meta),
    nrow(mm1$dt_meta) + nrow(mm2$dt_meta) - 1
  ) # minus 1 to account for the shared time variable
})
