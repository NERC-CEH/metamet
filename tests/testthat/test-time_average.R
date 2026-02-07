test_that("time_average works", {
  P_sum_raw <- sum(mm3$dt$P_12_1_1, na.rm = TRUE)
  D_mean_raw <- mean(mm3$dt$D_SNOW_4_1_1, na.rm = TRUE)
  names(mm3$dt)
  summary(mm3$dt)
  mm_avg <- time_average(mm3, avg.time = "1 hour")
  summary(mm_avg$dt)
  names(mm3$dt)
  P_sum_avg <- sum(mm_avg$dt$P_12_1_1, na.rm = TRUE)
  D_mean_avg <- mean(mm_avg$dt$D_SNOW_4_1_1, na.rm = TRUE)

  expect_equal(P_sum_raw, P_sum_avg)
  # means do not match exactly
  expect_equal(round(D_mean_raw, 4), round(D_mean_avg, 4))
})
