test_that("time_average works", {
  # convert precip "P_12_1_1" in mm to a rate mm/s
  mm3$dt <- convert_sum_to_rate(
    mm3$dt,
    v_var_to_convert = "P_12_1_1",
    time_name = "TIMESTAMP"
  )

  P_mean_raw <- mean(mm3$dt$P_12_1_1, na.rm = TRUE)
  D_mean_raw <- mean(mm3$dt$D_SNOW_4_1_1, na.rm = TRUE)
  names(mm3$dt)
  summary(mm3$dt)
  mm_avg <- time_average(mm3, avg.time = "1 hour")
  summary(mm_avg$dt)
  names(mm3$dt)
  P_mean_avg <- mean(mm_avg$dt$P_12_1_1, na.rm = TRUE)
  D_mean_avg <- mean(mm_avg$dt$D_SNOW_4_1_1, na.rm = TRUE)

  expect_equal(round(P_mean_raw * 1e5, 2), round(P_mean_avg * 1e5, 2))
  # means do not match exactly
  expect_equal(round(D_mean_raw, 4), round(D_mean_avg, 4))
})
