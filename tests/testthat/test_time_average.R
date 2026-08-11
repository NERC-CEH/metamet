test_that("time_average works", {
  # convert precip "P_12_1_1" in mm to a rate mm/s
  mm3$dt <- convert_sum_to_rate(
    mm3$dt,
    v_var_to_convert = "P_12_1_1",
    time_name = "TIMESTAMP"
  )

  # raw means for non-precip variables
  D_mean_raw <- mean(mm3$dt$D_SNOW_4_1_1, na.rm = TRUE)

  # compute expected hourly precipitation sums
  dt <- copy(mm3$dt)
  dt[, TIMESTAMP := as.POSIXct(TIMESTAMP)]
  expected_precip <- dt[,
    .(
      P_hourly_sum = sum(P_12_1_1, na.rm = TRUE)
    ),
    by = .(hour = lubridate::floor_date(TIMESTAMP, "hour"))
  ]
  # run averaging
  mm_avg <- time_average(mm3, avg.time = "1 hour")
  # extract averaged precip
  avg_precip <- mm_avg$dt[,
    .(
      P_hourly_sum = sum(P_12_1_1, na.rm = TRUE)
    ),
    by = TIMESTAMP
  ]
  # precipitation: sum check
  expect_equal(
    round(expected_precip$P_hourly_sum, 4),
    round(avg_precip$P_hourly_sum, 4)
  )
  # non-precip variable: mean check
  D_mean_avg <- mean(mm_avg$dt$D_SNOW_4_1_1, na.rm = TRUE)
  expect_equal(round(D_mean_raw, 4), round(D_mean_avg, 4))
})
