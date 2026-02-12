rename_era5 <- function(
  mm,
  fname_era5
) {
  dt_era5 <- fread(fname_era5)
  # changing time resolution is a pain if we have a mix of averaged and summed
  # variables, so make all time-averaged per second.
  # convert precip "tp" in mm to a rate mm/s
  dt_era5 <- convert_sum_to_rate(
    dt_era5,
    var_to_convert = "tp",
    time_name = "time"
  )
  dt_era5[, site := mm$dt_site$site]
  ind <- match(names(mm$dt), mm$dt_meta$name_dt)
  v_names_era5 <- mm$dt_meta$standard_name_era5[ind]
  dt_era5 <- dt_era5[, ..v_names_era5]
  names(dt_era5) <- names(mm$dt)
  return(dt_era5)
}


add_era5 <- function(
  mm,
  fname_era5 = "data-raw/UK-AMO/dt_era5.csv",
  restrict_era5_to_obs = TRUE,
  restrict_obs_to_era5 = FALSE,
  report_end_interval = TRUE,
  first_date = NULL,
  last_date = NULL,
  extra_rows = 2
) {
  # get era5 data
  dt_era5 <- rename_era5(mm, fname_era5)

  time_name <- mm$dt_meta[type == "time", name_dt]

  # Do units work in timeAverage? Maybe set, drop and reset in time_average

  # restrict era5 data to date range of obs
  if (restrict_era5_to_obs) {
    start_date <- min(mm$dt[, get(time_name)])
    end_date <- max(mm$dt[, get(time_name)])
    dt_era5 <- dt_era5[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }

  # restrict obs data to date range of era5
  if (restrict_obs_to_era5) {
    start_date <- min(dt_era5[, get(time_name)])
    end_date <- max(dt_era5[, get(time_name)])
    mm$dt <- mm$dt[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
    mm$dt_qc <- mm$dt_qc[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }

  # check the era5 time resolution is the same as the mm$dt obs data
  interval_length_dt <- as.numeric(difftime(
    mm$dt[2, get(time_name)],
    mm$dt[1, get(time_name)],
    units = "secs"
  ))
  interval_length_era5 <- as.numeric(difftime(
    dt_era5[2, get(time_name)],
    dt_era5[1, get(time_name)],
    units = "secs"
  ))

  if (interval_length_era5 != interval_length_dt) {
    dt_era5 <- time_average_dt(
      dt_era5,
      avg.time = paste(interval_length_dt, "sec"),
      first_date = min(mm$dt[, get(time_name)]),
      last_date = max(mm$dt[, get(time_name)]),
      time_name = time_name,
      wd_name = mm$dt_meta[type == "wind direction", name_dt],
      ws_name = mm$dt_meta[type == "wind speed" | type == "windspeed", name_dt],
      report_end_interval = report_end_interval,
      extra_rows = extra_rows
    )
  }

  # check that all are the same no. rows
  if (!identical(dim(mm$dt)[1], dim(mm$dt_qc)[1], dim(dt_era5)[1])) {
    print(paste(
      "Obs and ERA5 data tables do not have the same dimensions",
      dim(mm$dt),
      dim(mm$dt_qc),
      dim(dt_era5)
    ))
  }

  mm <- list(dt = mm$dt, dt_qc = mm$dt_qc, dt_era5 = dt_era5)
  return(mm)
}
