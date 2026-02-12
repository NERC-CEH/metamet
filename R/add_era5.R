add_era5 <- function(
  mm,
  fname_era5 = "data-raw/UK-AMO/dt_era5.csv",
  restrict_ref_to_obs = TRUE,
  restrict_obs_to_ref = FALSE,
  report_end_interval = TRUE,
  first_date = NULL,
  last_date = NULL,
  extra_rows = 2
) {
  # get era5 data to use as reference
  mm$dt_ref <- fread(fname_era5)
  # changing time resolution is a pain if we have a mix of averaged and summed
  # variables, so make all time-averaged per second.
  # convert precip "tp" in mm to a rate mm/s
  mm$dt_ref <- convert_sum_to_rate(
    mm$dt_ref,
    var_to_convert = "tp",
    time_name = "time"
  )
  # rename variables with names in dt
  mm <- rename_era5(mm)

  time_name <- mm$dt_meta[type == "time", name_dt]

  # Do units work in timeAverage? Maybe set, drop and reset in time_average

  # restrict ref data to date range of obs
  if (restrict_ref_to_obs) {
    start_date <- min(mm$dt[, get(time_name)])
    end_date <- max(mm$dt[, get(time_name)])
    mm$dt_ref <- mm$dt_ref[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }

  # restrict obs data to date range of ref
  if (restrict_obs_to_ref) {
    start_date <- min(mm$dt_ref[, get(time_name)])
    end_date <- max(mm$dt_ref[, get(time_name)])
    mm$dt <- mm$dt[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
    mm$dt_qc <- mm$dt_qc[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }

  # check the ref time resolution is the same as the mm$dt obs data
  interval_length_dt <- as.numeric(difftime(
    mm$dt[2, get(time_name)],
    mm$dt[1, get(time_name)],
    units = "secs"
  ))
  interval_length_ref <- as.numeric(difftime(
    mm$dt_ref[2, get(time_name)],
    mm$dt_ref[1, get(time_name)],
    units = "secs"
  ))

  if (interval_length_ref != interval_length_dt) {
    mm$dt_ref <- time_average_dt(
      mm$dt_ref,
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
  if (!identical(dim(mm$dt)[1], dim(mm$dt_ref)[1])) {
    print(paste(
      "Obs and ERA5 data tables do not have the same dimensions",
      dim(mm$dt),
      dim(mm$dt_ref)
    ))
  }
  return(mm)
}

rename_era5 <- function(mm) {
  mm$dt_ref[, site := mm$dt_site$site]
  ind <- match(names(mm$dt), mm$dt_meta$name_dt)
  v_names_era5 <- mm$dt_meta$standard_name_era5[ind]
  mm$dt_ref <- mm$dt_ref[, ..v_names_era5]
  names(mm$dt_ref) <- names(mm$dt)
  return(mm)
}
