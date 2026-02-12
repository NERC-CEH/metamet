time_average_dt <- function(
  dt_in,
  avg.time = "30 min",
  statistic = "mean", # use "median" for qc codes
  first_date = NULL,
  last_date = NULL,
  time_name = NULL,
  wd_name = NULL,
  ws_name = NULL,
  report_end_interval = TRUE,
  extra_rows = 2
) {
  dt <- copy(dt_in) # do we need this to avoid modifying the original object?

  # extract numeric time from character - must be in seconds
  if (stringr::str_detect(avg.time, "sec")) {
    regexp <- "[[:digit:]]+"
    interval_length_s <- as.numeric(stringr::str_extract(avg.time, regexp))
  } else {
    stop("Specify time interval for averaging in 'sec'")
  }

  # rename time variable with openair convention
  setnames(dt, eval(time_name), "date")
  # and ws & wd if present, for proper vector averaging
  if (length(wd_name) > 0) {
    dt[, wd := get(wd_name)]
  }
  if (length(ws_name) > 0) {
    dt[, ws := get(ws_name)]
  }

  # if not passed in, use the limits of this data table
  if (is.null(first_date)) {
    first_date <- min(dt[, date])
  }
  if (is.null(last_date)) {
    last_date <- max(dt[, date])
  }
  # start at the beginning of the first hour, xx:00:00
  start_date <- lubridate::floor_date(first_date, unit = "hour") -
    interval_length_s * extra_rows
  # run up to 1 second before the next hour to avoid creating an extra interval
  # with only 1 value
  end_date <- lubridate::ceiling_date(last_date, unit = "hour") +
    interval_length_s * extra_rows

  # a data.table time averaging function would presumably be quicker
  # intervalaverage exists but only does IDate i.e. whole days
  df_mean <- openair::timeAverage(
    openair::selectByDate(dt, start = first_date, end = last_date),
    start.date = start_date,
    # end_date = end_date,
    avg.time = avg.time,
    fill = TRUE,
    statistic = statistic,
    type = "site"
  )
  dt <- data.table::as.data.table(df_mean) # openair returns df - upgrade to dt

  # data.table::setnafill(dt[, c(-1, -2)], type = "locf")
  # data.table::setnafill(dt[, -c("date", "site")], type = "nocb")

  # site becomes a factor but we want a character variable
  dt[, site := as.character(site)]

  # Note that timeAverage associates the mean with the start of the averaging
  # period, so for hourly data the mean value for 00:00:00 is the mean of values
  # from 00:00:00 to 00:59:59.
  # ICOS reports values at the time of sampling, so any logger averaging applies
  # to the previous time interval. We should perhaps report both interval start
  # and end times in the output data table.
  # 'report_end_interval = FALSE' gives the default behaviour of timeAverage,
  # which is to associate the mean with the start of the averaging period;
  # 'report_end_interval = TRUE' associates the mean with the end of the
  # averaging period.
  if (report_end_interval) {
    # report the end time of the interval instead of the start time
    interval_length <- difftime(dt[2, date], dt[1, date])
    dt[, date := date + interval_length]
  }

  # restrict averaged data to date range of original data
  dt <- dt[
    date >= first_date & date <= last_date
  ]

  # first few rows may be missing data; fill in with nocb
  v_coal <- data.table::fcoalesce(dt[, 4:8]) # does not matter which
  first_nona <- min(which(!is.na(v_coal)))
  if (first_nona > 1) {
    dtt <- dt[, -c("date", "site")]
    dtt[1:(first_nona - 1), ] <- dtt[first_nona, ]
    dt <- cbind(dt[, c("date", "site")], dtt)
  }

  # restore original time name
  setnames(dt, "date", eval(time_name))
  # and delete extra names
  if (length(wd_name) > 0) {
    dt[, wd := NULL]
  }
  if (length(ws_name) > 0) {
    dt[, ws := NULL]
  }
  return(dt)
}

# character variables are lost on averaging, but we want to keep the structure the same
# mm$dt_qc[, validator := NA]
