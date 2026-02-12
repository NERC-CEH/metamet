##' Time-average a `metamet` object
##'
##' Aggregates meteorological data in a `metamet` object over specified time
##' intervals. The function handles different variable types appropriately:
##' precipitation is summed, and other variables (temperature, wind speed, etc.)
##' are averaged. Wind direction is vector-averaged if present.
##'
##' The function uses `openair::timeAverage()` for the aggregation and preserves
##' the structure of the input object, including quality control (`dt_qc`) and
##' reference (`dt_ref`) tables if present.
##'
##' @param mm_in A `metamet` object containing at least `dt`, `dt_meta`, and
##'   `dt_site`.
##' @param avg.time Time interval for averaging; passed to
##'   `openair::timeAverage()`. Default is `"30 min"`.
##' @param report_end_interval Logical. If `TRUE` (default), the returned
##'   timestamps represent the end of the averaging interval. If `FALSE`,
##'   timestamps represent the start of the interval.
##'
##' @return A `metamet` object with time-averaged `dt`, `dt_qc`, and `dt_ref`
##'   tables (where applicable). The object structure is preserved.
##'
##' @examples
##' # mm_avg <- time_average(mm, avg.time = "1 hour")
##'
##' @export
time_average <- function(
  mm_in,
  avg.time = "30 min",
  report_end_interval = TRUE
) {
  mm <- copy(mm_in) # do we need this to avoid modifying the original object?

  # get the name and format of the time, precip, ws & wd variables
  time_name <- mm$dt_meta[type == "time", name_dt]
  precip_name <- mm$dt_meta[type == "precipitation", name_dt]
  wd_name <- mm$dt_meta[type == "wind direction", name_dt]
  ws_name <- mm$dt_meta[type == "wind speed" | type == "windspeed", name_dt]

  # define a sub-function that will do the averaging for dt, and for qc and ref if they exist

  perform_averaging <- function(dt = mm$dt, stat = c("mean", "sum")) {
    # rename time variable with openair convention
    # assume these are the same in qc and ref tables
    setnames(dt, eval(time_name), "date")

    # and ws & wd if present, for proper vector averaging
    if (length(wd_name) > 0) {
      dt[, wd := get(wd_name)]
    }
    if (length(ws_name) > 0) {
      dt[, ws := get(ws_name)]
    }

    first_date <- min(dt[, date])
    last_date <- max(dt[, date])
    # start at the beginning of the first hour, xx:00:00
    start.date <- lubridate::floor_date(first_date, unit = "hour")
    # run up to 1 second before the next hour to avoid creating an extra interval with only 1 value
    end.date <- lubridate::ceiling_date(last_date, unit = "hour") - 1

    # a data.table time averaging function would presumably be quicker
    # intervalaverage exists but only does IDate i.e. whole days
    df_mean <- openair::timeAverage(
      openair::selectByDate(dt, start = start.date, end = end.date),
      start.date = start.date,
      avg.time = avg.time,
      statistic = stat[1],
      type = "site"
    )
    # if the data contains precipitation, we want to sum instead of average
    if (length(precip_name) > 0) {
      df_sum <- openair::timeAverage(
        openair::selectByDate(dt, start = start.date, end = end.date),
        start.date = start.date,
        avg.time = avg.time,
        statistic = stat[2],
        type = "site"
      )
      # extract the summed precip as a vector
      v_ppt <- df_sum[, eval(precip_name)][[1]]
      # and put it into the dt of mean values
      df_mean[, eval(precip_name)] <- v_ppt
    }
    dt <- data.table::as.data.table(df_mean) # openair returns a data frame - upgrade to dt
    dt[, site := as.character(site)]

    # Note that timeAverage associates the mean with the start of the averaging period,
    # so for hourly data the mean value for 00:00:00 is the mean of values from 00:00:00 to 00:59:59.
    # ICOS reports values at the time of sampling, so any logger averaging applies to the previous time interval.
    # We should perhaps report both interval start and end times in the output data table.
    # 'report_end_interval = FALSE' gives the default behaviour of timeAverage,
    # which is to associate the mean with the start of the averaging period;
    # 'report_end_interval = TRUE' associates the mean with the end of the averaging period.
    if (report_end_interval) {
      # report the end time of the interval instead of the start time
      interval_length <- difftime(dt[2, date], dt[1, date])
      dt[, date := date + interval_length]
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

  # call the function to perform averaging as necessary
  mm$dt <- perform_averaging(dt = mm$dt, stat = c("mean", "sum"))
  if (!is.null(mm$dt_qc)) {
    mm$dt_qc <- perform_averaging(dt = mm$dt_qc, stat = c("median", "median"))
    # character variables are lost on averaging, but we want to keep the structure the same
    mm$dt_qc[, validator := NA]
  }
  if (!is.null(mm$dt_ref)) {
    mm$dt_ref <- perform_averaging(dt = mm$dt_ref, stat = c("mean", "sum"))
  }
  return(mm)
}
