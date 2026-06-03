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
##' @param extra_rows Integer A number of time intervals to add before and
##'   after the data; usually truncated after averaging.
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
  report_end_interval = TRUE,
  extra_rows = 0
) {
  # we need to make a copy to avoid modifying the original object by reference
  # i.e. we (probably) want to retain the unaveraged mm object
  mm <- .ensure_wide(copy(mm_in))

  # get the name and format of the time, precip, ws & wd variables
  time_name <- mm$dt_meta[type == "time", name_dt]

  # Drop NA-timestamp rows and sort — openair::timeAverage fails with NAs in
  # the date column (any(dst(...)) returns NA instead of TRUE/FALSE).
  mm$dt <- mm$dt[!is.na(get(time_name))]
  data.table::setorderv(mm$dt, time_name)
  precip_name <- mm$dt_meta[type == "precipitation", name_dt]
  wd_name <- mm$dt_meta[type == "wind direction", name_dt]
  ws_name <- mm$dt_meta[type == "wind speed" | type == "windspeed", name_dt]

  # call the function to perform averaging as necessary
  if (!is.null(mm$dt_qc)) {
    if ("var_name" %in% names(mm$dt_qc)) {
      # Long-format dt_qc: pivot to wide, average, pivot back.
      # Wind direction / speed do not appear as columns here, so pass NULL.
      f <- stats::as.formula(
        paste0("site + `", time_name, "` ~ var_name")
      )
      dt_qc_wide <- data.table::dcast(mm$dt_qc, f, value.var = "qc")
      dt_qc_wide <- time_average_dt(
        dt_qc_wide,
        avg.time = avg.time,
        statistic = "median",
        first_date = min(mm$dt[, get(time_name)]),
        last_date = max(mm$dt[, get(time_name)]),
        time_name = time_name,
        wd_name = NULL,
        ws_name = NULL,
        report_end_interval = report_end_interval,
        extra_rows = extra_rows
      )
      mm$dt_qc <- data.table::melt(
        dt_qc_wide,
        id.vars = c("site", time_name),
        variable.name = "var_name",
        value.name = "qc",
        variable.factor = FALSE
      )
      mm$dt_qc[, validator := NA_character_]
      mm$dt_qc[, comment := NA_character_]
    } else {
      mm$dt_qc <- time_average_dt(
        mm$dt_qc,
        avg.time = avg.time,
        statistic = "median",
        first_date = min(mm$dt[, get(time_name)]),
        last_date = max(mm$dt[, get(time_name)]),
        time_name = time_name,
        wd_name = wd_name,
        ws_name = ws_name,
        report_end_interval = report_end_interval,
        extra_rows = extra_rows
      )
      # character variables are lost on averaging, but we want to keep them
      mm$dt_qc[, validator := NA]
    }
  }

  if (!is.null(mm$dt_ref)) {
    mm$dt_ref <- time_average_dt(
      mm$dt_ref,
      avg.time = avg.time,
      statistic = "median", # use "median" for qc codes
      first_date = min(mm$dt[, get(time_name)]),
      last_date = max(mm$dt[, get(time_name)]),
      time_name = time_name,
      wd_name = wd_name,
      ws_name = ws_name,
      report_end_interval = report_end_interval,
      extra_rows = extra_rows
    )
  }

  # do dt last as first_date/last_date are based on this and we do not want to
  # use the time-averaged start/end
  mm$dt <- time_average_dt(
    mm$dt,
    avg.time = avg.time,
    first_date = min(mm$dt[, get(time_name)]),
    last_date = max(mm$dt[, get(time_name)]),
    time_name = time_name,
    wd_name = wd_name,
    ws_name = ws_name,
    report_end_interval = report_end_interval,
    extra_rows = extra_rows
  )
  return(mm)
}
