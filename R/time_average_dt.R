##' Time-average a data table
##'
##' Aggregates meteorological data in a data table over specified time intervals.
##' Handles flexible variable naming, wind direction vector averaging, and
##' configurable reporting of interval start or end times. Uses \code{openair::timeAverage()}
##' for aggregation with support for custom statistics (mean, median, sum, etc.).
##'
##' @param dt_in A data table containing meteorological observations with a time column.
##' @param avg.time Time interval for averaging (e.g., "30 min", "1 hour"). Default: "30 min".
##'   Passed to \code{openair::timeAverage()}.
##' @param statistic Character string specifying the aggregation statistic ("mean", "median", "sum", etc.).
##'   Default: "mean". Use "median" for quality control codes.
##' @param first_date Optional POSIXct date. Earliest date to include. If \code{NULL}, uses minimum date in data.
##' @param last_date Optional POSIXct date. Latest date to include. If \code{NULL}, uses maximum date in data.
##' @param time_name Character string. Name of the time column in \code{dt_in} (e.g., "time").
##' @param wd_name Character string. Name of wind direction column, if present. Used for vector averaging.
##' @param ws_name Character string. Name of wind speed column, if present. Used for vector averaging.
##' @param report_end_interval Logical. If \code{TRUE} (default), timestamps report the end of the averaging interval.
##'   If \code{FALSE}, timestamps report the start of the interval (default \code{openair} behavior).
##' @param extra_rows Integer (default: 2). Number of extra time intervals to include before and after
##'   the specified date range, used for padding during averaging.
##'
##' @details
##' - Creates a temporary copy of input data to avoid modifying the original by reference.
##' - Renames the time column to "date" for compatibility with \code{openair::timeAverage()}.
##' - Optionally renames wind direction and wind speed columns for proper vector averaging.
##' - Pads the averaging window with extra intervals (controlled by \code{extra_rows}) then restricts
##'   results to the original date range.
##' - Converts any factor columns (e.g., "site") to character for consistency.
##' - Uses "nocb" (next observation carried backward) to fill leading missing values.
##' - Restores original time and column names in the output.
##'
##' @return A data table with time-averaged values, preserving the original time column name and
##'   variable structure. The "site" column is stored as character.
##'
##' @seealso
##' - \code{\link{time_average}} for averaging a `metamet` object
##' - \code{\link{add_era5}} for applying time averaging to ERA5 reference data
##'
##' @examples
##' # dt_averaged <- time_average_dt(
##' #   dt_in = my_dt,
##' #   avg.time = "1 hour",
##' #   time_name = "time",
##' #   wd_name = "wind_direction",
##' #   ws_name = "wind_speed"
##' # )
##'
##' @keywords internal
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
  # we need to make a copy to avoid modifying the original object by reference
  # i.e. we (probably) want to retain the unaveraged mm object
  dt <- copy(dt_in)

  interval_length_s <- as.numeric(lubridate::duration(avg.time))

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
  end_date <- lubridate::ceiling_date(last_date, unit = "hour") +
    interval_length_s * extra_rows

  # a data.table time averaging function would presumably be quicker
  # intervalaverage exists but only does IDate i.e. whole days
  df_mean <- openair::timeAverage(
    openair::selectByDate(dt, start = first_date, end = last_date),
    start.date = start_date,
    avg.time = avg.time,
    fill = TRUE,
    statistic = statistic,
    type = "site"
  )
  dt <- data.table::setDT(df_mean) # openair returns df - upgrade to dt

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
    # should be the same as interval_length_s so this is overcautious
    dt[, date := date + interval_length]
  }

  # restrict averaged data to date range of original data
  dt <- dt[
    date >= first_date & date <= last_date
  ]

  # this only works on character variables, so have to split and cbind
  dt_num <- dt[, -c("date", "site")]

  # force all to be numeric; if any col contains only NA, this is set as logical
  dt_num[, names(dt_num) := lapply(.SD, as.numeric)]
  # have another check:
  if (any(sapply(dt_num, class) != "numeric")) {
    stop(
      "Trying to time-average some non-numeric columns; maybe missing values?"
    )
  }

  # first few rows may be missing data; fill in with nocb
  data.table::setnafill(dt_num, type = "nocb")
  data.table::setnafill(dt_num, type = "locf")
  dt <- cbind(dt[, c("date", "site")], dt_num)

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
