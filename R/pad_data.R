#' @title pad_data
#' @description Adds in any gaps in a data frame representing a time series
#' @param df A data frame
#' @param by Time interval of series, Default: '30 min'
#' @param time_name Column name for POSIX date/time variable in df, Default: 'DATECT'
#' @param v_dates A vector of POSIX date/times, potentially from another df, to match with it.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  df <- pad_data(df)
#'  }
#' }
#' @rdname pad_data
#' @export
pad_data <- function(dt, by = NULL, time_name = NULL, v_dates = NULL) {
  setDT(dt)
  if (is.null(v_dates)) {
    v_dates <- dt[, get(time_name)]
  }

  if (is.null(by)) {
    # if not specified otherwise, create a text string representing time
    # interval in the data table
    interval_length_mins <- as.numeric(difftime(
      dt[2, get(time_name)],
      dt[1, get(time_name)],
      units = "mins"
    ))
    by <- paste(interval_length_mins, "min")
  }

  first <- min(v_dates, na.rm = TRUE)
  last <- max(v_dates, na.rm = TRUE)
  # make a dt with complete time series with interval "by"
  dt_date <- data.table(date = seq.POSIXt(first, last, by = by))

  # make the time_name column be the same in both and set as a key
  setnames(dt_date, "date", eval(time_name))
  setkeyv(dt_date, time_name)
  setkeyv(dt, time_name)

  # then we can safely join without specifying "on" variables
  dt <- dt[dt_date, ]
  return(dt)
}

#' @title detect_gaps
#' @description Detects any gaps in a data frame representing a time series
#' @param df A data frame
#' @param by Time interval of series, Default: '30 min'
#' @param time_name Column name for POSIX date/time variable in df
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  gaps <- detect_gaps(l_logr$df)
#'  }
#' }
#' @rdname detect_gaps
#' @export
detect_gaps <- function(
  dt_in,
  expected_interval_mins = NULL,
  time_name = NULL
) {
  dt <- copy(dt_in)
  setDT(dt)
  v_dates <- dt[, get(time_name)]
  dt[, date_curr := v_dates]
  dt[, date_prev := shift(date_curr, 1)]
  dt[, date_int := difftime(date_curr, date_prev, units = "mins")]
  dt$date_int[1] <- expected_interval_mins
  v_longer <- sum(dt$date_int > expected_interval_mins)
  v_shorter <- sum(dt$date_int < expected_interval_mins)
  v_gaps <- which(dt$date_int != expected_interval_mins)
  dt_gaps <- dt[v_gaps]
  return(
    # v_longer = v_longer,
    # v_shorter = v_shorter,
    # v_gaps = v_gaps,
    dt_gaps # = dt_gaps
  )
}
