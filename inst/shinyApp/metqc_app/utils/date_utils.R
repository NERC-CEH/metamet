#' Default processing date range
#'
#' Creates a default date range from 1900-01-01 to two days before today.
#'
#' @return A data frame with `start_date` and `end_date` columns.
default_processing_range <- function() {
  data.frame(
    start_date = as.POSIXct(
      "1900/01/01 00:00",
      format = "%Y/%m/%d %H:%M",
      tz = "UTC"
    ),
    end_date = as.POSIXct(Sys.Date() - 2, tz = "UTC")
  )
}

#' Combine date and time into a POSIXct value
#'
#' Creates a UTC datetime from a date, hour, and minute.
#'
#' @param date Date object.
#' @param hour Hour value (0-23).
#' @param minute Minute value (0-59).
#'
#' @return A POSIXct datetime in UTC.
combine_date_time <- function(date, hour, minute) {
  as.POSIXct(
    sprintf(
      "%02d/%02d/%04d %02d:%02d",
      lubridate::day(date),
      lubridate::month(date),
      lubridate::year(date),
      hour,
      minute
    ),
    format = "%d/%m/%Y %H:%M",
    tz = "UTC"
  )
}

#' Create a processing date range
#'
#' Creates a start and end datetime range, including formatted character
#' representations.
#'
#' @param start_date Start date.
#' @param start_hour Start hour.
#' @param start_minute Start minute.
#' @param end_date End date.
#' @param end_hour End hour.
#' @param end_minute End minute.
#'
#' @return A list containing POSIXct and formatted character date ranges.
make_processing_date_range <- function(
  start_date,
  start_hour,
  start_minute,
  end_date,
  end_hour,
  end_minute
) {
  list(
    start_date = combine_date_time(
      start_date,
      start_hour,
      start_minute
    ),
    end_date = combine_date_time(
      end_date,
      end_hour,
      end_minute
    ),
    start_date_ch = format(
      combine_date_time(start_date, start_hour, start_minute),
      "%d/%m/%Y %H:%M"
    ),
    end_date_ch = format(
      combine_date_time(end_date, end_hour, end_minute),
      "%d/%m/%Y %H:%M"
    )
  )
}
