#' Infer whether ambiguous dates are day-first or month-first
#'
#' Inspect a sample of character date strings and infer whether they are more
#' likely to follow a day-first (d/m/Y) or month-first (m/d/Y) convention.
#' Only strings beginning with a pattern of the form `d/m/Y` or `m/d/Y`
#' are considered.
#'
#' @param x Character vector of date-like strings.
#' @param sample_n Integer. Maximum number of non-NA values to inspect.
#'
#' @return
#' * `TRUE`  if the sample suggests day-first (d/m/Y),
#' * `FALSE` if the sample suggests month-first (m/d/Y),
#' * `NA`    if the sample is empty or fully ambiguous.
#'
#' @details
#' The function extracts the first two numeric components of each date string.
#' If any first component is between 13 and 31, the format must be day-first.
#' If any second component is between 13 and 31, the format must be month-first.
#' If neither condition is met, the result is ambiguous.
#'
#' @examples
#' infer_day_first(c("12/08/2020", "31/01/2020"))   # TRUE
#' infer_day_first(c("08/12/2020", "01/31/2020"))   # FALSE
#' infer_day_first(c("01/02/2020", "02/03/2020"))   # NA
#'
#' @keywords internal
infer_day_first <- function(x, sample_n = 5000L) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA)
  }

  x <- x[seq_len(min(sample_n, length(x)))]
  # Keep only things that look like m/d/Y or d/m/Y at the start
  ok <- grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{4}", x)
  x <- x[ok]
  if (!length(x)) {
    return(NA)
  }

  a <- as.integer(sub("^\\s*(\\d{1,2})/.*$", "\\1", x)) # first number
  b <- as.integer(sub("^\\s*\\d{1,2}/(\\d{1,2})/.*$", "\\1", x)) # second number

  if (any(a > 12 & a <= 31, na.rm = TRUE)) {
    return(TRUE)
  } # day-first (dmy)
  if (any(b > 12 & b <= 31, na.rm = TRUE)) {
    return(FALSE)
  } # month-first (mdy)

  NA # fully ambiguous (all days and months <= 12 in sample)
}

#' Parse Excel-style datetime strings with flexible day/month ordering
#'
#' Parse character datetime strings commonly exported from Excel, allowing
#' both day-first (d/m/Y) and month-first (m/d/Y) formats. The parsing order
#' can be explicitly supplied or inferred upstream.
#'
#' @param x Character vector of datetime strings.
#' @param tz Time zone passed to downstream consumers (not used directly by
#'   \code{parse_date_time2()}, which returns POSIXct in UTC).
#' @param prefer_day_first Logical or \code{NA}. If \code{TRUE}, day-first
#'   formats are tried first; if \code{FALSE}, month-first formats are tried
#'   first; if \code{NA}, a neutral mixed ordering is used.
#'
#' @return A POSIXct vector parsed using \code{lubridate::parse_date_time2()}.
#'   Invalid or empty strings become \code{NA}.
#'
#' @details
#' Excel exports ambiguous dates without locale information. This helper
#' chooses an appropriate set of parsing orders based on the inferred or
#' user-specified preference. Both date-only and date-time formats are
#' supported, including `HH:MM` and `HH:MM:SS`.
#'
#' @examples
#' parse_excel_datetime("31/01/2020 12:30", prefer_day_first = TRUE)
#' parse_excel_datetime("01/31/2020 12:30", prefer_day_first = FALSE)
#'
#' @seealso \code{infer_day_first()}, \code{lubridate::parse_date_time2()}
#'
#' @keywords internal
parse_excel_datetime <- function(x, tz = "UTC", prefer_day_first = NA) {
  x <- trimws(x)
  x[x == ""] <- NA_character_

  # Allow both HH:MM and HH:MM:SS, plus date-only just in case
  if (isTRUE(prefer_day_first)) {
    orders <- c("dmY HMS", "dmY HM", "dmY", "mdY HMS", "mdY HM", "mdY")
  } else if (isFALSE(prefer_day_first)) {
    orders <- c("mdY HMS", "mdY HM", "mdY", "dmY HMS", "dmY HM", "dmY")
  } else {
    # If we can't infer, pick a safe default policy (you can change this)
    orders <- c("dmY HM", "dmY HMS", "dmY", "mdY HM", "mdY HMS", "mdY")
  }
  lubridate::parse_date_time2(x, orders = orders)
}

#' Read a CSV and parse Excel-style datetime columns
#'
#' Read a CSV file using \code{data.table::fread()} and automatically parse
#' specified datetime columns that may contain ambiguous Excel-style date
#' formats. The function infers day-first vs. month-first ordering for each
#' column independently.
#'
#' @param file Path to the CSV file.
#' @param datetime_cols Character vector naming the columns to parse.
#' @param tz Time zone to pass to \code{parse_excel_datetime()}.
#' @param sample_n Integer. Number of rows to sample when inferring date order.
#'
#' @return A \code{data.table} with the specified columns converted to POSIXct.
#'
#' @details
#' Each datetime column is coerced to character, sampled, and passed to
#' \code{infer_day_first()} to determine whether day-first or month-first
#' parsing should be attempted first. Parsing is performed by
#' \code{parse_excel_datetime()}.
#'
#' @examples
#' \dontrun{
#' read_csv_with_excel_datetimes(
#'   "export.csv",
#'   datetime_cols = c("created_at", "updated_at")
#' )
#' }
#'
#' @seealso
#'   \code{infer_day_first()},
#'   \code{parse_excel_datetime()},
#'   \code{data.table::fread()}
#'
#' @keywords internal
read_csv_with_excel_datetimes <- function(
  file,
  datetime_cols,
  tz = "UTC",
  sample_n = 5000L
) {
  dt <- fread(file, na.strings = c("NA", "#N/A", ""))

  for (col in datetime_cols) {
    if (!col %in% names(dt)) {
      stop("Column not found: ", col)
    }
    if (!is.character(dt[[col]])) {
      dt[, (col) := as.character(get(col))]
    }

    pref <- infer_day_first(dt[[col]], sample_n = sample_n)
    dt[,
      (col) := parse_excel_datetime(get(col), tz = tz, prefer_day_first = pref)
    ]
  }

  dt
}
