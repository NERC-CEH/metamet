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

read_csv_with_excel_datetimes <- function(
  file,
  datetime_cols,
  tz = "UTC",
  sample_n = 5000L
) {
  dt <- fread(file)

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
