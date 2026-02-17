subset_by_date <- function(
  mm,
  start_date = "2025-01-01 00:30:00",
  end_date = "2026-01-01 00:00:00"
) {
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)
  time_name <- mm$dt_meta[type == "time", name_dt]

  if (time_name %!in% names(mm$dt)) {
    stop("Date variable not present when trying to subset by date")
  }

  mm$dt <- mm$dt[get(time_name) >= start_date & get(time_name) <= end_date]

  # repeat for qc if present
  if (!is.null(mm$dt_qc)) {
    mm$dt_qc <- mm$dt_qc[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }
  # repeat for ref if present
  if (!is.null(mm$dt_ref)) {
    mm$dt_ref <- mm$dt_ref[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }

  return(mm)
}
