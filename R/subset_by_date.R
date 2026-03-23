##' Subset a `metamet` object by date
##'
##' Subsets a `metamet` object to a date range between a start and end date.
##' This is applied to the observation data (`dt`) and the QC (`dt_qc`) and
##' reference data (`dt_ref`) if they are present.
##'
##' @param mm A `metamet` object containing observation data and metadata.
##' @param start_date  Earliest date to include in subset.
##' @param end_date  Last date to include in subset.
##'
##' @details
##' - Limits all time-referenced data to between the start and end dates.
##' - Dates are inclusive.
##' - Warns if date variable not present.
##'
##' @return The `metamet` object `mm`, restricted to between start and end dates.
##'
##' @seealso
##' - \code{\link{rename_era5}} for variable renaming
##' - \code{\link{time_average_dt}} for time-averaging implementation
##' - \code{\link{metamet}} for object construction
##'
##' @examples
#' \dontrun{
##' mm <- subset_by_date(
##'   mm,
##'   start_date = "2025-06-01 00:30:00",
##'   end_date   = "2025-06-02 00:00:00"
##' )
##' }
##' @export
subset_by_date <- function(
  mm,
  start_date = "2025-01-01 00:30:00",
  end_date = "2026-01-01 00:00:00"
) {
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)
  time_name <- mm$dt_meta[type == "time", name_dt]
  # if duplicate time variables, stop or discard if all the same
  if (length(unique(time_name)) > 1) {
    stop("Multiple time variables present in input file.")
  } else {
    time_name <<- unique(time_name)
  }
  time_name <- "TIMESTAMP" ##* WIP: temp test

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
