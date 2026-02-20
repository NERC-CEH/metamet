##' Add ERA5 reference data to a `metamet` object
##'
##' Reads ERA5 meteorological data and attaches it to a `metamet` object as
##' reference data (`dt_ref`). Converts accumulated precipitation to a rate,
##' renames variables, and ensures temporal alignment between observed and
##' reference data. Optionally restricts date ranges and harmonizes time
##' resolution with the main observations.
##'
##' @param mm A `metamet` object containing observation data and metadata.
##' @param fname_era5 Path to a CSV file containing ERA5 data.
##' @param restrict_ref_to_obs Logical. If `TRUE` (default), restricts reference data to the date range of observations.
##' @param restrict_obs_to_ref Logical. If `TRUE`, restricts observations to the date range of the reference data.
##' @param report_end_interval Logical. Should the time be reported at the end (`TRUE`, default) or start (`FALSE`) of the -averaging interval.
##' @param first_date Optional. Earliest date to include (overrides automatic date selection if set).
##' @param last_date Optional. Latest date to include (overrides automatic date selection if set).
##' @param extra_rows Integer. Extra rows to add when time-averaging for padding.
##'
##' @details
##' - Converts ERA5 precipitation ("tp") from a sum (in mm) to a rate (mm/s).
##' - Renames variables in ERA5 data to match main observations according to `dt_meta`.
##' - Optionally limits ERA5 data to match the observation period, and vice versa.
##' - Ensures reference and observed data tables have matching time resolutions,
##'   aggregating or disaggregating if needed.
##' - Warns if dimensions become mismatched.
##'
##' @return The `metamet` object `mm`, with processed ERA5 data available as `mm$dt_ref`.
##'
##' @seealso
##' - \code{\link{rename_era5}} for variable renaming
##' - \code{\link{time_average_dt}} for time-averaging implementation
##' - \code{\link{metamet}} for object construction
##'
##' @examples
##' # mm <- add_era5(mm)
##'
##' @export
add_era5 <- function(
  mm,
  fname_era5 = "data-raw/UK-AMO/dt_era5.csv",
  restrict_ref_to_obs = TRUE,
  restrict_obs_to_ref = FALSE,
  report_end_interval = TRUE,
  first_date = NULL,
  last_date = NULL,
  extra_rows = 2
) {
  # get era5 data to use as reference
  mm$dt_ref <- fread(fname_era5)
  # changing time resolution is a pain if we have a mix of averaged and summed
  # variables, so make all time-averaged per second.
  # convert precip "tp" in mm to a rate mm/s
  mm$dt_ref <- convert_sum_to_rate(
    mm$dt_ref,
    v_var_to_convert = "tp",
    time_name = "time"
  )
  # rename variables with names in dt
  mm <- rename_era5(mm)

  time_name <- mm$dt_meta[type == "time", name_dt]
  # make sure time series is complete without gaps in time variable
  mm$dt_ref <- pad_data(mm$dt_ref, time_name = time_name)

  # Do units work in timeAverage? Maybe set, drop and reset in time_average

  # restrict ref data to date range of obs
  if (restrict_ref_to_obs) {
    start_date <- min(mm$dt[, get(time_name)])
    end_date <- max(mm$dt[, get(time_name)])
    mm$dt_ref <- mm$dt_ref[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
  }

  # restrict obs data to date range of ref
  if (restrict_obs_to_ref) {
    start_date <- min(mm$dt_ref[, get(time_name)])
    end_date <- max(mm$dt_ref[, get(time_name)])
    mm$dt <- mm$dt[
      get(time_name) >= start_date & get(time_name) <= end_date
    ]
    # and if it exists, restrict qc to date range of ref
    if (!is.null(mm$dt_qc)) {
      mm$dt_qc <- mm$dt_qc[
        get(time_name) >= start_date & get(time_name) <= end_date
      ]
    }
  }

  # check the ref time resolution is the same as the mm$dt obs data
  interval_length_dt <- as.numeric(difftime(
    mm$dt[2, get(time_name)],
    mm$dt[1, get(time_name)],
    units = "secs"
  ))
  interval_length_ref <- as.numeric(difftime(
    mm$dt_ref[2, get(time_name)],
    mm$dt_ref[1, get(time_name)],
    units = "secs"
  ))

  if (interval_length_ref != interval_length_dt) {
    mm$dt_ref <- time_average_dt(
      mm$dt_ref,
      avg.time = paste(interval_length_dt, "sec"),
      first_date = min(mm$dt[, get(time_name)]),
      last_date = max(mm$dt[, get(time_name)]),
      time_name = time_name,
      wd_name = mm$dt_meta[type == "wind direction", name_dt],
      ws_name = mm$dt_meta[type == "wind speed" | type == "windspeed", name_dt],
      report_end_interval = report_end_interval,
      extra_rows = extra_rows
    )
  }

  dt_gaps <- detect_gaps(
    dt_in = mm$dt,
    expected_interval_mins = interval_length_dt / 60,
    time_name = time_name
  )
  dt_ref_gaps <- detect_gaps(
    dt_in = mm$dt_ref,
    expected_interval_mins = interval_length_dt / 60,
    time_name = time_name
  )
  if (nrow(dt_gaps) > 0) {
    stop("Gaps found in obs data, dt")
  }
  if (nrow(dt_ref_gaps) > 0) {
    stop("Gaps found in ref data, dt_ref")
  }

  # check that all are the same no. rows
  if (!identical(dim(mm$dt)[1], dim(mm$dt_ref)[1])) {
    print(paste(
      "Obs and ERA5 data tables do not have the same dimensions",
      dim(mm$dt),
      dim(mm$dt_ref)
    ))
  }

  return(mm)
}

##' Rename variables in ERA5 reference data to match observation names
##'
##' Renames columns in the ERA5 reference data table (`dt_ref`) within a
##' `metamet` object so they correspond to the column names in the primary
##' observation table (`dt`). The mapping is based on the `dt_meta` metadata table.
##'
##' @param mm A `metamet` object containing at least `dt`, `dt_meta`, `dt_site`,
##' and `dt_ref`.
##'
##' @details
##' - Sets the `site` column in `dt_ref` using the value from `dt_site`.
##' - Matches column names in `dt` to those in `dt_ref` using the
##'   `standard_name_era5` field in `dt_meta`.
##' - Reorders and renames columns in `dt_ref` to match the structure of `dt`.
##'
##' @return The updated `metamet` object `mm`, with ERA5 reference data columns
##' renamed to be consistent with observations.
##'
##' @seealso
##' - \code{\link{add_era5}} for populating ERA5 reference data
##'
##' @examples
##' # mm <- rename_era5(mm)
##'
##' @keywords internal
rename_era5 <- function(mm) {
  mm$dt_ref[, site := mm$dt_site$site]
  ind <- match(names(mm$dt), mm$dt_meta$name_dt)
  v_names_era5 <- mm$dt_meta$standard_name_era5[ind]
  mm$dt_ref <- mm$dt_ref[, ..v_names_era5]
  names(mm$dt_ref) <- names(mm$dt)
  return(mm)
}
