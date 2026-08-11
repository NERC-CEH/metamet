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

  # Build a per-variable statistic map:
  # - If a `statistic` column exists in dt_meta use it.
  # - Otherwise default: precipitation: "sum", others: "mean".
  v_meta <- mm$dt_meta
  v_all_names <- intersect(v_meta$name_dt, colnames(mm$dt))
  if ("statistic" %in% names(v_meta)) {
    stats_map <- setNames(as.character(v_meta$statistic), v_meta$name_dt)
  } else {
    stats_map <- character(0)
  }

  # helper to get statistic for a variable name
  get_stat_for <- function(varname) {
    if (
      length(stats_map) > 0L &&
        varname %in% names(stats_map) &&
        !is.na(stats_map[[varname]]) &&
        nzchar(stats_map[[varname]])
    ) {
      stats_map[[varname]]
    } else if (
      !is.null(precip_name) &&
        length(precip_name) > 0L &&
        varname %in% precip_name
    ) {
      "sum"
    } else {
      "mean"
    }
  }

  # Helper to average a wide-ish data.table according to per-variable statistics.
  # Returns an averaged data.table with site + time_name + averaged cols.
  average_wide_by_stat <- function(
    dt_in,
    time_name_local,
    wd_nm,
    ws_nm,
    fill_na = FALSE
  ) {
    dt_cols <- intersect(
      colnames(dt_in),
      c(time_name_local, "site", v_all_names)
    )
    # remove timestamp and site from set of variable names to group by statistic
    var_cols <- setdiff(dt_cols, c(time_name_local, "site"))
    if (length(var_cols) == 0L) {
      # nothing to average, return trimmed dt_in with site+time only (but ensure no duplicate times)
      out <- unique(dt_in[, ..c("site", time_name_local)])
      return(out)
    }

    # Group variable names by required statistic
    v_stats_for_vars <- vapply(
      var_cols,
      get_stat_for,
      FUN.VALUE = character(1),
      USE.NAMES = TRUE
    )
    stats_unique <- unique(v_stats_for_vars)

    l_out <- list()
    for (st in stats_unique) {
      vars_for_st <- names(v_stats_for_vars)[v_stats_for_vars == st]
      cols_to_use <- c("site", time_name_local, vars_for_st)
      dt_sub <- dt_in[, ..cols_to_use]

      # determine whether to pass wd/ws - only if both present and match exactly
      wd_pass <- if (
        !is.null(wd_nm) &&
          length(wd_nm) == 1L &&
          !is.na(wd_nm) &&
          wd_nm %in% vars_for_st
      ) {
        wd_nm
      } else {
        NULL
      }
      ws_pass <- if (
        !is.null(ws_nm) &&
          length(ws_nm) == 1L &&
          !is.na(ws_nm) &&
          ws_nm %in% vars_for_st
      ) {
        ws_nm
      } else {
        NULL
      }

      dt_avg <- time_average_dt(
        dt_sub,
        avg.time = avg.time,
        statistic = st,
        first_date = min(mm$dt[, get(time_name)]),
        last_date = max(mm$dt[, get(time_name)]),
        time_name = time_name_local,
        wd_name = wd_pass,
        ws_name = ws_pass,
        report_end_interval = report_end_interval,
        extra_rows = extra_rows,
        fill_na = fill_na
      )
      # ensure consistent column order
      data.table::setcolorder(
        dt_avg,
        c(
          "site",
          time_name_local,
          setdiff(colnames(dt_avg), c("site", time_name_local))
        )
      )
      l_out[[st]] <- dt_avg
    }

    # merge result tables on site + time_name_local
    if (length(l_out) == 1L) {
      return(l_out[[1]])
    }
    dt_merged <- Reduce(
      function(x, y) merge(x, y, by = c("site", time_name_local), all = TRUE),
      l_out
    )
    # order by time
    data.table::setorderv(dt_merged, time_name_local)
    return(dt_merged)
  }

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
      # long-format dt_qc already; time-average as-is using median (preserve existing behaviour)
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

  # Average dt_ref (reference data) using per-variable statistic when provided,
  # falling back to defaults. ERA5 is hourly so fill_na = TRUE is useful.
  if (!is.null(mm$dt_ref)) {
    mm$dt_ref <- average_wide_by_stat(
      mm$dt_ref,
      time_name,
      wd_name,
      ws_name,
      fill_na = TRUE
    )
  }

  # do dt last as first_date/last_date are based on this and we do not want to
  # use the time-averaged start/end
  mm$dt <- average_wide_by_stat(
    mm$dt,
    time_name,
    wd_name,
    ws_name,
    fill_na = FALSE
  )

  return(mm)
}
