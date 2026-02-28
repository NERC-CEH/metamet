##' Convert a summed variable to a rate
##'
##' Converts accumulated or summed meteorological variables (e.g., precipitation)
##' to a rate by dividing by the time interval between observations. This is useful
##' for standardizing variables recorded as cumulative sums to rate units (per second).
##'
##' @param dt A `data.table` containing the variables to convert. Modified by reference.
##' @param v_var_to_convert Character vector of column names in `dt` to convert
##'   from a sum to a rate. For example, `c("P_12_1_1", "LWS_4_1_2")`.
##'   If `NA` (default), no conversion is performed.
##' @param time_name Character string specifying the name of the time column in `dt`
##'   (e.g., `"DATECT"`). Used to calculate the interval length between the first
##'   two observations. If `NA` (default), an error will occur.
##'
##' @return The `data.table` `dt` with specified columns converted to rates
##'   (units per second). The input table is modified by reference and invisibly returned.
##'
##' @details
##' The function calculates the time interval from the first two observations
##' and divides all specified variables by this interval length (in seconds).
##' Units are converted from their original units to rates per second.
##'
##' Note: Unit attributes in the metadata are not automatically updated;
##' ensure metadata reflects the new rate units after conversion.
##'
##' @seealso
##'   \code{\link{add_era5}} for an application converting ERA5 precipitation
##'
#' @export
convert_sum_to_rate <- function(
  dt,
  v_var_to_convert = NA, # c("P_12_1_1", "LWS_4_1_2"),
  time_name = NA # "DATECT"
) {
  interval_length_s <- as.numeric(difftime(
    dt[2, get(time_name)],
    dt[1, get(time_name)],
    units = "secs"
  ))
  dt[,
    eval(v_var_to_convert) := lapply(.SD, FUN = function(x) {
      x / interval_length_s
    }),
    .SDcols = v_var_to_convert
  ]
  # the units are changed from whatever they were initially to a rate x / secs
  # add proper units handling later
  return((dt))
}
