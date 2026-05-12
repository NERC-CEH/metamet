##' Apply Quality Control Checks to Meteorological Data
##'
##' Applies range-based quality control to a `metamet` object by setting
##' out-of-range values to `NA` and recording QC codes in the `qc` column.
##' Accepts both wide and long format; always returns long format.
##'
##' The function performs the following steps:
##' \enumerate{
##'   \item Reshapes to long format if necessary.
##'   \item Sets values outside the min/max range specified in `dt_meta` to `NA`.
##'   \item Sets `qc = 1` for missing or invalid values, `qc = 0` for valid values.
##'   \item Sets `validator = "auto"` for flagged rows.
##' }
##'
##' @param mm0 A `metamet` object (wide or long format).
##'
##' @return A long-format `metamet` object with updated `value`, `qc`, and
##'   `validator` columns.
##'
##' @examples
##' # mm_qc <- apply_qc(mm)
##'
##' @export
apply_qc <- function(mm0) {
  mm <- .ensure_long(data.table::copy(mm0))

  # Attach per-variable range limits from dt_meta via var_name
  ranges <- unique(mm$dt_meta[, .(name_dt, range_min, range_max)])
  mm$dt[
    ranges,
    c(".rmin", ".rmax") := .(range_min, range_max),
    on = c(var_name = "name_dt")
  ]

  # Set out-of-range values to NA
  mm$dt[
    !is.na(.rmin) & !is.na(value) & (value < .rmin | value > .rmax),
    value := NA_real_
  ]
  mm$dt[, c(".rmin", ".rmax") := NULL]

  # qc: 1 = missing or invalid, 0 = valid
  mm$dt[, qc := as.numeric(is.na(value))]
  mm$dt[qc == 1L, validator := "auto"]
  mm$dt[qc == 0L, validator := NA_character_]

  mm
}
