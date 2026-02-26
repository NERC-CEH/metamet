#' Change the naming convention of measurement variables
#'
#' Updates the column names in `mm$dt`, `mm$dt_ref`, and `mm$dt_qc` based on a
#' naming convention stored in `mm$dt_meta`. The function replaces the existing
#' `name_dt` values with new names constructed from the specified naming
#' convention and available ID suffixes.
#'
#' @param mm_in A metamet object containing at least:
#'   * `dt` — a data.table of measurement values.
#'   * `dt_meta` — a data.table with metadata, including `name_dt`,
#'     the naming convention column (e.g., `"name_era5"`), and ID columns
#'     (`horizontal_id`, `vertical_id`, `replicate_id`).
#'   * `dt_ref` — optional reference data.table whose column names should also be updated.
#'   * `dt_qc` — optional quality-control data.table whose column names should also be updated.
#'
#' @param name_convention A character string giving the column in `dt_meta`
#'   that contains the base names to use (default: `"name_era5"`).
#'
#' @return The modified `mm_in` object with updated column names in all relevant
#'   data.tables and updated `name_dt` values in `dt_meta`.
#'
#' @details
#' The function:
#'   * Ensures ID columns are stored as integers.
#'   * Builds new variable names from the chosen naming convention.
#'   * Appends ID suffixes (`horizontal_id`, `vertical_id`, `replicate_id`)
#'     when `replicate_id` is not `NA`.
#'   * Applies the new names consistently across `dt`, `dt_ref`, and `dt_qc`
#'     when present.
#'
#' This is used for harmonising naming schemes across datasets or switching
#' between naming conventions (e.g., ERA5 vs. model-specific names).
#'
#' @examples
#' \dontrun{
#' mm <- change_naming_convention(mm, name_convention = "name_era5")
#' }
#'
#' @export
change_naming_convention <- function(mm_in, name_convention = "name_era5") {
  mm <- copy(mm_in)

  v_cols_id <- c("horizontal_id", "vertical_id", "replicate_id")
  v_cols <- c(name_convention, "horizontal_id", "vertical_id", "replicate_id")

  mm$dt_meta[, eval(v_cols_id) := lapply(.SD, as.integer), .SDcols = v_cols_id]

  mm$dt_meta[, new_names := get(name_convention)]

  mm$dt_meta[
    !is.na(replicate_id),
    new_names := do.call(paste, c(.SD, sep = "_")),
    .SDcols = v_cols
  ]

  setnames(
    mm$dt,
    mm$dt_meta[, name_dt],
    mm$dt_meta[, new_names]
  )

  if (!is.null(mm$dt_qc)) {
    setnames(
      mm$dt_ref,
      mm$dt_meta[, name_dt],
      mm$dt_meta[, new_names]
    )
  }

  if (!is.null(mm$dt_qc)) {
    setnames(
      mm$dt_qc,
      mm$dt_meta[, name_dt],
      mm$dt_meta[, new_names]
    )
  }

  mm$dt_meta[, name_dt := new_names]

  return(mm)
}
