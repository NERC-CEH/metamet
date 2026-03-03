#' Reshape a metamet object from wide to long format
#'
#' This function converts the component data tables of a metamet (`mm`)
#' object—`dt`, `dt_qc`, and `dt_ref`—from wide format to long format using
#' `data.table::melt()`. It merges QC flags, reference values, and metadata
#' into a single long-format `dt` table and removes the now-redundant
#' `dt_qc` and `dt_ref` components.
#'
#' @param mm A metamet object (a list-like structure) containing at least
#'   the elements `dt`, `dt_qc`, `dt_ref`, and `dt_meta`. Each component
#'   must be a `data.table` with expected columns:
#'   * `dt`: `site`, `TIMESTAMP`, measurement columns
#'   * `dt_qc`: `site`, `TIMESTAMP`, `validator`, QC columns
#'   * `dt_ref`: `site`, `TIMESTAMP`, reference columns
#'   * `dt_meta`: includes `site`, `name_dt`, `type`, `name_icos`
#'
#' @return The modified metamet object, where:
#'   * `mm$dt` is now a long-format table containing measurement values,
#'     QC flags, validator names, reference values, and metadata.
#'   * `mm$dt_qc` and `mm$dt_ref` are set to `NULL` because their
#'     content has been merged into `mm$dt`.
#'
#' @details
#' Internally, the function:
#'   1. Melts each data table to long format.
#'   2. Sets keys and performs keyed merges for efficient joining.
#'   3. Annotates `mm$dt` with QC, reference, and metadata fields.
#'
#' This function is designed for harmonising metamet objects before
#' downstream validation and processing.
#'
#' @examples
#' \dontrun{
#' mm_long <- reshape_wide_to_long(mm)
#' }
#'
#' @export
reshape_wide_to_long <- function(mm) {
  mm$dt <- melt(
    mm$dt,
    id.vars = c("site", "TIMESTAMP"),
    variable.name = "var_name"
  )
  dt_qc <- melt(
    mm$dt_qc,
    id.vars = c("site", "TIMESTAMP", "validator"),
    variable.name = "var_name",
    value.name = "qc"
  )
  dt_ref <- melt(
    mm$dt_ref,
    id.vars = c("site", "TIMESTAMP"),
    variable.name = "var_name",
    value.name = "ref"
  )

  key_cols <- c("site", "TIMESTAMP", "var_name")
  setkeyv(mm$dt, key_cols)
  setkeyv(dt_qc, key_cols)
  setkeyv(dt_ref, key_cols)
  setkeyv(mm$dt_meta, c("site", "name_dt"))

  mm$dt <- mm$dt[
    mm$dt_meta,
    `:=`(type = type, name_icos = name_icos),
    on = c(var_name = "name_dt")
  ]
  mm$dt <- mm$dt[dt_qc, `:=`(qc = qc, validator = validator)]
  mm$dt <- mm$dt[dt_ref, ref := ref]

  # return a reformatted metamet object with long format dt containing all info
  # from mm$dt_qc and mm$dt_ref, so these are no longer needed
  mm$dt_qc <- NULL
  mm$dt_ref <- NULL
  return(mm)
}

#' Combine lists of metamet data tables into a single metamet object
#'
#' This function binds lists of data tables (`l_dt`, `l_dt_meta`,
#' `l_dt_site`) using `data.table::rbindlist()` and inserts the combined
#' tables into an existing metamet object (`mm`). It is typically used to
#' merge metamet data from multiple sites or time periods.
#'
#' @param mm A metamet object into which combined tables will be stored.
#'   The object should contain (or be able to contain) elements `dt`,
#'   `dt_meta`, and `dt_site`.
#'
#' @param l_dt A list of data tables to be row-bound into `mm$dt`.
#' @param l_dt_meta A list of metadata tables to be row-bound into `mm$dt_meta`.
#' @param l_dt_site A list of site information tables to be row-bound into
#'   `mm$dt_site`.
#'
#' @return The modified metamet object with combined `dt`, `dt_meta`,
#'   and `dt_site` components.
#'
#' @examples
#' \dontrun{
#' mm <- rbind_metamet(
#'   mm,
#'   l_dt = list(dt1, dt2),
#'   l_dt_meta = list(meta1, meta2),
#'   l_dt_site = list(site1, site2)
#' )
#' }
#'
#' @export
rbind_metamet <- function(mm, l_dt, l_dt_meta, l_dt_site) {
  mm$dt <- rbindlist(l_dt)
  mm$dt_meta <- rbindlist(l_dt_meta)
  mm$dt_site <- rbindlist(l_dt_site)
  return(mm)
}
