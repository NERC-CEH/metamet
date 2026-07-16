#' Format the data for submission to CEDA
#'
#' @param mm A metamet object (wide or long format).
#' @param v_names A character vector of column names
#'   that should be included in the new data frame.
#'
#' @return A new data frame with the selected columns from \code{dt} and
#'   \code{dt_qc}.
#'
##' @keywords internal
##' @noRd
format_for_ceda <- function(
  mm,
  v_names = NULL # if NULL, derive from mm$dt
) {
  mm <- .ensure_wide(mm)

  v_time <- unique(mm$dt_meta[type == "time", name_dt])

  # derive variable names dynamically if not supplied
  dt_cols <- names(mm$dt)

  if (is.null(v_names)) {
    if ("var_name" %in% dt_cols && "name_icos" %in% dt_cols) {
      # LONG format
      v_names <- unique(mm$dt$name_icos)
    } else {
      # WIDE format: variables are column names except site + time cols
      v_names <- setdiff(dt_cols, c("site", v_time))
    }
  }

  # Only keep v_names columns that actually exist in dt
  v_names_present <- intersect(v_names, names(mm$dt))
  by_cols <- c("site", v_time)
  dt <- mm$dt[, c(by_cols, v_names_present), with = FALSE]

  if (!is.null(mm$dt_qc)) {
    # Only keep qc columns for variables in v_names; dt_qc may have fewer rows
    # than dt (only timestamps with non-NA QC codes), so join rather than cbind
    v_qc_present <- intersect(v_names_present, names(mm$dt_qc))
    if (length(v_qc_present) > 0L) {
      dt_qc_sel <- mm$dt_qc[, c(by_cols, v_qc_present), with = FALSE]
      data.table::setnames(dt_qc_sel, v_qc_present, paste0(v_qc_present, "_qc"))
      dt <- merge(dt, dt_qc_sel, by = by_cols, all.x = TRUE)
    }
  }

  return(dt)
}
