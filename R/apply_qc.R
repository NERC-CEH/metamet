##' Apply Quality Control Checks to Meteorological Data
##'
##' Applies quality control procedures to a `metamet` object by removing
##' out-of-range values and creating a quality code table. The function flags
##' missing or invalid values that fall outside acceptable ranges defined in
##' the metadata.
##'
##' The function performs the following steps:
##' \enumerate{
##'   \item Removes values outside the min/max range specified in `dt_meta`
##'   \item Creates a quality control table (`dt_qc`) where 1 indicates a missing
##'         or out-of-range value, and 0 indicates a valid value
##'   \item Adds a validator field marked as "auto" (to be replaced by username
##'         when manually validated)
##' }
##'
##' @param mm0 A `metamet` object containing at least `dt` (data table),
##'   `dt_meta` (metadata table with min/max ranges), and `dt_site` (site
##'   information).
##'
##' @return A modified `metamet` object with:
##'   \item{dt}{Data values, with out-of-range values set to NA}
##'   \item{dt_qc}{New quality control table (1 = invalid/missing, 0 = valid)}
##'   All other fields from the input object are preserved.
##'
##' @examples
##' # mm_qc <- apply_qc(mm)
##'
##' @seealso
##' \code{\link{remove_out_of_range}} for the range-checking implementation
##'
##' @export
apply_qc <- function(mm0) {
  mm <- copy(mm0)
  # setindex needed to fix odd bug in data.table described here:
  # https://github.com/Rdatatable/data.table/issues/3745
  data.table::setindex(mm$dt_meta, type)

  time_name <- mm$dt_meta[type == "time", name_dt]
  v_cols_to_exclude <- c("site", time_name)
  v_cols <- names(mm$dt)[names(mm$dt) %!in% v_cols_to_exclude]

  # set improper values in dt to NA (missing)
  mm <- remove_out_of_range(mm, v_cols)
  # add other QC checks as needed e.g. comparison with ref data or stats

  # After all criteria applied, update dt_qc, the table holding the codes.
  # Use as.numeric(is.na()) to set qc to 1 if missing or 0 if not.
  mm$dt_qc <- copy(mm$dt)
  mm$dt_qc[,
    eval(v_cols) := lapply(.SD, function(x) as.numeric(is.na(x))),
    .SDcols = v_cols
  ]
  # add name of validator - later replaced by username when manually checked
  mm$dt_qc[, validator := "auto"]
  return(mm)
}

remove_out_of_range <- function(mm, v_cols = names(mm$dt)) {
  # get the range_min/max from these variable names
  range_filter <- function(x, v_range) {
    x[x < v_range[1] | x > v_range[2]] <- NA
    return(x)
  }

  # loop over each relevant column, finding the range and setting to NA outside
  for (i in seq_along(v_cols)) {
    v_range <- mm$dt_meta[which(name_dt == v_cols[i]), c(range_min, range_max)]
    mm$dt[, v_cols[i] := range_filter(get(v_cols[i]), v_range)]
  }
  return(mm)
}

# a possible lapply version is not as simple as:
# mm$dt[, v_cols := lapply(.SD, range_filter, v_range), .SDcols = v_cols]
# need to make m_range somehow ~
# ind <- match(mm$dt_meta$name_dt, v_cols)
