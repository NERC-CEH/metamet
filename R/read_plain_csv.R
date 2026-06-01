##' Read a plain-header CSV meteorological file
##'
##' Reads a standard CSV file in which the first row is a column header. The
##' only preprocessing performed is normalising the timestamp column name:
##' any column whose name matches \code{"timestamp"} (case-insensitive) is
##' renamed to \code{"TIMESTAMP"} so that downstream metamet functions can
##' locate it consistently.
##'
##' This reader is the appropriate choice for plain-CSV exports such as EIDC
##' data packages, where there is no multi-row BADC-CSV header and no Campbell
##' TOA5 instrument-info preamble.
##'
##' @param fname Path to the CSV file.
##' @param ... Additional arguments forwarded to \code{data.table::fread},
##'   e.g. \code{sep}, \code{na.strings}.
##'
##' @return A \code{data.table} with a \code{TIMESTAMP} column (still in its
##'   original character or numeric class — conversion to POSIXct happens
##'   inside \code{\link{metamet}} via \code{time_char_format} in
##'   \code{dt_meta}).
##'
##' @seealso \code{\link{read_ceda_csv}}, \code{\link{read_obs_autodetect}}
##'
##' @examples
##' \dontrun{
##' dt <- read_plain_csv(
##'   pkg_extdata("UK-WHM/historical/eidc/whim_met_2002_2023.csv")
##' )
##' }
##'
##' @export
read_plain_csv <- function(fname, ...) {
  dt <- data.table::fread(fname, ...)
  # Normalise timestamp column name to TIMESTAMP (case-insensitive match)
  ts_col <- names(dt)[tolower(names(dt)) == "timestamp"]
  if (length(ts_col) == 1L && ts_col != "TIMESTAMP") {
    data.table::setnames(dt, ts_col, "TIMESTAMP")
  }
  dt
}
