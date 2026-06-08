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
##' @return A \code{data.table} with column names exactly as written in the
##'   file header. No renaming is performed; the caller's \code{dt_meta} must
##'   use \code{name_dt} values that match the actual column names.
##'   Downstream metamet functions (\code{metamet_wide_to_long},
##'   \code{convert_time_char_to_posix}) normalise the time column to
##'   \code{TIMESTAMP} automatically based on \code{dt_meta}.
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
  data.table::fread(fname, ...)
}
