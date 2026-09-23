##' Read a Biral VPF750 Present Weather Sensor CSV file
##'
##' Reads a single VPF750 PWS file (or a yearly concatenation produced by
##' \code{concat_pws_by_year}). The format has three metadata header lines
##' followed by a column-header line and then data rows. Values carry embedded
##' unit suffixes (e.g. \code{"05.52 KM"}, \code{"+009.2 C"}) which are
##' stripped to leave bare numerics. The \code{"PC Time"} timestamp column is
##' converted from the sensor's backslash-separated format with milliseconds
##' (\code{"YYYY\\MM\\DD HH:MM:SS.ff"}) to \code{POSIXct} (UTC).
##'
##' Duplicate \code{"PC Time"} columns that sometimes appear in the raw file
##' are deduplicated automatically; only the first occurrence is kept.
##'
##' The returned data table uses the raw column names from the file header
##' (i.e. the sensor's \code{name_local} values). When the result is passed to
##' \code{\link{metamet}}, the constructor will rename \code{name_local} to
##' \code{name_dt} using the site \code{dt_meta} before restricting columns.
##'
##' @param fname Path to a single VPF750 CSV file or yearly concatenation.
##' @param ... Additional arguments forwarded to \code{data.table::fread},
##'   e.g. \code{fill}.
##'
##' @return A \code{data.table} with \code{"PC Time"} as a \code{POSIXct}
##'   column (UTC) and all measurement columns as \code{numeric}.
##'
##' @seealso \code{\link{read_obs_autodetect}}, \code{\link{metamet}}
##'
##' @examples
##' \dontrun{
##' dt <- read_pws_csv("data-raw/UK-AMO/pws/pws_2024.csv")
##' mm <- metamet(dt, dt_meta = dt_meta_amo, dt_site = dt_site, site_id = "UK-AMO")
##' }
##'
##' @export
read_pws_csv <- function(fname, ...) {
  stopifnot(length(fname) == 1L, is.character(fname), file.exists(fname))

  # Line 4 is the column header; lines 1-3 are instrument metadata
  header_line <- readLines(fname, n = 4L, warn = FALSE)[4L]
  col_names <- trimws(strsplit(header_line, ",")[[1L]])

  dt <- data.table::fread(
    fname,
    skip = 4L,
    header = FALSE,
    col.names = col_names,
    fill = TRUE,
    ...
  )

  # Deduplicate "PC Time" — the sensor occasionally writes it twice
  pc_idx <- which(names(dt) == "PC Time")
  if (length(pc_idx) > 1L) {
    dt[, (names(dt)[pc_idx[-1L]]) := NULL]
  }

  # Convert timestamp: "YYYY\MM\DD HH:MM:SS.ff" -> POSIXct UTC
  dt[, `PC Time` := gsub("\\\\", "-", `PC Time`)]
  dt[, `PC Time` := sub("\\.\\d+$", "", `PC Time`)]
  dt[, `PC Time` := as.POSIXct(`PC Time`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

  # Strip unit suffixes from character columns, e.g. "05.52 KM" -> 5.52
  char_cols <- setdiff(names(dt)[vapply(dt, is.character, logical(1L))], "PC Time")
  for (col in char_cols) {
    dt[[col]] <- suppressWarnings(as.numeric(
      gsub("^([+-]?\\d+\\.?\\d*).*", "\\1", dt[[col]])
    ))
  }

  dt
}
