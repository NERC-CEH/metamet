# path = "C:/Users/plevy/Documents/metamet/inst/extdata/UK-BUC/BushCabin_CR1000X_BUMON_30.dat"
#  path = "C:/Users/plevy/Documents/metamet/inst/extdata/UK-AMO/UK-AMo_BM_20260126_L03_F02.dat"
#  path = "C:/Users/plevy/Documents/metamet/inst/extdata/UK-AMO/UK-AMO_BM_dt_2026.csv"

#' @title read_obs_autodetect
#' @description Read a single observation file, auto-detecting whether it is a
#'   Campbell TOA5 `.dat` file or a plain CSV.  Dispatches to
#'   \code{\link{import_campbell_data}} for TOA5 and \code{data.table::fread}
#'   otherwise.
#' @param path Path to the file.
#' @param campbell_args Named list of extra arguments passed to
#'   \code{import_campbell_data}.
#' @param fread_args Named list of extra arguments passed to
#'   \code{data.table::fread}.
#' @return A \code{data.table}.
#' @export
read_obs_autodetect <- function(
  path,
  campbell_args = list(),
  fread_args = list()
) {
  stopifnot(length(path) == 1, is.character(path), file.exists(path))

  # Read just the first line
  first_line <- readLines(path, n = 1, warn = FALSE)
  if (length(first_line) == 0L) {
    stop("File is empty: ", path)
  }

  # Strip UTF-8 BOM if present (rare, but can break matching)
  first_line <- sub("^\ufeff", "", first_line)

  # TRUE if the line starts with TOA5 as the first CSV field,
  # allowing optional quotes and whitespace, and requiring a comma or EOL after it.
  is_toa5 <- grepl('^\\s*"?TOA5"?(\\s*,|\\s*$)', first_line)

  # Dispatch
  if (is_toa5) {
    do.call(import_campbell_data, c(list(path), campbell_args))
  } else {
    do.call(data.table::fread, c(list(input = path), fread_args))
  }
}
