##' Read a BADC-CSV (CEDA) formatted meteorological file
##'
##' Parses files conforming to the BADC-CSV 1.0 convention used by the UK
##' Centre for Environmental Data Analysis (CEDA). These files have a variable
##' number of header rows describing each column by number, followed by a
##' column-index row (`1,2,3,...,N`) and then the data.
##'
##' Column names are taken from `short_name` metadata rows where available,
##' falling back to `long_name`. The first column is always renamed
##' `"TIMESTAMP"` and parsed to POSIXct (UTC). Flag columns (names beginning
##' with `"Flag"` or `"FLAG"`) are dropped by default. Redundant packed
##' datetime columns in `YYYYMMDDHHMM` format are dropped automatically.
##' Missing-value sentinels (`-9999`, empty fields) are converted to `NA`.
##'
##' @param fname Path to the BADC-CSV file.
##' @param drop_flags Logical (default `TRUE`). Drop flag columns whose names
##'   begin with `"Flag"` or `"FLAG"`.
##'
##' @return A `data.table` with a `TIMESTAMP` column (POSIXct, UTC) and one
##'   numeric column per measurement variable.
##'
##' @examples
##' \dontrun{
##' dt <- read_ceda_csv(pkg_extdata("UK-AMO/historical/ceda/AU_MetData_2023.csv"))
##' dt <- read_ceda_csv(pkg_extdata("UK-BUC/historical/ceda/BushCabin_2024.csv"))
##' }
##'
##' @export
read_ceda_csv <- function(fname, drop_flags = TRUE) {
  lines <- readLines(fname, warn = FALSE, encoding = "latin1")

  # ---- Find the column-index row -------------------------------------------
  # It is the first row whose non-empty comma-separated fields are exactly the
  # consecutive integers 1, 2, 3, ... (distinguishes it from flag_values rows
  # which use non-sequential codes).
  col_index_row <- NA_integer_
  for (i in seq_along(lines)) {
    parts <- trimws(strsplit(lines[[i]], ",")[[1L]])
    parts <- parts[nzchar(parts)]
    vals <- suppressWarnings(as.integer(parts))
    if (
      !anyNA(vals) && length(vals) >= 2L && identical(vals, seq_along(vals))
    ) {
      col_index_row <- i
      n_cols <- max(vals)
      break
    }
  }
  if (is.na(col_index_row)) {
    stop(
      "Cannot find column-index row in '",
      basename(fname),
      "'. ",
      "Is this a BADC-CSV file?",
      call. = FALSE
    )
  }

  # ---- Parse header rows for column names ----------------------------------
  # Only short_name / long_name rows are needed; simple strsplit is sufficient
  # because those fields never contain commas in practice.
  col_short <- list()
  col_long <- list()

  for (line in lines[seq_len(col_index_row - 1L)]) {
    parts <- trimws(strsplit(line, ",")[[1L]])
    if (length(parts) < 3L) {
      next
    }

    field <- tolower(parts[[1L]])
    col_id <- parts[[2L]]
    if (toupper(col_id) == "G") {
      next
    }
    col_num <- suppressWarnings(as.integer(col_id))
    if (is.na(col_num)) {
      next
    }

    value <- gsub('^"+|"+$', "", parts[[3L]])
    key <- as.character(col_num)

    if (field == "short_name" && is.null(col_short[[key]])) {
      col_short[[key]] <- value
    } else if (field == "long_name" && is.null(col_long[[key]])) {
      col_long[[key]] <- value
    }
  }

  # ---- Build column name vector --------------------------------------------
  col_names <- vapply(
    seq_len(n_cols),
    function(i) {
      key <- as.character(i)
      sn <- col_short[[key]]
      if (length(sn) == 1L && nzchar(sn)) {
        return(sn)
      }
      ln <- col_long[[key]]
      if (length(ln) == 1L && nzchar(ln)) {
        return(ln)
      }
      paste0("V", i)
    },
    character(1L)
  )

  col_names[[1L]] <- "TIMESTAMP"
  col_names <- make.unique(col_names, sep = ".")

  # ---- Read data -----------------------------------------------------------
  dt <- data.table::fread(
    fname,
    skip = col_index_row,
    header = FALSE,
    na.strings = c("", "NA", "-9999"),
    fill = TRUE,
    showProgress = FALSE
  )

  # Trim to the expected number of columns and assign names
  keep <- seq_len(min(ncol(dt), n_cols))
  dt <- dt[, keep, with = FALSE]
  data.table::setnames(dt, col_names[keep])

  # ---- Drop flag columns ---------------------------------------------------
  if (drop_flags) {
    flag_cols <- grep("^(Flag|FLAG)\\b", names(dt), value = TRUE)
    if (length(flag_cols)) dt[, (flag_cols) := NULL]
  }

  # ---- Drop redundant packed-datetime columns (YYYYMMDDHHMM) ---------------
  # These are numeric columns whose values fall in the plausible YYYYMMDDHHMM
  # range (198001010000 – 209912312359) — unambiguously not a measurement.
  for (col in setdiff(names(dt), "TIMESTAMP")) {
    vals <- dt[[col]]
    if (!is.numeric(vals)) {
      next
    }
    non_na <- vals[!is.na(vals)]
    if (
      length(non_na) > 0L &&
        all(non_na >= 198001010000 & non_na <= 209912312359)
    ) {
      dt[, (col) := NULL]
    }
  }

  # ---- Parse TIMESTAMP to POSIXct ------------------------------------------
  # parse_date_time with multiple orders uses a training heuristic that can
  # misidentify 4-digit years (e.g. "01/01/2024 00:30" parsed as dmy HMS with
  # y=20, H=24). Try explicit strptime formats individually instead.
  dt[,
    TIMESTAMP := {
      ts <- TIMESTAMP
      result <- as.POSIXct(rep(NA_real_, .N), tz = "UTC")
      for (fmt in c(
        "%d/%m/%y %H:%M",
        "%d/%m/%Y %H:%M",
        "%d/%m/%y %H:%M:%S",
        "%d/%m/%Y %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d %H:%M:%S"
      )) {
        fill <- is.na(result) & !is.na(ts)
        if (!any(fill)) {
          break
        }
        parsed <- as.POSIXct(strptime(ts[fill], fmt, tz = "UTC"))
        ok <- !is.na(parsed)
        result[which(fill)[ok]] <- parsed[ok]
      }
      result
    }
  ]

  # Drop rows with no parseable timestamp (end_data marker, empty trailing rows)
  dt <- dt[!is.na(TIMESTAMP)]

  # ---- Coerce remaining character columns to numeric -----------------------
  char_cols <- names(dt)[vapply(dt, is.character, logical(1L))]
  char_cols <- setdiff(char_cols, "TIMESTAMP")
  if (length(char_cols)) {
    dt[,
      (char_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
      .SDcols = char_cols
    ]
  }

  dt[]
}
