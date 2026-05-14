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
  v_lines <- readLines(fname, warn = FALSE, encoding = "latin1")

  # ---- Find the column-index row -------------------------------------------
  # It is the first row whose non-empty comma-separated fields are exactly the
  # consecutive integers 1, 2, 3, ... (distinguishes it from flag_values rows
  # which use non-sequential codes).
  i_col_index_row <- NA_integer_
  for (i in seq_along(v_lines)) {
    v_parts <- trimws(strsplit(v_lines[[i]], ",")[[1L]])
    v_parts <- v_parts[nzchar(v_parts)]
    v_vals <- suppressWarnings(as.integer(v_parts))
    if (
      !anyNA(v_vals) &&
        length(v_vals) >= 2L &&
        identical(v_vals, seq_along(v_vals))
    ) {
      i_col_index_row <- i
      n_cols <- max(v_vals)
      break
    }
  }
  if (is.na(i_col_index_row)) {
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
  l_col_short <- list()
  l_col_long <- list()

  for (line in v_lines[seq_len(i_col_index_row - 1L)]) {
    v_parts <- trimws(strsplit(line, ",")[[1L]])
    if (length(v_parts) < 3L) {
      next
    }

    field <- tolower(v_parts[[1L]])
    col_id <- v_parts[[2L]]
    if (toupper(col_id) == "G") {
      next
    }
    i_col <- suppressWarnings(as.integer(col_id))
    if (is.na(i_col)) {
      next
    }

    value <- gsub('^"+|"+$', "", v_parts[[3L]])
    key <- as.character(i_col)

    if (field == "short_name" && is.null(l_col_short[[key]])) {
      l_col_short[[key]] <- value
    } else if (field == "long_name" && is.null(l_col_long[[key]])) {
      l_col_long[[key]] <- value
    }
  }

  # ---- Build column name vector --------------------------------------------
  v_col_names <- vapply(
    seq_len(n_cols),
    function(i) {
      key <- as.character(i)
      sn <- l_col_short[[key]]
      if (length(sn) == 1L && nzchar(sn)) {
        return(sn)
      }
      ln <- l_col_long[[key]]
      if (length(ln) == 1L && nzchar(ln)) {
        return(ln)
      }
      paste0("V", i)
    },
    character(1L)
  )

  v_col_names[[1L]] <- "TIMESTAMP"
  v_col_names <- make.unique(v_col_names, sep = ".")

  # ---- Read data -----------------------------------------------------------
  dt <- data.table::fread(
    fname,
    skip = i_col_index_row,
    header = FALSE,
    na.strings = c("", "NA", "-9999"),
    fill = TRUE,
    showProgress = FALSE
  )

  # Trim to the expected number of columns and assign names
  v_keep <- seq_len(min(ncol(dt), n_cols))
  dt <- dt[, v_keep, with = FALSE]
  data.table::setnames(dt, v_col_names[v_keep])

  # ---- Drop flag columns ---------------------------------------------------
  if (drop_flags) {
    v_flag_cols <- grep("^(Flag|FLAG)\\b", names(dt), value = TRUE)
    if (length(v_flag_cols)) dt[, (v_flag_cols) := NULL]
  }

  # ---- Drop redundant packed-datetime columns (YYYYMMDDHHMM) ---------------
  # These are numeric columns whose values fall in the plausible YYYYMMDDHHMM
  # range (198001010000 – 209912312359) — unambiguously not a measurement.
  for (col in setdiff(names(dt), "TIMESTAMP")) {
    v_vals <- dt[[col]]
    if (!is.numeric(v_vals)) {
      next
    }
    v_non_na <- v_vals[!is.na(v_vals)]
    if (
      length(v_non_na) > 0L &&
        all(v_non_na >= 198001010000 & v_non_na <= 209912312359)
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
      v_ts <- TIMESTAMP
      v_result <- as.POSIXct(rep(NA_real_, .N), tz = "UTC")
      for (fmt in c(
        "%d/%m/%y %H:%M",
        "%d/%m/%Y %H:%M",
        "%d/%m/%y %H:%M:%S",
        "%d/%m/%Y %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d %H:%M:%S"
      )) {
        v_fill <- is.na(v_result) & !is.na(v_ts)
        if (!any(v_fill)) {
          break
        }
        v_parsed <- as.POSIXct(strptime(v_ts[v_fill], fmt, tz = "UTC"))
        v_ok <- !is.na(v_parsed)
        v_result[which(v_fill)[v_ok]] <- v_parsed[v_ok]
      }
      v_result
    }
  ]

  # Drop rows with no parseable timestamp (end_data marker, empty trailing rows)
  dt <- dt[!is.na(TIMESTAMP)]

  # ---- Coerce remaining character columns to numeric -----------------------
  v_char_cols <- names(dt)[vapply(dt, is.character, logical(1L))]
  v_char_cols <- setdiff(v_char_cols, "TIMESTAMP")
  if (length(v_char_cols)) {
    dt[,
      (v_char_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
      .SDcols = v_char_cols
    ]
  }

  dt[]
}
