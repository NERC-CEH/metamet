##' Read Campbell CR23X .dat files using .dld column definitions
##'
##' Parses the headerless comma-separated data files produced by Campbell
##' Scientific CR23X dataloggers, using the accompanying Final Storage Label
##' (.dld) file to assign column names. Multiple output tables with different
##' sampling intervals coexist in the same .dat file; each is returned as a
##' separate element of a named list.
##'
##' The two \code{_RTM} (Real-Time) columns in each output-table definition are
##' treated as timestamp components: the first is day-of-year (1--366) and the
##' second is time of day in HHMM format (e.g. 1015 = 10:15). A
##' \code{TIMESTAMP} (POSIXct, UTC) column is synthesised from these plus the
##' year.  Year-rollover within a file (day-of-year decreasing from ~365 to 1)
##' is detected automatically.
##'
##' When \code{year} is not supplied it is inferred from the filename pattern
##' \code{_YYYY_MM_DD}: if the first data row's day-of-year is > 300 and the
##' filename month is <= 3, the year is decremented by one (files downloaded in
##' early January commonly contain data from late December of the prior year).
##'
##' Output-table definitions are matched to data rows first by the table
##' identifier in column 1, then by column count as a fallback (used when the
##' logger program has been revised and the table number changed since the .dld
##' was generated).
##'
##' @param fname_dat Path to the Campbell .dat file.
##' @param fname_dld Path to the corresponding .dld (Final Storage Label) file.
##' @param year Integer. Calendar year for the first data row. If \code{NULL}
##'   (default) it is inferred from the filename as described above.
##' @param table_id Integer or character. If supplied, return only the
##'   \code{data.table} for this table identifier. Otherwise a named list of
##'   \code{data.table}s is returned, one per table found in the file.
##'
##' @return A named list of \code{data.table}s (names are the table identifiers
##'   as they appear in column 1 of the .dat file), each with a leading
##'   \code{TIMESTAMP} (POSIXct, UTC) column followed by the measurement
##'   variables named from the .dld.  A single \code{data.table} is returned
##'   when \code{table_id} is specified.
##'
##' @examples
##' \dontrun{
##' tables <- read_old_campbell_dat(
##'   pkg_extdata("UK-WHM/current/whim_met_2026_01_05.dat"),
##'   pkg_extdata("UK-WHM/current/Whim23X260609.dld")
##' )
##' dt_15min <- tables[["120"]] # 15-minute output table
##' dt_1min <- tables[["227"]] # 1-minute output table
##' }
##'
##' @export
read_old_campbell_dat <- function(
  fname_dat,
  fname_dld,
  year = NULL,
  table_id = NULL
) {
  # ---- Parse DLD definitions -----------------------------------------------
  l_dld_tables <- .parse_campbell_dld(fname_dld)
  if (!length(l_dld_tables)) {
    stop(
      "No Output_Table definitions found in '",
      basename(fname_dld),
      "'.",
      call. = FALSE
    )
  }
  v_dld_ncols <- vapply(l_dld_tables, length, integer(1L))

  # ---- Read data lines -----------------------------------------------------
  v_lines <- readLines(fname_dat, warn = FALSE)
  v_lines <- v_lines[nzchar(trimws(v_lines))]
  if (!length(v_lines)) {
    return(list())
  }

  v_table_ids <- sub(",.*", "", v_lines)

  # ---- Determine starting year ---------------------------------------------
  if (is.null(year)) {
    v_date_ch <- regmatches(
      basename(fname_dat),
      regexpr("\\d{4}_\\d{2}_\\d{2}", basename(fname_dat))
    )
    if (!length(v_date_ch)) {
      stop(
        "Cannot infer year from '",
        basename(fname_dat),
        "'; supply `year` explicitly.",
        call. = FALSE
      )
    }
    v_date_int <- as.integer(strsplit(v_date_ch, "_")[[1L]])
    year_from_fname <- v_date_int[1L]
    month_from_fname <- v_date_int[2L]
    # Second field of first data row is day-of-year
    first_day <- suppressWarnings(
      as.integer(sub("^[^,]+,([^,]+),.*", "\\1", v_lines[1L]))
    )
    # Files downloaded in early year often contain late-prior-year data
    if (!is.na(first_day) && first_day > 300L && month_from_fname <= 3L) {
      year <- year_from_fname - 1L
    } else {
      year <- year_from_fname
    }
  }

  # ---- Build one data.table per table ID ------------------------------------
  l_result <- list()

  for (uid in unique(v_table_ids)) {
    v_chunk_lines <- v_lines[v_table_ids == uid]
    n_cols_data <- length(strsplit(v_chunk_lines[1L], ",")[[1L]])

    # Match DLD: exact ID first, then by column count
    dld_key <- if (uid %in% names(l_dld_tables)) {
      uid
    } else {
      v_matching <- names(v_dld_ncols)[v_dld_ncols == n_cols_data]
      if (!length(v_matching)) {
        warning(
          "No DLD definition matches table ID ",
          uid,
          " (",
          n_cols_data,
          " columns); skipping.",
          call. = FALSE
        )
        next
      }
      v_matching[1L]
    }

    v_col_names <- make.unique(l_dld_tables[[dld_key]], sep = ".")
    n_cols_def <- length(v_col_names)

    dt <- data.table::fread(
      text = paste(v_chunk_lines, collapse = "\n"),
      header = FALSE,
      na.strings = c("", "NA"),
      showProgress = FALSE
    )

    # Trim or pad to match the DLD column count
    if (ncol(dt) > n_cols_def) {
      dt <- dt[, seq_len(n_cols_def), with = FALSE]
    } else if (ncol(dt) < n_cols_def) {
      for (j in seq(ncol(dt) + 1L, n_cols_def)) {
        dt[, paste0("V", j) := NA_real_]
      }
    }
    data.table::setnames(dt, v_col_names)

    # ---- Build TIMESTAMP with year-rollover detection ----------------------
    v_day_vals <- as.integer(dt[["day_of_year"]])
    v_time_vals <- as.integer(dt[["time_hhmm"]])
    v_diffs <- diff(v_day_vals)
    # A drop of > 200 days between consecutive rows signals a year boundary
    v_year_offsets <- cumsum(c(0L, as.integer(v_diffs < -200L)))
    v_yr <- as.integer(year) + v_year_offsets
    v_H <- v_time_vals %/% 100L
    v_M <- v_time_vals %% 100L

    dt[,
      TIMESTAMP := as.POSIXct(
        ISOdate(v_yr, 1L, 1L, v_H, v_M, 0L, tz = "UTC") +
          (v_day_vals - 1L) * 86400
      )
    ]

    # Drop internal columns and move TIMESTAMP to the front
    dt[, c("table_id", "day_of_year", "time_hhmm") := NULL]
    data.table::setcolorder(dt, c("TIMESTAMP", setdiff(names(dt), "TIMESTAMP")))

    l_result[[uid]] <- dt
  }

  # ---- Return single table or full list ------------------------------------
  if (!is.null(table_id)) {
    tid <- as.character(table_id)
    if (!tid %in% names(l_result)) {
      stop(
        "Table ID ",
        table_id,
        " not found in '",
        basename(fname_dat),
        "'.",
        call. = FALSE
      )
    }
    return(l_result[[tid]])
  }

  l_result
}

##' Parse a Campbell .dld Final Storage Label file
##'
##' Extracts Output_Table column definitions from the label section of a
##' Campbell Scientific .dld file.
##'
##' @param fname_dld Path to the .dld file.
##' @return A named list (keyed by declared table ID) where each element is a
##'   character vector of column names. Column 1 (the stored table ID) is
##'   renamed \code{"table_id"}; \code{_RTM} columns become
##'   \code{"day_of_year"} and \code{"time_hhmm"}.
##' @keywords internal
.parse_campbell_dld <- function(fname_dld) {
  v_lines <- readLines(fname_dld, warn = FALSE)

  v_section_bounds <- grep("^;%", v_lines)
  if (length(v_section_bounds) < 2L) {
    return(list())
  }
  v_section <- v_lines[seq(
    v_section_bounds[1L] + 1L,
    v_section_bounds[2L] - 1L
  )]

  l_tables <- list()
  cur_id <- NULL
  v_cur_names <- character(0)
  n_rtm_count <- 0L
  is_output <- FALSE

  for (line in v_section) {
    stripped <- trimws(sub("^;", "", line))

    if (grepl("^\\d+\\s+(Output_Table|Input_Storage)", stripped)) {
      if (is_output && !is.null(cur_id) && length(v_cur_names)) {
        l_tables[[as.character(cur_id)]] <- v_cur_names
      }
      cur_id <- as.integer(sub("\\s.*", "", stripped))
      is_output <- grepl("Output_Table", stripped)
      v_cur_names <- character(0)
      n_rtm_count <- 0L
      next
    }

    if (is_output && grepl("^\\d+\\s+\\S", stripped)) {
      v_parts <- strsplit(stripped, "\\s+")[[1L]]
      if (length(v_parts) < 2L) {
        next
      }
      nm <- v_parts[2L]
      if (grepl("^\\d+$", nm)) {
        nm <- "table_id"
      } else if (nm == "_RTM") {
        n_rtm_count <- n_rtm_count + 1L
        nm <- if (n_rtm_count == 1L) "day_of_year" else "time_hhmm"
      }
      v_cur_names <- c(v_cur_names, nm)
    }
  }

  if (is_output && !is.null(cur_id) && length(v_cur_names)) {
    l_tables[[as.character(cur_id)]] <- v_cur_names
  }

  l_tables
}
