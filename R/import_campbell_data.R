# define function to read TOA5 data
import_campbell_data <- function(fname) {
  # second line of header contains variable names
  header <- scan(
    file = fname,
    skip = 1,
    nlines = 1,
    what = character(),
    sep = ","
  )
  # read in data; fill=TRUE ensures blank lines (0 fields) are read as all-NA
  # rows rather than causing fread to stop early.
  dt <- fread(
    file = fname,
    skip = 4,
    header = FALSE,
    na.strings = c("NAN", ""),
    sep = ",",
    fill = TRUE
  )
  names(dt) <- header

  # If fread could not auto-detect POSIXct (e.g. due to a few corrupted rows
  # causing the column to be read as character), convert explicitly and drop
  # the rows that cannot be parsed rather than stopping.
  if (!inherits(dt$TIMESTAMP, "POSIXct")) {
    n_before <- nrow(dt)
    dt[,
      TIMESTAMP := as.POSIXct(
        TIMESTAMP,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    ]
    dt <- dt[!is.na(TIMESTAMP)]
    n_dropped <- n_before - nrow(dt)
    if (n_dropped > 0L) {
      warning(
        n_dropped,
        " row(s) with unparseable timestamps dropped in ",
        basename(fname),
        call. = FALSE
      )
    }
  }

  # Drop rows with implausible years (e.g. "23-04-01" parsed as year 23 from
  # a partially-corrupted line where "2023-04-01" lost its leading "20").
  # Year 1900 is a conservative lower bound; any TOA5 data predating it is
  # certainly a parsing artefact.
  n_before <- nrow(dt)
  dt <- dt[!is.na(TIMESTAMP) & data.table::year(TIMESTAMP) >= 1900L]
  n_bad_year <- n_before - nrow(dt)
  if (n_bad_year > 0L) {
    warning(
      n_bad_year,
      " row(s) with implausible year (<1900) dropped in ",
      basename(fname),
      call. = FALSE
    )
  }

  # remove duplicate rows - sometimes occur in the Campbell files
  dt <- dt[!duplicated(dt$TIMESTAMP), ]
  # if variable RECORD exists, remove it
  if ("RECORD" %in% colnames(dt)) {
    dt$RECORD <- NULL
  }
  return(dt)
}
