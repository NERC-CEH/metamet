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
  # read in data
  dt <- fread(
    file = fname,
    skip = 4,
    header = FALSE,
    na.strings = c("NAN"),
    sep = ","
  )
  names(dt) <- header

  # If fread could not auto-detect POSIXct (e.g. due to a few corrupted rows
  # causing the column to be read as character), convert explicitly and drop
  # the rows that cannot be parsed rather than stopping.
  if (!inherits(dt$TIMESTAMP, "POSIXct")) {
    n_before <- nrow(dt)
    dt[, TIMESTAMP := as.POSIXct(TIMESTAMP,
      format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
    )]
    dt <- dt[!is.na(TIMESTAMP)]
    n_dropped <- n_before - nrow(dt)
    if (n_dropped > 0L) {
      warning(
        n_dropped, " row(s) with unparseable timestamps dropped in ",
        basename(fname),
        call. = FALSE
      )
    }
  }

  # remove duplicate rows - sometimes occur in the Campbell files
  dt <- dt[!duplicated(dt$TIMESTAMP), ]
  # if variable RECORD exists, remove it
  if ("RECORD" %in% colnames(dt)) {
    dt$RECORD <- NULL
  }
  return(dt)
}
