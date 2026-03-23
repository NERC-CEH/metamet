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

  if (class(dt$TIMESTAMP)[1] != "POSIXct") {
    stop(paste("Non-POSIX timestamp in", fname))
  }

  # remove duplicate rows - sometimes occur in the Campbell files
  dt <- dt[!duplicated(dt$TIMESTAMP), ]
  # if variable RECORD exists, remove it
  if ("RECORD" %in% colnames(dt)) {
    dt$RECORD <- NULL
  }
  return(dt)
}
