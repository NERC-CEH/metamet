utils::globalVariables(c("errorMessage"))
.datatable.aware <- TRUE
# Use GMT, never BST
Sys.setenv(TZ = "GMT")

# This file defines the metamet class, its constructor, and some essential S3
# methods. A metamet object is a list of data tables - one for the met data
# itself, and additional ones for the meta-data needed to process and interpret
# the met data.

# Constructor
new_metamet <- function(
  dt = data.table::data.table(),
  dt_meta = NULL,
  dt_site = NULL
) {
  # Make sure `dt` is a data table
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data table")
  }

  # Make the metamet object
  new_metamet <- list(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site
  )

  # Specify the class and return the metamet object
  class(new_metamet) <- c("metamet", class(new_metamet))
  return(new_metamet)
}

# 1. Define the generic function
metamet <- function(dt, ...) {
  UseMethod("metamet")
}

# 2. Default method for unexpected types
#' @export
metamet.default <- function(dt, ...) {
  stop("Unsupported input type: ", class(dt))
}

# 3. Method for character strings (files)
#' @export
metamet.character <- function(dt, dt_meta = NULL, dt_site = NULL, ...) {
  message("Loading file: ", dt)
  dt <- data.table::fread(dt)

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  data.table::setDT(dt_meta)

  # and repeat for site, qc, faults, ref, ...
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  data.table::setDT(dt_site)

  # l_dt <- lapply(v_fname, import_campbell_data)
  # and then call the data table method
  metamet(dt = dt, dt_meta = dt_meta, dt_site = dt_site)
}

# 4. Method for data frames
#' @export
metamet.data.frame <- function(dt, dt_meta = NULL, dt_site = NULL, ...) {
  message("Converting data frame to data table...")
  data.table::setDT(dt)

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  data.table::setDT(dt_meta)

  # and repeat for site, qc, faults, ref, ...
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  data.table::setDT(dt_site)

  metamet(dt = dt, dt_meta = dt_meta, dt_site = dt_site)
}

# 4. Method for data tables
#' @export
metamet.data.table <- function(dt, dt_meta = NULL, dt_site = NULL, ...) {
  message("Reading data table ...")

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  data.table::setDT(dt_meta)

  # and repeat for site, qc, faults, ref, ...
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  data.table::setDT(dt_site)

  mm <- new_metamet(dt = dt, dt_meta = dt_meta, dt_site = dt_site)
  mm <- restrict(mm)
  mm <- convert_time_char_to_posix(mm)
  return(mm)
}

# restrict dt and dt_meta tables to only those that occur in both
restrict <- function(mm) {
  v_name_meta <- mm$dt_meta$name_dt
  v_name_dt <- colnames(mm$dt)
  # subset dt to only those that exist in metadata
  v_name_dt <- v_name_dt[v_name_dt %in% v_name_meta]
  mm$dt <- mm$dt[, ..v_name_dt]

  # subset metadata to only variables that exist in dt
  mm$dt_meta <- mm$dt_meta[name_dt %in% v_name_dt]

  return(mm)
}

convert_time_char_to_posix <- function(mm) {
  # get the name and format of the time variable
  time_name <- mm$dt_meta[type == "time", name_dt]
  time_format <- mm$dt_meta[type == "time", time_char_format]
  # convert character variable to POSIXct
  mm$dt[, eval(time_name) := as.POSIXct(strptime(get(time_name), time_format))]

  # remove duplicate rows - sometimes occur in Campbell files
  mm$dt <- mm$dt[!duplicated(mm$dt[, TIMESTAMP]), ]

  return(mm)
}

time_average <- function(mm, avg.time = "hour") {
  # get the name and format of the time, ws & wd variables
  time_name <- mm$dt_meta[type == "time", name_dt]
  wd_name <- mm$dt_meta[type == "wind direction", name_dt]
  ws_name <- mm$dt_meta[type == "wind speed" | type == "windspeed", name_dt]

  # rename time variable with openair convention
  # mm$dt[, date := get(time_name)]
  setnames(mm$dt, eval(time_name), "date")

  # and ws & wd if present, for proper vector averaging
  if (length(wd_name) > 0) {
    mm$dt[, wd := get(wd_name)]
  }
  if (length(ws_name) > 0) {
    mm$dt[, ws := get(ws_name)]
  }

  first_date <- min(mm$dt[, date])
  start.date <- lubridate::floor_date(first_date, unit = "hour")

  mm$dt <- openair::timeAverage(
    mm$dt,
    avg.time = avg.time,
    start.date = start.date
  )
  data.table::setDT(mm$dt) # openair returns a data frame - upgrade to dt
  # ICOS convention is to use end of interval, and first row pre-dates the start
  mm$dt <- mm$dt[date >= first_date]

  # restore original time name
  setnames(mm$dt, "date", eval(time_name))
  # and delete extra names
  if (length(wd_name) > 0) {
    mm$dt[, wd := NULL]
  }
  if (length(ws_name) > 0) {
    mm$dt[, ws := NULL]
  }
  return(mm)
}
