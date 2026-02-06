utils::globalVariables(c("errorMessage"))
.datatable.aware <- TRUE

# This file defines a constructor for the metamet class and some essential S3
# methods. A metamet object is a list of data tables, extended by
# specifying meta data.

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

  # Specify the class and return the metamet
  class(new_metamet) <- c("metamet", class(new_metamet))
  return(new_metamet)
}

# 1. Define the generic function
metamet <- function(x, ...) {
  UseMethod("metamet")
}

# 2. Default method for unexpected types
#' @export
metamet.default <- function(x, ...) {
  stop("Unsupported input type: ", class(x))
}

# 3. Method for character strings (files)
#' @export
metamet.character <- function(x, dt_meta = NULL, dt_site = NULL, ...) {
  message("Loading file: ", x)
  dt <- data.table::fread(x)

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  dt_meta <- data.table::setDT(dt_meta)

  # and repeat for site, qc, faults, ref, ...
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  dt_site <- data.table::setDT(dt_site)

  # l_dt <- lapply(v_fname, import_campbell_data)
  # and then call the data table method
  metamet(dt, dt_meta = dt_meta, dt_site = dt_site)
}

# 4. Method for data frames
#' @export
metamet.data.frame <- function(x, dt_meta = NULL, dt_site = NULL, ...) {
  message("Converting data frame to data table...")
  dt <- data.table::setDT(x)

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  dt_meta <- data.table::setDT(dt_meta)

  # and repeat for site, qc, faults, ref, ...
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  dt_site <- data.table::setDT(dt_site)

  metamet(dt, dt_meta = dt_meta, dt_site = dt_site)
}

# 4. Method for data tables
#' @export
metamet.data.table <- function(x, dt_meta = NULL, dt_site = NULL, ...) {
  message("Reading data table ...")

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  dt_meta <- data.table::setDT(dt_meta)

  # and repeat for site, qc, faults, ref, ...
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  dt_site <- data.table::setDT(dt_site)

  new_metamet(dt = x, dt_meta = dt_meta, dt_site = dt_site)
}

# helper function for creating metamet objects from a list of data tables or file names

get_metamet_filenames <- function(
  fname_pattern = "*Metmast_MainMet_30min*",
  dir_in = here::here("data-raw/UK-AMO")
) {
  v_fname <- fs::dir_ls(dir_in, glob = fname_pattern)
  return(v_fname)
}
