##' Create a new `metamet` object
##'
##' Internal constructor function for creating `metamet` objects. This function
##' initializes a list containing meteorological data and associated metadata.
##' Generally, users should use the \code{\link{metamet}} function instead,
##' which provides a more flexible interface for loading data from files or
##' data tables.
##'
##' @param dt A `data.table` containing the main meteorological observations.
##'   Must have a time column and a site identifier. Required.
##' @param dt_meta A `data.table` describing the columns in `dt`, including
##'   variable names, types (e.g., "time", "temperature"), and units.
##'   Required.
##' @param dt_site A `data.table` containing site-level metadata (e.g.,
##'   latitude, longitude, elevation). Required.
##' @param dt_qc Optional `data.table` of quality control flags or codes
##'   corresponding to the observations in `dt`. Defaults to `NULL`.
##' @param dt_ref Optional `data.table` of reference data (e.g., from ERA5
##'   reanalysis) for comparison with observations. Defaults to `NULL`.
##' @param site_id A character string specifying the site identifier.
##'   Must be provided and non-empty.
##'
##' @return A `metamet` object (a list with class `"metamet"`) containing
##'   the provided data tables. The object has a "site" column added to
##'   `dt`, `dt_qc`, and `dt_ref`.
##'
##' @details
##' The function adds a "site" column to `dt`, `dt_qc`, and `dt_ref` containing
##' the provided `site_id`. This is an internal constructor; users should prefer
##' the \code{\link{metamet}} generic function.
##'
##' @keywords internal
##' @noRd
new_metamet <- function(
  dt = data.table::data.table(),
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL
) {
  # Make sure `dt` is a data table
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data table")
  }
  # Make sure `site_id` is a specified character
  if (length(as.character(site_id)) < 1) {
    stop("`site_id` must be provided")
  }

  # add site_id to the data as site column
  # should this be ..site_id?
  dt[, site := as.character(site_id)]
  if (!is.null(dt_qc)) {
    dt_qc[, site := as.character(site_id)]
  }
  if (!is.null(dt_ref)) {
    dt_ref[, site := as.character(site_id)]
  }

  # Make the metamet object
  new_metamet <- list(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc = dt_qc,
    dt_ref = dt_ref
  )

  # Specify the class and return the metamet object
  class(new_metamet) <- c("metamet", class(new_metamet))
  return(new_metamet)
}

##' Create a `metamet` object from various input types
##'
##' Constructor function for creating `metamet` objects from files or
##' data tables. This is a generic function with methods for different
##' input types, allowing flexible object creation from CSV/Excel files,
##' data frames, or data tables.
##'
##' @param dt The main data input. For `metamet.character`, this is a file path
##'   to a CSV or other format readable by `data.table::fread()`. For
##'   `metamet.data.frame` or `metamet.data.table`, this is the actual data object.
##' @param dt_meta A `data.table`, `data.frame`, or file path to metadata describing
##'   the columns in `dt`. Excel files (.xlsx) are supported; otherwise CSV format
##'   is assumed. Required.
##' @param dt_site A `data.table`, `data.frame`, or file path to site metadata
##'   containing site-level information. Required.
##' @param dt_qc Optional quality control data; can be a `data.table`, `data.frame`,
##'   or file path. Defaults to `NULL`.
##' @param dt_ref Optional reference data (e.g., ERA5 reanalysis); can be a
##'   `data.table`, `data.frame`, or file path. Defaults to `NULL`.
##' @param site_id A character string specifying the site identifier.
##'   Required for `metamet.character` method. Used to tag all observations.
##' @param ... Additional arguments passed to methods.
##'
##' @return A `metamet` object containing the provided data and metadata,
##'   with processed and validated structure.
##'
##' @seealso
##'   \code{\link{new_metamet}} for the internal constructor
##'
##' @examples
##' \dontrun{
##' # Create from files
##' mm <- metamet(
##'   dt = "data.csv",
##'   dt_meta = "metadata.xlsx",
##'   dt_site = "site_info.csv",
##'   site_id = "UK-AMO"
##' )
##'
##' # Create from data tables (requires metadata and site data)
##' mm <- metamet(
##'   dt = my_data_table,
##'   dt_meta = my_metadata_table,
##'   dt_site = my_site_info_table,
##'   site_id = "UK-AMO"
##' )
##' }
##'
##' @export
# 1. Define the generic function
metamet <- function(dt, ...) {
  UseMethod("metamet")
}

##' @rdname metamet
##' @export
# 2. Default method for unexpected types
metamet.default <- function(dt, ...) {
  stop("Unsupported input type: ", class(dt))
}

##' @rdname metamet
##' @export
# 3. Method for character strings (files)
metamet.character <- function(
  dt,
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL,
  ...
) {
  message("Loading file: ", dt)
  dt <- data.table::fread(dt)

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    # excel format
    if (fs::path_ext(dt_meta) == "xlsx") {
      dt_meta <- readxl::read_excel(dt_meta)
    } else {
      # .csv format
      dt_meta <- data.table::fread(dt_meta)
    }
  }
  data.table::setDT(dt_meta)

  # and repeat for site
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  data.table::setDT(dt_site)

  # and repeat for qc
  if ("character" %in% class(dt_qc)) {
    dt_qc <- data.table::fread(dt_qc)
  }
  # allow dt_qc to not be required - can remain NULL
  if (!is.null(dt_qc)) {
    data.table::setDT(dt_qc)
  }

  # and repeat for ref
  if ("character" %in% class(dt_ref)) {
    dt_ref <- data.table::fread(dt_ref)
  }
  # allow dt_ref to not be required - can remain NULL
  if (!is.null(dt_ref)) {
    data.table::setDT(dt_ref)
  }

  # l_dt <- lapply(v_fname, import_campbell_data)
  # and then call the data table method
  metamet(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc = dt_qc,
    dt_ref = dt_ref,
    site_id = site_id
  )
}

##' @rdname metamet
##' @export
# 4. Method for data frames
metamet.data.frame <- function(
  dt,
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL,
  ...
) {
  message("Converting data frame to data table...")
  data.table::setDT(dt)

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  data.table::setDT(dt_meta)

  # and repeat for site
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  data.table::setDT(dt_site)

  # and repeat for qc
  if ("character" %in% class(dt_qc)) {
    dt_qc <- data.table::fread(dt_qc)
  }
  # allow dt_qc to not be required - can remain NULL
  if (!is.null(dt_qc)) {
    data.table::setDT(dt_qc)
  }

  # and repeat for ref
  if ("character" %in% class(dt_ref)) {
    dt_ref <- data.table::fread(dt_ref)
  }
  # allow dt_ref to not be required - can remain NULL
  if (!is.null(dt_ref)) {
    data.table::setDT(dt_ref)
  }

  metamet(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc = dt_qc,
    dt_ref = dt_ref,
    site_id = site_id
  )
}

##' @rdname metamet
##' @export
# 4. Method for data tables
metamet.data.table <- function(
  dt,
  dt_meta = NULL,
  dt_site = NULL,
  dt_qc = NULL,
  dt_ref = NULL,
  site_id = NULL,
  ...
) {
  message("Reading data table ...")

  # read metadata from file if a path is specified
  # otherwise the argument is assumed to be a data table/frame
  if ("character" %in% class(dt_meta)) {
    dt_meta <- data.table::fread(dt_meta)
  }
  data.table::setDT(dt_meta)

  # and repeat for site
  if ("character" %in% class(dt_site)) {
    dt_site <- data.table::fread(dt_site)
  }
  data.table::setDT(dt_site)

  # and repeat for qc
  if ("character" %in% class(dt_qc)) {
    dt_qc <- data.table::fread(dt_qc)
  }
  # allow dt_qc to not be required - can remain NULL
  if (!is.null(dt_qc)) {
    data.table::setDT(dt_qc)
  }

  # and repeat for ref
  if ("character" %in% class(dt_ref)) {
    dt_ref <- data.table::fread(dt_ref)
  }
  # allow dt_ref to not be required - can remain NULL
  if (!is.null(dt_ref)) {
    data.table::setDT(dt_ref)
  }

  mm <- new_metamet(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc = dt_qc,
    dt_ref = dt_ref,
    site_id = site_id
  )
  mm <- restrict(mm)
  mm <- convert_time_char_to_posix(mm)
  return(mm)
}

# restrict dt and dt_meta variables to only those that occur in both
restrict <- function(mm) {
  v_site_dt <- unique(mm$dt[, site])
  v_name_meta <- mm$dt_meta$name_dt
  v_name_dt <- colnames(mm$dt)
  # subset dt to only those that exist in metadata
  v_name_dt <- v_name_dt[v_name_dt %in% v_name_meta]
  mm$dt <- mm$dt[, ..v_name_dt]
  # possibly subset these based on time in dt but time is just a character here
  # qc table has an extra column for the name of the validator
  if (!is.null(mm$dt_qc)) {
    mm$dt_qc <- mm$dt_qc[, c(..v_name_dt, "validator")]
  }
  if (!is.null(mm$dt_ref)) {
    mm$dt_ref <- mm$dt_ref[, ..v_name_dt]
  }

  # subset metadata to only variables that exist in dt and correspond to that site
  mm$dt_meta <- mm$dt_meta[site %in% v_site_dt & name_dt %in% v_name_dt]

  # subset site to only those that exist in dt
  v_site_dt <- unique(mm$dt[, site])
  mm$dt_site <- mm$dt_site[site %in% v_site_dt]

  return(mm)
}

convert_time_char_to_posix <- function(mm) {
  # get the name and format of the time variable
  time_name <- mm$dt_meta[type == "time", name_dt]

  # sometimes time is automatically read as POSIXct; only convert if is not
  if ("POSIXct" %!in% class(mm$dt[, get(time_name)])) {
    time_format <- mm$dt_meta[type == "time", time_char_format]
    # make sure time is a character variable - sometimes read as integer
    mm$dt[, eval(time_name) := as.character(get(time_name))]
    # convert character variable to POSIXct
    mm$dt[,
      eval(time_name) := as.POSIXct(
        get(time_name),
        tz = "GMT",
        format = time_format
      )
    ]
  }

  # remove duplicate rows - sometimes occur in Campbell files
  mm$dt <- mm$dt[!duplicated(mm$dt[, ..time_name]), ]

  return(mm)
}

# define method for printing a metamet object
#' @export
#' @method print metamet
print.metamet <- function(x, ...) {
  cat("metamet object\n")
  cat("-------------\n")
  cat("Data table (dt) - first 6 cols:\n")
  print(x$dt[, 1:min(ncol(x$dt), 6)])
  cat("\nMetadata (dt_meta) - first 6 cols:\n")
  print(head(x$dt_meta[, 1:min(ncol(x$dt_meta), 6)]))
  cat("\nSite information (dt_site):\n")
  print(x$dt_site)
  cat("\nQC information (dt_qc) - first 6 cols:\n")
  print(x$dt_qc[, 1:min(ncol(x$dt_qc), 6)])
  cat("\nReference data (dt_ref) - first 6 cols:\n")
  print(x$dt_ref[, 1:min(ncol(x$dt_ref), 6)])
}

# define summary method for a metamet object
#' @export
#' @method summary metamet
summary.metamet <- function(object, ...) {
  cat("Summary of metamet object\n")
  cat("------------------------\n")
  cat("Data table (dt):\n")
  print(summary(object$dt))
  cat("\nMetadata (dt_meta):\n")
  print(summary(object$dt_meta))
  cat("\nSite information (dt_site):\n")
  print(summary(object$dt_site))
}
