##* WIP: this duplicates site column, and should change to powerjoin anyway
# so can probably retire this

#' Combine metamet Objects by Columns
#'
#' Combines multiple metamet objects by column-binding their data tables,
#' while merging metadata tables by variable name.
#'
#' @param ... metamet objects to combine
#' @param deparse.level Integer determining the naming of combined data tables
#'   (unused for metamet objects, included for S3 method compatibility)
#'
#' @return A new metamet object containing:
#'   \item{dt}{Combined data table with columns from all input objects,
#'     merged by TIMESTAMP}
#'   \item{dt_meta}{Unique rows from metadata tables of all input objects}
#'   \item{dt_site}{Unique rows from site information tables of all input objects}
#'
#' @export
#' @method cbind metamet
#'
#' @examples
#' \dontrun{
#'   mm_combined <- cbind(mm1, mm2, mm3)
#' }
#'
cbind.metamet <- function(..., deparse.level = 1) {
  # get the list of metamet objects
  l_mm <- list(...)
  # check that all objects are of class metamet
  if (!all(sapply(l_mm, function(x) "metamet" %in% class(x)))) {
    stop("All objects must be of class metamet")
  }

  # Check that all are from the same site for simple cbind function
  site_id <- l_mm[[1]]$dt[1, site] # assume first row is enough
  for (i in seq_along(l_mm)) {
    this_site_name <- l_mm[[i]]$dt[1, site]
    if (this_site_name != site_id) {
      stop("All objects must be from same site")
    }
  }

  # Change time_name variable of the first in the list to be consistent
  # throughout the list e.g. all "TIMESTAMP"
  first_time_name <- l_mm[[1]]$dt_meta[type == "time", name_dt]
  for (i in seq_along(l_mm)) {
    this_time_name <- l_mm[[i]]$dt_meta[type == "time", name_dt]
    setnames(l_mm[[i]]$dt, this_time_name, first_time_name)
  }

  # combine the data tables by column
  # merge the metadata tables by name_dt
  ##* WIP: this duplicates the site column - change to use powerjoin anyway.
  dt_combined <- Reduce(
    function(x, y) merge(x, y, by = first_time_name, all = TRUE),
    lapply(l_mm, function(x) x$dt)
  )

  l_dt_meta <- lapply(l_mm, with, dt_meta)
  dt_meta_combined <- data.table::rbindlist(
    l_dt_meta,
    use.names = TRUE,
    fill = TRUE
  )
  dt_meta_combined <- unique(dt_meta_combined)

  l_dt_site <- lapply(l_mm, with, dt_site)
  dt_site_combined <- data.table::rbindlist(
    l_dt_site,
    use.names = TRUE,
    fill = TRUE
  )
  dt_site_combined <- unique(dt_site_combined)

  # create a new metamet object with the combined data and metadata
  new_metamet(
    dt = dt_combined,
    dt_meta = dt_meta_combined,
    dt_site = dt_site_combined,
    site_id = site_id
  )
}
