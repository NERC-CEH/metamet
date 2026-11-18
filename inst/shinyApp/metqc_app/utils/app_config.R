#' Get application root directories
#'
#' Returns available root directories for file browsing and the default root.
#'
#' @param package Package name containing application data.
#'
#' @return A list with `roots` and `default_root`.
get_app_roots <- function(package = "metamet") {
  roots <- if (.Platform$OS.type == "windows") {
    drives <- paste0(letters, ":/")
    drives <- drives[dir.exists(drives)]
    setNames(drives, drives)
  } else {
    c(home = fs::path_home())
  }
  package_extdata <- system.file("extdata", package = package)
  if (nzchar(package_extdata) && dir.exists(package_extdata)) {
    roots <- c("Package data" = package_extdata, roots)
  }
  list(
    roots = roots,
    default_root = names(roots)[1L]
  )
}

#' Get current username
#'
#' Returns the username of the current system user.
#'
#' @return A character string containing the username.
get_app_username <- function() {
  Sys.info()[["user"]]
}
