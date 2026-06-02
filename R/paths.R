pkg_extdata <- function(...) {
  # Use rprojroot to find the package root during development
  pkg_root <- tryCatch(
    rprojroot::find_root(rprojroot::is_r_package),
    error = function(e) NULL
  )

  if (!is.null(pkg_root)) {
    path <- fs::path(pkg_root, "inst", "extdata", ...)
    # Normalize the path to handle any encoding issues
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (!file.exists(path)) {
      cli::cli_abort(
        "Can't find package file at: {path}",
        call = NULL
      )
    }
    return(path)
  }

  # Fall back to system.file for installed packages
  system.file("extdata", ..., package = "metamet", mustWork = TRUE)
}
