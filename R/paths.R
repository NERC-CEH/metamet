pkg_extdata <- function(...) {
  # Try using system.file first (works for installed packages)
  extdata_path <- system.file(
    "extdata",
    package = "metamet",
    mustWork = TRUE
  )

  # Fallback to relative path for development/testing
  if (extdata_path == "") {
    extdata_path <- fs::path_package("metamet", "extdata")
  }

  # Ensure the path exists
  if (!dir.exists(extdata_path)) {
    cli::cli_abort("Can't find package file.")
  }

  fs::path(extdata_path, ...)
}
