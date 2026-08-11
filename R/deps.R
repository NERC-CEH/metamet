#' Check that required suggested packages are installed
#'
#' Verify that a set of suggested packages is available. If any are missing,
#' the function throws an informative error instructing the user how to
#' install them. This is intended for optional features that rely on
#' non-mandatory dependencies.
#'
#' @param pkgs Character vector of package names to check.
#' @param context Character scalar describing the feature or operation that
#'   requires the packages. Used in the error message.
#'
#' @return Invisibly returns `TRUE` if all packages are available. Otherwise,
#'   an error is thrown.
#'
#' @details
#' The function uses \code{requireNamespace()} with \code{quietly = TRUE} to
#' test availability. Missing packages are reported together in a single
#' error, including an `install.packages()` call the user can copy-paste.
#'
#' @examples
#' \dontrun{
#' .check_suggests(c("readxl", "openxlsx"), context = "Excel import")
#' }
#'
#' @keywords internal
.check_suggests <- function(pkgs, context = "this feature") {
  missing <- pkgs[
    !vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]

  if (length(missing)) {
    stop(
      "To use ",
      context,
      ", please install: ",
      paste(sprintf("'%s'", missing), collapse = ", "),
      "\nYou can install them with install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      ")).",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
