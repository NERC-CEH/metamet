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
