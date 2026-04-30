#' Launches the shiny app for the metamet package
#'
#' This function provides a web browser interface.
#' @keywords shiny app
#' @return shiny application object
#' @export
run_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The 'shiny' package is required to run the metamet app(). ",
      "Please install it with install.packages('shiny').",
      call. = FALSE
    )
  }

  appDir <- system.file("shinyApp", "metqc_app", package = "metamet")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing `metamet`.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}
