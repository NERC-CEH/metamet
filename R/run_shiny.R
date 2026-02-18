#' Launches the shiny app for the metamet package
#'
#' This function provides a web browser interface.
#' @keywords shiny app
#' @return shiny application object
#' @import shiny
#' @export
run_shiny <- function() {
  appDir <- system.file("shinyApp", "metqc_app", package = "metamet")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing `mypackage`.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}
