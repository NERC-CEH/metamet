#' Launches the shiny app for the metamet package
#'
#' This function provides a web browser interface.
#' @keywords shiny app
#' @return shiny application object
#' @export
run_shiny <- function() {
  .check_suggests(
    c(
      "shiny",
      "shinydashboard",
      "shinyjs",
      "shinyFiles",
      "shinyvalidate",
      "shinycssloaders",
      "ggiraph",
      "glue"
    ),
    context = "the metamet Shiny app"
  )

  appDir <- system.file("shinyApp", "metqc_app", package = "metamet")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing `metamet`.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}
