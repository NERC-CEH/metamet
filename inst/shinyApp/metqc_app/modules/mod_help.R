#' Help module UI
#'
#' User interface for the help module.
#'
#' @param id Module id.
#'
#' @return A Shiny UI element.
#' @noRd
mod_help_ui <- function(id) {
  NULL
}

#' Help module server
#'
#' Server logic for help actions, including opening user
#' documentation and closing the application.
#'
#' @param id Module id.
#' @param root_session Root Shiny session used to access
#'   global application inputs.
#'
#' @return No return value. Called for its side effects.
#' @noRd
mod_help_server <- function(id, root_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(root_session$input$stop_app, {
      stopApp()
    })

    observeEvent(root_session$input$open_app_guide, {
      browseURL(
        "https://nerc-ceh.github.io/metamet/articles/app_user_guide.html"
      )
    })

    observeEvent(root_session$input$open_gapfill_guide, {
      browseURL(
        "https://nerc-ceh.github.io/metamet/articles/gap_fill_methods.html"
      )
    })
  })
}
