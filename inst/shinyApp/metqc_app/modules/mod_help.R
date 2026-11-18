mod_help_ui <- function(id) {
  NULL
}

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
