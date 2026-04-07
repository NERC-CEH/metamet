# =========================
# Column Mapping UI
# =========================
mod_colmap_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("name_convention"),
                "Naming convention",
                choices = NULL),

    #h4("Raw data preview"),
    #DT::DTOutput(ns("raw_preview")),

    h4("Mapped data preview"),
    DT::DTOutput(ns("mapped_preview")),

    fluidRow(
      column(6, actionButton(ns("confirm_mapping"), "Confirm Mapping")),
      column(6, actionButton(ns("reset_mapping"), "Reset Mapping"))
    ),

    uiOutput(ns("confirmation_text"))
  )
}

# =========================
# Column Mapping Server
# =========================
mod_colmap_server <- function(id, uploaded) {
  moduleServer(id, function(input, output, session) {

    mapped_mm <- reactiveVal(NULL)
    confirmed <- reactiveVal(FALSE)

    # -------------------------
    # Naming conventions
    # -------------------------
    observe({
      req(uploaded()$mm)

      meta <- uploaded()$mm$dt_meta
      possible_names <- grep("^name_", names(meta), value = TRUE)

      # Exclude specific names (the original name and new name)
      possible_names <- setdiff(possible_names, c("name_local", "name_dt"))

      if (length(possible_names) == 0) {
        possible_names <- "name_era5"
      }

      updateSelectInput(session, "name_convention",
                        choices = possible_names,
                        selected = possible_names[1])
    })

    # -------------------------
    # Live mapped object
    # -------------------------
    live_mapped <- reactive({
      req(uploaded()$mm, input$name_convention)

      tryCatch({
        metamet::change_naming_convention(
          uploaded()$mm,
          input$name_convention
        )
      }, error = function(e) {
        message("Mapping failed: ", e$message)
        uploaded()$mm
      })
    })

    # -------------------------
    # Mapped preview
    # -------------------------
    output$mapped_preview <- DT::renderDataTable({
      req(live_mapped())
      DT::datatable(head(live_mapped()$dt, 10),
                    options = list(scrollX = TRUE))
    })

    # -------------------------
    # Confirm / Reset
    # -------------------------
    observeEvent(input$confirm_mapping, {
      mapped_mm(live_mapped())
      confirmed(TRUE)
    })

    observeEvent(input$reset_mapping, {
      confirmed(FALSE)
      mapped_mm(NULL)
    })

    # -------------------------
    # Status text
    # -------------------------
    output$confirmation_text <- renderUI({
      if (confirmed()) {
        tags$div(style = "color: green; font-weight: bold;",
                 "✅ Mapping confirmed!")
      } else {
        tags$div(style = "color: orange;",
                 "⚠️ Mapping not confirmed yet")
      }
    })

    # -------------------------
    # Return mapped object
    # -------------------------
    return(
      reactive({
        if (confirmed()) mapped_mm() else NULL
        # if confirmed returns object mapped_mm for use in app
      })
    )
  })
}
