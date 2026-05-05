# =========================
# Column Mapping UI
# =========================
mod_colmap_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("name_convention"),
                "Naming convention",
                choices = NULL),

    uiOutput(ns("custom_mapping_ui")),

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

    custom_map <- reactiveVal(NULL)
    custom_ready <- reactiveVal(FALSE)

    # -------------------------
    # Naming conventions
    # -------------------------
    observe({
      req(uploaded()$mm)

      meta <- uploaded()$mm$dt_meta
      possible_names <- grep("^name_", names(meta), value = TRUE)

      possible_names <- setdiff(possible_names, c("name_local", "name_dt"))

      if (length(possible_names) == 0) {
        possible_names <- "name_era5"
      }

      updateSelectInput(session, "name_convention",
                        choices = c(possible_names, "Custom"),
                        selected = possible_names[1])
    })


    # -------------------------
    # Metadata variable pool for Custom
    # -------------------------
    meta_var_choices <- reactive({
      req(uploaded()$mm)

      meta <- as.data.frame(uploaded()$mm$dt_meta)

      name_cols <- grep("^name_", names(meta), value = TRUE)

      if (length(name_cols) == 0) return(character(0))

      vals <- unlist(meta[, name_cols, drop = FALSE], use.names = FALSE)

      vals <- vals[!is.na(vals) & vals != ""]
      vals <- unique(vals)

      # -------------------------
      # REMOVE RESERVED NAMES
      # -------------------------
      reserved <- c("site", "TIMESTAMP", "time")

      vals <- vals[!vals %in% reserved]

      vals
    })


    # -------------------------
    # Show Custom mapping UI
    # -------------------------
    output$custom_mapping_ui <- renderUI({
      req(uploaded()$mm)

      if (input$name_convention != "Custom") return(NULL)

      dt <- uploaded()$mm$dt
      vars <- setdiff(names(dt), c("site", "TIMESTAMP"))

      tagList(
        tags$hr(),
        tags$h4("Custom variable selection"),
        tags$em("⚠️ Custom metadata handling is not yet implemented. This is a preview only."),

        lapply(vars, function(col) {
          selectInput(
            inputId = session$ns(paste0("map_", col)),
            label = paste("Map", col),
            choices = meta_var_choices(),
            selected = NULL
          )
        })
      )
    })


    # -------------------------
    # Build custom mapping
    # -------------------------
    observe({
      req(input$name_convention == "Custom", uploaded()$mm)
      req(uploaded()$mm)

      dt <- uploaded()$mm$dt
      vars <- setdiff(names(dt), c("site", "TIMESTAMP"))

      map <- lapply(vars, function(col) {
        input[[paste0("map_", col)]]
      })

      names(map) <- vars

      custom_map(map)
    })


    # -------------------------
    # Live mapped object
    # -------------------------
    live_mapped <- reactive({
      req(uploaded()$mm)

      # -------------------------
      # CUSTOM MODE
      # -------------------------
      if (input$name_convention == "Custom") {

        req(custom_map())

        dt <- uploaded()$mm$dt

        # build preview table
        new_dt <- dt[, c("site", "TIMESTAMP", names(custom_map())), drop = FALSE]

        for (v in names(custom_map())) {
          # rename variable columns according to mapping
          names(new_dt)[names(new_dt) == v] <- custom_map()[[v]]
        }

        return(list(dt = new_dt))
      }

      # -------------------------
      # STANDARD MODE
      # -------------------------
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

      DT::datatable(
        head(live_mapped()$dt, 10),
        options = list(scrollX = TRUE)
      )
    })


    # -------------------------
    # Confirm / Reset
    # -------------------------
    observeEvent(input$confirm_mapping, {
      mapped_mm(live_mapped())
      confirmed(TRUE)

      if (input$name_convention == "Custom") {
        custom_ready(TRUE)
      }
    })

    observeEvent(input$reset_mapping, {
      confirmed(FALSE)
      mapped_mm(NULL)
      custom_ready(FALSE)
    })


    # -------------------------
    # Status text
    # -------------------------
    output$confirmation_text <- renderUI({
      if (input$name_convention == "Custom") {
        tags$div(style = "color: blue; font-weight: bold;",
                 "ℹ️ Custom mode active: preview only (no metadata object created yet)")
      } else if (confirmed()) {
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
      })
    )
  })
}
