# This module uses the standardised dictionary names to map the columns of the files to upload
#' Column Mapping UI
mod_colmap_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dict_set_ui")),
    uiOutput(ns("mapping_ui")),

    # adds confirm mapping button and reset mapping button side by side
    fluidRow(
      column(6, actionButton(ns("confirm_mapping"), "Confirm Mapping")),
      column(6, actionButton(ns("reset_mapping"), "Reset Mapping"))
    ),

    uiOutput(ns("confirmation_text")),
    DT::DTOutput(ns("mapped_preview"))
  )
}

#' Column Mapping Server
#' @param id Module ID
#' @param uploaded Reactive containing a list with `$data`
#' @param dictionary_list List of tibbles (preloaded dictionaries)
mod_colmap_server <- function(id, uploaded, metadata = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mapped_data <- reactiveVal(NULL)
    confirmed <- reactiveVal(FALSE)

    # Dictionary selection dropdown, this also groups the extra lists (long_name & all_units) in a separate dictionary called Custom
    output$dict_set_ui <- renderUI({
      req(uploaded())
      normal_dicts <- setdiff(names(dictionary_list), c("long_name", "all_units"))
      selectInput(ns("dict_set"), "Select dictionary set",
                  choices = c(normal_dicts, "Custom"),
                  selected = normal_dicts[1])
    })

    # Mapping UI
    output$mapping_ui <- renderUI({
      req(uploaded())
      df <- uploaded()$data
      req(df)
      cols <- names(df)
      req(input$dict_set)

      # applies this UI to all dicts except Custom and era5_to_icos
      if (input$dict_set != "Custom" && input$dict_set != "era5_to_icos") {
        dict_tbl <- dictionary_list[[input$dict_set]]
        dict_cols <- grep("_name$", names(dict_tbl), value = TRUE)
        dict_choices <- unlist(dict_tbl[dict_cols[1]])

        tagList(
          lapply(cols, function(col) {
            all_choices <- unique(c(col, dict_choices))
            fluidRow(
              column(4, strong(col)),
              column(8,
                     selectInput(ns(paste0("map_", col)), label = NULL,
                                 choices = all_choices, selected = col))
            )
          })
        )
      # Special UI for era5_to_icos dictionary, gives the option to add sensor number
      } else if (input$dict_set == "era5_to_icos") {
        dict_tbl <- dictionary_list[[input$dict_set]]
        dict_cols <- grep("_name$", names(dict_tbl), value = TRUE)
        dict_choices <- unlist(dict_tbl[dict_cols[1]])

        tagList(
          lapply(cols, function(col) {
            all_choices <- unique(c(col, dict_choices))

            fluidRow(
              column(3, strong(col)),
              column(3,
                     selectInput(ns(paste0("map_", col)), NULL,
                                 choices = all_choices, selected = col)
              ),
              # allows users to add sensor number to the column name
              column(3,
                     checkboxInput(ns(paste0("add_sensor_", col)),
                                   "Add sensor number", value = FALSE)
              ),
              column(3,
                     # Conditional numeric boxes (only if user ticks the box to add the sensor num)
                     conditionalPanel(
                       condition = sprintf("input['%s']", ns(paste0("add_sensor_", col))),
                       fluidRow(
                         column(4, numericInput(ns(paste0("era5_box1_", col)), NULL, value = 1, min = 1)),
                         column(4, numericInput(ns(paste0("era5_box2_", col)), NULL, value = 1, min = 1)),
                         column(4, numericInput(ns(paste0("era5_box3_", col)), NULL, value = 1, min = 1))
                       )
                     )
              )
            )
          })
        )

      # Mapping UI for the custom dictionary. displays a list of long name and units
      } else {
        req("long_name" %in% names(dictionary_list))
        req("all_units" %in% names(dictionary_list))
        long_names <- dictionary_list$long_name$longname
        units <- dictionary_list$all_units$unit

        tagList(
          h4("Custom mapping"),
          fluidRow(
            column(4, strong("Original Name")),
            column(4, strong("Long Name")),
            column(4, strong("Units"))
          ),
          lapply(cols, function(col) {
            fluidRow(
              column(4, strong(col)),
              column(4,
                     selectInput(ns(paste0("map_long_", col)), NULL,
                                 choices = c(col, long_names), selected = col)),
              column(4,
                     selectInput(ns(paste0("map_unit_", col)), NULL,
                                 choices = c("", units), selected = ""))
            )
          })
        )
      }
    })

    # Reactive live mapping (handles normal, custom, and era5_to_icos)
    live_mapped <- reactive({
      req(uploaded()$data)
      df <- uploaded()$data
      validate(need(is.data.frame(df) && ncol(df) > 0, "No data available for mapping"))
      if (is.null(input$dict_set)) return(df)

      # Custom mapping
      if (input$dict_set == "Custom") {
        new_names <- sapply(names(df), function(col) {
          long_name <- input[[paste0("map_long_", col)]]
          unit_name <- input[[paste0("map_unit_", col)]]

          if (!is.null(long_name) && !is.null(unit_name) && unit_name != "") {
            paste0(long_name, " [", unit_name, "]")
          } else if (!is.null(long_name)) {
            long_name
          } else {
            col
          }
        }, USE.NAMES = FALSE)

      # era5_to_icos special handling: concatenates and add numeric boxes
      # for sensors
      } else if (input$dict_set == "era5_to_icos") {
        new_names <- sapply(names(df), function(col) {
          # base mapped name (or original col if missing)
          base_name <- if (!is.null(input[[paste0("map_", col)]])) input[[paste0("map_", col)]] else col
          add_sensor <- isTRUE(input[[paste0("add_sensor_", col)]])

          if (add_sensor) {
            b1 <- input[[paste0("era5_box1_", col)]]
            b2 <- input[[paste0("era5_box2_", col)]]
            b3 <- input[[paste0("era5_box3_", col)]]
            boxes <- c(b1, b2, b3)
            boxes <- boxes[!sapply(boxes, function(x) is.null(x) || is.na(x) || x == "")]
            if (length(boxes) > 0) {
              paste(c(base_name, as.character(boxes)), collapse = "_")
            } else base_name
          } else {
            base_name
          }
        }, USE.NAMES = FALSE)

      } else {
        new_names <- sapply(names(df), function(col) {
          if (!is.null(input[[paste0("map_", col)]])) input[[paste0("map_", col)]] else col
        }, USE.NAMES = FALSE)
      }

      # apply names - still not working
      if (length(new_names) == ncol(df)) {
        colnames(df) <- new_names
      }

      df
    })

    # Confirm mapping button
    observeEvent(input$confirm_mapping, {
      mapped_data(live_mapped())
      confirmed(TRUE)
    })

    # Reset mapping button (this also reset era5_to_icos numeric boxes for sensors)
    observeEvent(input$reset_mapping, {
      confirmed(FALSE)
      mapped_data(NULL)
      req(uploaded()$data)
      cols <- names(uploaded()$data)

      if (!is.null(input$dict_set) && input$dict_set == "Custom") {
        lapply(cols, function(col) {
          updateSelectInput(session, paste0("map_long_", col), selected = col)
          updateSelectInput(session, paste0("map_unit_", col), selected = "")
        })
      } else {
        lapply(cols, function(col) {
          # update map select if present
          try(updateSelectInput(session, paste0("map_", col), selected = col), silent = TRUE)
          # update era5 boxes if present
          try(updateNumericInput(session, paste0("era5_box1_", col), value = 1), silent = TRUE)
          try(updateNumericInput(session, paste0("era5_box2_", col), value = 1), silent = TRUE)
          try(updateNumericInput(session, paste0("era5_box3_", col), value = 1), silent = TRUE)
        })
      }
    })

    # Confirmation feedback text
    output$confirmation_text <- renderUI({
      if (confirmed()) {
        tags$div(style = "color: green; font-weight: bold;", "✅ Mapping confirmed!")
      } else {
        tags$div(style = "color: orange;", "⚠️ Mapping not confirmed yet")
      }
    })

    # Preview the table
    output$mapped_preview <- DT::renderDataTable({
      req(live_mapped())
      DT::datatable(head(live_mapped(), 10), options = list(scrollX = TRUE))
    })

    # Return final mapped data reactively (only when confirmed)
    return(
      reactive({
        if (confirmed()) mapped_data() else NULL
      })
    )
  })
}
