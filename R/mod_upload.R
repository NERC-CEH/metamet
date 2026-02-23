#' Upload Module UI
#' @param id shiny id
#' @return UI element for file uploads
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Upload data"),
    fileInput(ns("rds_file"), "Upload your file(s)"),
    fluidRow(
      column(
        12,
        actionButton(
          ns("load_preview"),
          "Load and preview file(s)",
          class = "btn-primary"
        )
      ),
      # if users change their mind they can remove file
      column(
        12,
        actionButton(ns("remove_file"), "Remove file", class = "btn-danger")
      )
    ),
    br(),
    br(),
    uiOutput(ns("file_preview_ui"))
  )
}

#' Upload Module Server
#' @param id shiny id
#' @return reactive list with $mm (uploaded metamet object)
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_rv <- reactiveValues(mm = NULL)

    observeEvent(input$load_preview, {
      # If no file selected, message will appear
      if (is.null(input$rds_file)) {
        showNotification(
          "No file selected. Please choose a file to upload.",
          type = "warning"
        )
        return()
      }

      #req(input$rds_file)

      # get the file path
      file_path <- input$rds_file$datapath

      # determine file extension
      file_ext <- tools::file_ext(file_path)

      data_rv$mm <- tryCatch(
        {
          if (tolower(file_ext) %in% c("rds")) {
            readRDS(file_path)
          } else {
            stop(
              "Unsupported file type. Please upload an .rds (or .qs)? file."
            )
          }
        },
        error = function(e) {
          showNotification(
            paste("❌ Failed to read file:", e$message),
            type = "error"
          )
          return(NULL)
        }
      )

      req(data_rv$mm)
      showNotification("✅ File loaded successfully!", type = "message")
    })

    # if users want to remove or change the file
    observeEvent(input$remove_file, {
      data_rv$mm <- NULL

      # reset the fileInput control UI using shinyjs (ensure useShinyjs() in UI)
      try(
        {
          shinyjs::reset("rds_file")
        },
        silent = TRUE
      )

      showNotification(
        "🗑️ File removed. You can upload a new one.",
        type = "warning"
      )
    })

    # part of the UI which gives a preview of the uploaded data
    output$file_preview_ui <- renderUI({
      req(data_rv$mm)
      tagList(
        h4("Preview of uploaded data"),
        DT::dataTableOutput(ns("data_preview"))
      )
    })

    # data preview
    output$data_preview <- DT::renderDataTable({
      req(data_rv$mm)
      DT::datatable(head(data_rv$mm$dt, 10), options = list(scrollX = TRUE))
    })

    # Returns the uploaded data
    return(
      reactive({
        list(mm = data_rv$mm)
      })
    )
  })
}
