# Machine faults invalidation from excel file

mod_machine_faults_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

mod_machine_faults_server <- function(id, mm_qry, username) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_faults_modal, {
      showModal(modalDialog(
        title = "Batch invalidate data",
        fileInput(
          session$ns("faults_excel"),
          "Upload file (.xlsx, .xls)",
          accept = c(".xlsx", ".xls")
        ),
        uiOutput(session$ns("faults_preview")),
        actionButton(
          session$ns("apply_batch_invalidation"),
          "Apply invalidation"
        ),
        footer = modalButton("Close"),
        size = "l",
        easyClose = TRUE
      ))
    })

    # Load excel file and preview
    observeEvent(input$faults_excel, {
      req(input$faults_excel)
      faults <<- readxl::read_excel(input$faults_excel$datapath,
                                    sheet = "QC")
      # read sheet as data table
      faults_dt <<- data.table::as.data.table(
        readxl::read_excel(input$faults_excel$datapath, sheet = "QC")
      )
      # Required cols
      required_cols <- c("start_time", "end_time", "var_name", "comment")
      missing_cols <- setdiff(required_cols, names(faults))
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return()
      }
      # Convert date columns
      faults$start_time <- as.POSIXct(faults$start_time, tz = "UTC")
      faults$end_time <- as.POSIXct(faults$end_time, tz = "UTC")
      # Preview table
      output$faults_preview <- renderUI({
        tableOutput(session$ns("faults_table"))
      })
      output$faults_table <- renderTable(faults)
    })

    # apply invalidation to metamet obj
    observeEvent(input$apply_batch_invalidation, {
      if (is.null(mm_qry) || is.null(mm_qry$dt)) {
        showNotification(
          "Please retrieve data before applying batch invalidation.",
          type = "error"
        )
        return()
      }
      req(exists("faults_dt"))
      dt <- mm_qry$dt

      # Ensure comment column exists
      if (!"comment" %in% names(dt)) {
        dt[, comment := NA_character_]
      }

      for (i in seq_len(nrow(faults))) {
        start <- faults$start_time[i]
        end   <- faults$end_time[i]
        var   <- faults$var_name[i]
        note  <- faults$comment[i]

        dt[
          var_name == var &
          #name_icos == var &
            TIMESTAMP >= start &
            TIMESTAMP <= end,
          `:=`(
            qc = 1L,
            comment = paste0(
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              " - Batch invalidation: ", note
            ),
            validator = username
          )
        ]
      }

      mm_qry$dt <<- dt
      showNotification("Batch invalidation applied.", type = "message")
    })
  })
}
