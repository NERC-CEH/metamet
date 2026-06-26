mod_download_ui <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxGroupInput(
      ns("download_levels"),
      "Select data products:",
      choices = c("Level 1" = "lev1", "Level 2" = "lev2", "CEDA" = "ceda"),
      selected = c("lev1", "lev2", "ceda")
    ),
    uiOutput(ns("levels_warning")),

    checkboxGroupInput(
      ns("download_formats"),
      "Select formats:",
      choices = c("CSV" = "csv", "RDS" = "rds"),
      selected = c("csv", "rds")
    ),
    uiOutput(ns("format_warning")),
    downloadButton(ns("download_zip"), "Download ZIP")
  )
}

mod_download_server <- function(id, mm_final) {
  moduleServer(id, function(input, output, session) {
    # warning if no level is selected or if a dataset is not available
    output$levels_warning <- renderUI({
      msgs <- c()

      if (length(input$download_levels) == 0) {
        msgs <- c(msgs, "Please select at least one data product.")
      }

      if ("lev2" %in% input$download_levels && is.null(mm_final()$dt_qc)) {
        msgs <- c(msgs, "Level 2 data is not available for this dataset.")
      }

      if (length(msgs) > 0) {
        tags$div(style="color:#b30000; font-size:0.9em;", HTML(paste(msgs, collapse="<br>")))
      }
    })

    # warning if no format is selected
    output$format_warning <- renderUI({
      if (length(input$download_formats) == 0) {
        tags$div(style="color:#b30000; font-size: 0.9em;",
                 "Please select at least one format.")
      }
    })

    # Disable download button unless at least one format and level are selected
    observe({
      if (length(input$download_formats) == 0 ||
          length(input$download_levels) == 0) {
        shinyjs::disable("download_zip")
      } else {
        shinyjs::enable("download_zip")
      }
    })
    output$download_zip <- downloadHandler(
      filename = function() {
        paste0("processed_data_", Sys.Date(), ".zip")
      },
      content = function(file) {

        cat("=== DOWNLOAD DEBUG START ===\n")
        cat("Levels:", paste(input$download_levels, collapse=", "), "\n")
        cat("Formats:", paste(input$download_formats, collapse=", "), "\n")

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Preparing download...", value = 0)

        tmpdir <- tempdir()
        oldwd <- setwd(tmpdir)
        on.exit(setwd(oldwd), add = TRUE)

        files_to_zip <- c()
        mm <- mm_final()

        for (lev in input$download_levels) {

          if (lev == "lev1") {
            df_out <- mm$dt
            prefix <- "level1"
          } else if (lev == "lev2") {
            df_out <- mm$dt_qc
            prefix <- "level2"
          } else if (lev == "ceda") {
            df_out <- metamet:::format_for_ceda(mm)
            prefix <- "ceda"
          }

          # Skip if dataset is missing
          if (is.null(df_out)) {
            cat("WARNING: df_out is NULL for", prefix, "- skipping\n")
            next
          }

          ## debugging on objects class
          cat("\n--- DEBUG df_out for", prefix, "---\n")
          cat("class(df_out):", class(df_out), "\n")
          cat("is.data.frame:", is.data.frame(df_out), "\n")
          cat("is.data.table:", data.table::is.data.table(df_out), "\n")
          cat("str(df_out):\n")
          print(utils::str(df_out))
          cat("--- END DEBUG ---\n\n")
          ## 🔍 END DEBUGGING BLOCK


          # CSV
          if ("csv" %in% input$download_formats) {
            fname_csv <- paste0(prefix, ".csv")
            if (file.exists(fname_csv)) cat("WARNING: CSV exists:", fname_csv, "\n")
            data.table::fwrite(df_out, fname_csv)
            files_to_zip <- c(files_to_zip, fname_csv)
          }

          # RDS
          if ("rds" %in% input$download_formats) {
            fname_rds <- paste0(prefix, ".rds")
            if (file.exists(fname_rds)) cat("WARNING: RDS exists:", fname_rds, "\n")
            saveRDS(df_out, fname_rds)
            files_to_zip <- c(files_to_zip, fname_rds)
          }

          progress$inc(1 / length(input$download_levels), detail = paste("Processed", prefix))
        }

        cat("Files to zip:\n")
        print(files_to_zip)
        cat("Existence:\n")
        print(file.exists(files_to_zip))

        cat("Running zip()...\n")
        zip_result <- try(zip(zipfile = file, files = files_to_zip), silent = TRUE)

        cat("zip() result class:", class(zip_result), "\n")
        print(zip_result)
        cat("Zipfile exists after zip():", file.exists(file), "\n")
        cat("=== DOWNLOAD DEBUG END ===\n")
      }
    )
  })
}
