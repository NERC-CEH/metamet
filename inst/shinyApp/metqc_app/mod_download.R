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

    checkboxGroupInput(
      ns("download_avg"),
      "Averaging for download:",
      choices = c(
        "Raw (no averaging)" = "none",
        "Daily"  = "1 day",
        "Weekly" = "1 week",
        "Monthly" = "1 month"
      ),
      selected = "none"
    ),
    uiOutput(ns("avg_warning")),
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

    # Warning if no averaging option selected
    output$avg_warning <- renderUI({
      if (length(input$download_avg) == 0) {
        tags$div(
          style="color:#b30000; font-size:0.9em;",
          "Please select at least one averaging option."
        )
      }
    })

    # Disable download button unless at least one format and level are selected
    observe({
      if (length(input$download_formats) == 0 ||
          length(input$download_levels) == 0 ||
          length(input$download_avg) == 0) {
        shinyjs::disable("download_zip")
      } else {
        shinyjs::enable("download_zip")
      }
    })

    # download handler
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
        mm_raw <- mm_final()

        # APPLY DOWNLOAD‑TIME AVERAGING (if selected)
        for (lev in input$download_levels) {
          for (avg in input$download_avg) {

            cat("\n=== PROCESSING:", lev, "with averaging:", avg, "===\n")

            # -------------------------------------------------------
            # 1. APPLY AVERAGING (if not "none")
            # -------------------------------------------------------
            if (avg != "none") {

              cat("Applying metamet::time_average()...\n")

              mm_avg <- metamet::time_average(
                mm_in    = mm_raw,
                avg.time = avg,
                report_end_interval = TRUE,
                extra_rows = 0
              )
              # reshape obj back to long format
              mm_avg <- metamet_reshape(mm_avg, "long")
              # debug
              cat("Averaging complete. Structure:\n")
              print(str(mm_avg))
            } else {
              cat("No averaging selected — using raw QC‑corrected data.\n")
              mm_avg <- mm_raw
            }

            # -------------------------------------------------------
            # 2. EXTRACT LEVEL
            # -------------------------------------------------------
            if (lev == "lev1") {
              df_out <- mm_avg$dt
              prefix <- "level1"

            } else if (lev == "lev2") {
              df_out <- mm_avg$dt_qc
              prefix <- "level2"

            } else if (lev == "ceda") {
              df_out <- metamet:::format_for_ceda(mm_avg)
              prefix <- "ceda"
            }

            if (is.null(df_out)) {
              cat("WARNING: df_out is NULL for", prefix, "- skipping\n")
              next
            }

            # -------------------------------------------------------
            # 3. DEBUGGING
            # -------------------------------------------------------
            cat("\n--- DEBUG df_out for", prefix, "avg =", avg, "---\n")
            cat("class(df_out):", class(df_out), "\n")
            cat("is.data.frame:", is.data.frame(df_out), "\n")
            cat("is.data.table:", data.table::is.data.table(df_out), "\n")
            print(utils::str(df_out))
            cat("--- END DEBUG ---\n\n")

            # propagate comments from raw to averaged data if lev1 and averaging is applied
            if (lev == "lev1" &&
                avg != "none" &&
                "comment" %in% names(mm_raw$dt)) {

              # Map averaging string to lubridate duration
              avg_duration <- switch(
                avg,
                "1 day"  = lubridate::ddays(1),
                "1 week" = lubridate::dweeks(1),
                "1 month"= lubridate::dmonths(1)
              )

              if (!is.null(avg_duration)) {

                # Raw rows that actually have comments
                comments_raw <- mm_raw$dt[
                  !is.na(comment),
                  .(site, name_icos, raw_time = TIMESTAMP, comment)
                ]

                cat("Number of raw commented rows:", nrow(comments_raw), "\n")

                if (nrow(comments_raw) > 0) {

                  # Compute interval start for each averaged row
                  df_out[, interval_start := TIMESTAMP - avg_duration]

                  df_out[comments_raw,
                         on = .(
                           site,
                           name_icos,
                           interval_start <= raw_time,
                           TIMESTAMP      >= raw_time
                         ),
                         comment := i.comment
                  ]

                  # Drop helper column
                  df_out[, interval_start := NULL]
                }
              }
            }
            ##################################

            # -------------------------------------------------------
            # 4. WRITE FILES (include averaging in filename)
            # -------------------------------------------------------
            avg_tag <- switch(
              avg,
              "none"   = "raw",
              "1 day"  = "daily",
              "1 week" = "weekly",
              "1 month"= "monthly"
            )

            if ("csv" %in% input$download_formats) {
              fname_csv <- paste0(prefix, "_", avg_tag, ".csv")
              data.table::fwrite(df_out, fname_csv)
              files_to_zip <- c(files_to_zip, fname_csv)
            }

            if ("rds" %in% input$download_formats) {
              fname_rds <- paste0(prefix, "_", avg_tag, ".rds")
              saveRDS(df_out, fname_rds)
              files_to_zip <- c(files_to_zip, fname_rds)
            }

            progress$inc(
              1 / (length(input$download_levels) * length(input$download_avg)),
              detail = paste("Processed", prefix, avg_tag)
            )
          }
        }

        # -------------------------------------------------------
        # 5. ZIP EVERYTHING
        # -------------------------------------------------------
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
