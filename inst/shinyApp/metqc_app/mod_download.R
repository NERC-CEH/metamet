mod_download_ui <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxGroupInput(
      ns("download_levels"),
      "Select datatable format:",
      choices = c("Long" = "lev1", "Wide" = "ceda"),
      selected = c("lev1", "ceda")
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
        "Daily" = "1 day",
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
    # warning if no level is selected
    output$levels_warning <- renderUI({
      msgs <- c()

      if (length(input$download_levels) == 0) {
        msgs <- c(msgs, "Please select at least one data product.")
      }

      if (length(msgs) > 0) {
        tags$div(
          style = "color:#b30000; font-size:0.9em;",
          HTML(paste(msgs, collapse = "<br>"))
        )
      }
    })

    # warning if no format is selected
    output$format_warning <- renderUI({
      if (length(input$download_formats) == 0) {
        tags$div(
          style = "color:#b30000; font-size: 0.9em;",
          "Please select at least one format."
        )
      }
    })

    # Warning if no averaging option selected
    output$avg_warning <- renderUI({
      if (length(input$download_avg) == 0) {
        tags$div(
          style = "color:#b30000; font-size:0.9em;",
          "Please select at least one averaging option."
        )
      }
    })

    # Disable download button unless at least one format, level, and averaging option are selected
    observe({
      if (
        length(input$download_formats) == 0 ||
          length(input$download_levels) == 0 ||
          length(input$download_avg) == 0
      ) {
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
        cat("Levels:", paste(input$download_levels, collapse = ", "), "\n")
        cat("Formats:", paste(input$download_formats, collapse = ", "), "\n")

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
                mm_in = mm_raw,
                avg.time = avg,
                report_end_interval = TRUE,
                extra_rows = 0
              )
              # reshape obj back to long format
              mm_avg <- metamet_reshape(mm_avg, "long")
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
              # copy so we do not modify the app's in-memory data.table by reference
              df_out <- data.table::copy(mm_avg$dt)
              prefix <- "long"
            } else if (lev == "ceda") {
              # format_for_ceda returns a data.table; copy to avoid modifying mm_avg
              df_out <- data.table::copy(metamet:::format_for_ceda(mm_avg))
              # --- DEBUG: inspect mm_avg dt / dt_qc when creating CEDA wide output ---
              cat("DEBUG: download -> CEDA debug\n")
              if (!is.null(mm_avg$dt_qc)) {
                cat(
                  "names(mm_avg$dt_qc):",
                  paste(names(mm_avg$dt_qc), collapse = ", "),
                  "\n"
                )
                cat(
                  "anyDuplicated(names(mm_avg$dt_qc)):",
                  anyDuplicated(names(mm_avg$dt_qc)),
                  "\n"
                )
              } else {
                cat("mm_avg$dt_qc is NULL\n")
              }
              cat(
                "names(mm_avg$dt):",
                paste(names(mm_avg$dt), collapse = ", "),
                "\n"
              )
              if (!is.null(mm_avg$dt_meta)) {
                cat(
                  "dt_meta$name_dt:",
                  paste(mm_avg$dt_meta$name_dt, collapse = ", "),
                  "\n"
                )
                cat(
                  "time variable (dt_meta[type=='time',name_dt]):",
                  paste(
                    unique(mm_avg$dt_meta[type == 'time', name_dt]),
                    collapse = ", "
                  ),
                  "\n"
                )
              }
              # --- end DEBUG ---
              prefix <- "wide"
            }

            if (is.null(df_out)) {
              cat("WARNING: df_out is NULL for", prefix, "- skipping\n")
              next
            }

            # remove internal cols if present
            if ("row_name" %in% names(df_out)) {
              df_out[, row_name := NULL]
            }
            if ("datect_num" %in% names(df_out)) {
              df_out[, datect_num := NULL]
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

            #########################################################
            # 3b. PROPAGATE QC + COMMENT FROM RAW TO AVERAGED LEVEL 1
            #########################################################
            if (
              lev == "lev1" &&
                avg != "none" &&
                "comment" %in% names(mm_raw$dt) &&
                "qc" %in% names(mm_raw$dt)
            ) {
              if (avg == "1 month") {
                cat("Applying MONTHLY QC/comment propagation...\n")
                raw_dt <- mm_raw$dt[
                  !is.na(qc) | !is.na(comment),
                  .(
                    site,
                    name_icos,
                    year_month = format(TIMESTAMP, "%Y-%m"),
                    qc_raw = qc,
                    comment_raw = comment
                  )
                ]
                monthly_raw <- raw_dt[,
                  .(
                    qc = if (all(is.na(qc_raw))) {
                      NA_integer_
                    } else {
                      max(qc_raw, na.rm = TRUE)
                    },
                    comment = {
                      cmt <- comment_raw[
                        !is.na(comment_raw) & comment_raw != ""
                      ]
                      if (length(cmt) > 0) cmt[1] else NA_character_
                    }
                  ),
                  by = .(site, name_icos, year_month)
                ]
                df_out[, year_month := format(TIMESTAMP, "%Y-%m")]
                data.table::setkey(df_out, site, name_icos, year_month)
                data.table::setkey(monthly_raw, site, name_icos, year_month)
                df_out[
                  monthly_raw,
                  `:=`(
                    qc = i.qc,
                    comment = i.comment
                  )
                ]
                df_out[, year_month := NULL]
              } else {
                cat(
                  "Applying QC/comment propagation for",
                  avg,
                  "averaging...\n"
                )

                # Map averaging string to lubridate duration
                avg_duration <- switch(
                  avg,
                  "1 day" = lubridate::ddays(1),
                  "1 week" = lubridate::dweeks(1),
                  "1 month" = lubridate::dmonths(1)
                )

                if (!is.null(avg_duration)) {
                  # Raw rows that actually have non‑zero QC or comments
                  raw_dt <- mm_raw$dt[
                    !is.na(qc) | !is.na(comment),
                    .(
                      site,
                      name_icos,
                      TIMESTAMP,
                      qc_raw = qc,
                      comment_raw = comment
                    )
                  ]

                  cat("Number of raw rows with qc/comment:", nrow(raw_dt), "\n")

                  if (nrow(raw_dt) > 0) {
                    # Build interval table for averaged rows
                    df_int <- data.table::copy(df_out)
                    df_int[, interval_start := TIMESTAMP - avg_duration]
                    df_int[, interval_end := TIMESTAMP]

                    df_int <- df_int[,
                      .(
                        site,
                        name_icos,
                        start = interval_start,
                        end = interval_end,
                        TIMESTAMP,
                        value,
                        type,
                        var_name,
                        qc,
                        comment
                      )
                    ]

                    # Raw as point intervals
                    raw_int <- raw_dt[,
                      .(
                        site,
                        name_icos,
                        start = TIMESTAMP,
                        end = TIMESTAMP,
                        qc_raw,
                        comment_raw
                      )
                    ]

                    # Set keys
                    data.table::setkey(df_int, site, name_icos, start, end)
                    data.table::setkey(raw_int, site, name_icos, start, end)

                    # Interval overlap join
                    joined <- data.table::foverlaps(
                      x = df_int,
                      y = raw_int,
                      type = "any",
                      nomatch = 0L
                    )

                    # Aggregate per averaged row:
                    # qc = max qc_raw (if any), comment = first non‑NA comment_raw
                    joined <- joined[,
                      .(
                        site = site,
                        name_icos = name_icos,
                        TIMESTAMP = TIMESTAMP,
                        var_name = var_name,
                        value = value,
                        type = type,
                        qc = if (all(is.na(qc_raw))) {
                          qc[1]
                        } else {
                          max(qc_raw, na.rm = TRUE)
                        },
                        comment = {
                          cmt <- comment_raw[
                            !is.na(comment_raw) & comment_raw != ""
                          ]
                          if (length(cmt) > 0) cmt[1] else comment[1]
                        }
                      ),
                      by = .(site, name_icos, TIMESTAMP)
                    ]

                    # Put back into df_out (aligned by site, name_icos, TIMESTAMP)
                    data.table::setkey(df_out, site, name_icos, TIMESTAMP)
                    data.table::setkey(joined, site, name_icos, TIMESTAMP)

                    df_out[
                      joined,
                      `:=`(
                        qc = i.qc,
                        comment = i.comment
                      )
                    ]
                  }
                }
              }
            }

            # formats timestamp for monthly avg
            if (avg == "1 month") {
              df_out[, TIMESTAMP := format(TIMESTAMP, "%Y-%m")]
            }
            # formats timestamp for daily/weekly avg to ISO date (YYYY-MM-DD)
            if (avg %in% c("1 day", "1 week")) {
              # Convert TIMESTAMP to POSIXct in GMT and format as ISO date to avoid locale-specific formats
              df_out[,
                TIMESTAMP := format(
                  as.POSIXct(TIMESTAMP, tz = "GMT"),
                  "%Y-%m-%d"
                )
              ]
            }

            # -------------------------------------------------------
            # 4. WRITE FILES (include averaging in filename)
            # -------------------------------------------------------
            avg_tag <- switch(
              avg,
              "none" = "raw",
              "1 day" = "daily",
              "1 week" = "weekly",
              "1 month" = "monthly"
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
        zip_result <- try(
          zip(zipfile = file, files = files_to_zip),
          silent = TRUE
        )

        cat("zip() result class:", class(zip_result), "\n")
        print(zip_result)
        cat("Zipfile exists after zip():", file.exists(file), "\n")
        cat("=== DOWNLOAD DEBUG END ===\n")
      }
    )
  })
}
