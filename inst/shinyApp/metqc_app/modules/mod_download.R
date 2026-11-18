#' Download module UI
#'
#' User interface for downloading processed data in different formats.
#'
#' @param id Module id.
#'
#' @return A Shiny UI element.
#' @noRd
mod_download_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "download",
    fluidRow(
      box(
        id = ns("download_box"),
        title = "Data download",
        status = "success",
        solidHeader = TRUE,
        checkboxGroupInput(
          ns("download_levels"),
          "Select data product:",
          choices = c(
            "Long (Level 2)" = "lev2",
            "Wide (CEDA)" = "ceda"
          ),
          selected = c("lev2", "ceda")
        ),
        uiOutput(ns("levels_warning")),
        checkboxGroupInput(
          ns("download_formats"),
          "Select format:",
          choices = c(
            "RDS" = "rds",
            "CSV" = "csv"
          ),
          selected = "rds"
        ),
        tags$div(
          style = paste(
            "margin-top:-10px;",
            "margin-left:5px;",
            "color:#666;",
            "font-size:0.9em;"
          ),
          "RDS is recommended for reloading long-format QC data into the app."
        ),
        uiOutput(ns("format_warning")),
        downloadButton(
          ns("download_zip"),
          "Download ZIP"
        )
      )
    )
  )
}

#' Download module server
#'
#' Server logic for exporting processed data as ZIP archives
#' containing Level 2 and CEDA data products.
#'
#' @param id Module id.
#' @param uploaded Reactive containing the uploaded metamet object.
#' @param edited_data Reactive containing the edited query results.
#'
#' @return No return value. Called for its side effects.
#' @noRd
mod_download_server <- function(id, uploaded, edited_data) {
  moduleServer(id, function(input, output, session) {
    output$levels_warning <- renderUI({
      if (length(input$download_levels) == 0L) {
        tags$div(
          style = "color:#b30000; font-size:0.9em;",
          "Please select at least one data product."
        )
      }
    })

    output$format_warning <- renderUI({
      if (length(input$download_formats) == 0L) {
        tags$div(
          style = "color:#b30000; font-size:0.9em;",
          "Please select at least one format."
        )
      }
    })

    observe({
      valid_selection <- length(input$download_levels) > 0L &&
        length(input$download_formats) > 0L

      if (valid_selection) {
        shinyjs::enable(session$ns("download_zip"))
      } else {
        shinyjs::disable(session$ns("download_zip"))
      }
    })

    output$download_zip <- downloadHandler(
      filename = function() {
        paste0("processed_data_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(uploaded())
        req(edited_data())
        req(length(input$download_levels) > 0L)
        req(length(input$download_formats) > 0L)

        progress <- shiny::Progress$new()
        on.exit(progress$close(), add = TRUE)

        total_steps <- length(input$download_levels) *
          length(input$download_formats)

        progress$set(
          message = "Preparing download...",
          value = 0
        )

        mm_qry_save <- data.table::copy(edited_data())

        internal_columns <- intersect(
          c("row_name", "datect_num"),
          names(mm_qry_save$dt)
        )

        if (length(internal_columns) > 0L) {
          mm_qry_save$dt[, (internal_columns) := NULL]
        }

        # Recombine the edited query with the original full object.
        mm_final <- metamet::join(
          uploaded()$mm,
          mm_qry_save
        )

        withr::with_dir(tempdir(), {
          files_to_zip <- character()
          progress_step <- 0

          for (level in input$download_levels) {
            if (level == "lev2") {
              level_data <- data.table::copy(mm_final$dt)
              level_name <- "level_2"
            } else if (level == "ceda") {
              level_data <- data.table::copy(
                metamet:::format_for_ceda(mm_final)
              )
              level_name <- "ceda"
            } else {
              next
            }

            if ("row_name" %in% names(level_data)) {
              level_data[, row_name := NULL]
            }

            if ("datect_num" %in% names(level_data)) {
              level_data[, datect_num := NULL]
            }

            for (format in input$download_formats) {
              if (level == "lev2" && format == "rds") {
                filename <- "level_2.rds"

                # Save the complete object so that it can be reloaded
                # into the application.
                saveRDS(mm_final, filename)

                files_to_zip <- c(files_to_zip, filename)
              }

              if (level == "lev2" && format == "csv") {
                data_filename <- "level_2-data.csv"
                qc_filename <- "level_2-qc.csv"

                data.table::fwrite(
                  level_data,
                  data_filename
                )

                qc_data <- mm_final$dt_qc

                # dt_qc is optional. Derive a QC table from dt when
                # QC information is stored in the long data table.
                if (is.null(qc_data)) {
                  qc_source <- data.table::copy(mm_final$dt)

                  if (!"qc" %in% names(qc_source)) {
                    qc_source[, qc := NA_integer_]
                  }

                  qc_columns <- intersect(
                    c(
                      "TIMESTAMP",
                      "site",
                      "name_icos",
                      "var_name",
                      "qc",
                      "validator"
                    ),
                    names(qc_source)
                  )

                  if (length(qc_columns) == 0L) {
                    stop(
                      "Could not create a QC export from the metamet object.",
                      call. = FALSE
                    )
                  }

                  qc_data <- qc_source[, ..qc_columns]
                }

                data.table::fwrite(
                  qc_data,
                  qc_filename
                )

                files_to_zip <- c(
                  files_to_zip,
                  data_filename,
                  qc_filename
                )
              }

              if (level == "ceda" && format == "rds") {
                filename <- "ceda-data.rds"

                saveRDS(level_data, filename)

                files_to_zip <- c(files_to_zip, filename)
              }

              if (level == "ceda" && format == "csv") {
                filename <- "ceda-data.csv"

                data.table::fwrite(level_data, filename)

                files_to_zip <- c(files_to_zip, filename)
              }

              progress_step <- progress_step + 1L

              progress$set(
                value = progress_step / total_steps,
                detail = paste("Processed", level_name, format)
              )
            }
          }

          if (length(files_to_zip) == 0L) {
            stop(
              "No files were generated for the selected download options.",
              call. = FALSE
            )
          }

          utils::zip(
            zipfile = file,
            files = unique(files_to_zip)
          )
        })

        progress$set(
          message = "Download ready.",
          value = 1
        )
      }
    )
  })
}
