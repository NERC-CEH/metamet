mod_download_ui <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxGroupInput(
      ns("download_levels"),
      "Select data products:",
      choices = c("Level 1" = "lev1", "Level 2" = "lev2", "CEDA" = "ceda"),
      selected = c("lev1", "lev2", "ceda")
    ),

    checkboxGroupInput(
      ns("download_formats"),
      "Select formats:",
      choices = c("CSV" = "csv", "RDS" = "rds"),
      selected = c("csv", "rds")
    ),

    downloadButton(ns("download_zip"), "Download ZIP")
  )
}

mod_download_server <- function(id, mm_final) {
  moduleServer(id, function(input, output, session) {

    output$download_zip <- downloadHandler(

      filename = function() {
        paste0("processed_data_", Sys.Date(), ".zip")
      },

      content = function(file) {

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Preparing download...", value = 0)

        tmpdir <- tempdir()
        oldwd <- setwd(tmpdir)
        on.exit(setwd(oldwd), add = TRUE)

        files_to_zip <- c()

        mm <- mm_final()   # final imputed + QC-propagated object

        # Loop over selected levels
        for (lev in input$download_levels) {

          # 1. Select dataset
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

          # 2. Write CSV
          if ("csv" %in% input$download_formats) {
            fname_csv <- paste0(prefix, ".csv")
            data.table::fwrite(df_out, fname_csv)
            files_to_zip <- c(files_to_zip, fname_csv)
          }

          # 3. Write RDS
          if ("rds" %in% input$download_formats) {
            fname_rds <- paste0(prefix, ".rds")
            saveRDS(df_out, fname_rds)
            files_to_zip <- c(files_to_zip, fname_rds)
          }

          progress$inc(1 / length(input$download_levels), detail = paste("Processed", prefix))
        }

        # 4. Create ZIP
        zip(zipfile = file, files = files_to_zip)
      }
    )
  })
}
