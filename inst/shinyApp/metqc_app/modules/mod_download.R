mod_download_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "download",
    fluidRow(
      box(
        id = "download_box",
        title = "Data download",
        status = "success",
        solidHeader = TRUE,
        selectInput(
          ns("download_file"),
          "Data to download:",
          choices = c(
            "Level 1" = "lev1",
            "Level 2" = "lev2",
            "CEDA" = "ceda"
          )
        ),
        downloadButton(
          ns("download_data"),
          label = "Download"
        )
      )
    )
  )
}

mod_download_server <- function(id, uploaded, edited_data) {
  moduleServer(id, function(input, output, session) {
    output$download_data <- downloadHandler(
      filename = function() {
        switch(
          input$download_file,
          lev1 = paste0("level_1-", Sys.Date(), ".zip"),
          lev2 = paste0("level_2-", Sys.Date(), ".zip"),
          ceda = paste0("ceda-", Sys.Date(), ".zip")
        )
      },
      content = function(file) {
        req(uploaded())
        req(edited_data())

        mm_qry_save <- data.table::copy(edited_data())
        mm_qry_save$dt[, c("row_name", "datect_num") := NULL]

        mm <- join(uploaded()$mm, mm_qry_save)

        if (input$download_file == "lev2") {
          runjs(
            paste0(
              "document.getElementById('",
              session$ns("download_data"),
              "').textContent='Preparing download...';"
            )
          )

          shinyjs::disable("download_data")

          withr::with_dir(tempdir(), {
            files <- c("level_2-data.csv", "level_2-qc.csv")

            data.table::fwrite(mm$dt, files[1])
            data.table::fwrite(mm$dt_qc, files[2])

            utils::zip(zipfile = file, files = files)
          })

          shinyjs::enable("download_data")
        }

        if (input$download_file == "ceda") {
          runjs(
            paste0(
              "document.getElementById('",
              session$ns("download_data"),
              "').textContent='Preparing download...';"
            )
          )

          shinyjs::disable("download_data")

          withr::with_dir(tempdir(), {
            filename <- "ceda-data.csv"
            df_ceda <- metamet:::format_for_ceda(mm)

            data.table::fwrite(df_ceda, filename)
            utils::zip(zipfile = file, files = filename)
          })

          shinyjs::enable("download_data")
        }
      }
    )
  })
}
