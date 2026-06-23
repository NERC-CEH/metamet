
mod_time_average_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("time_avg"),
      label = "Time averaging",
      choices = c(
        "None" = "none",
        "Hourly" = "hour",
        "Daily" = "day",
        "Weekly" = "week",
        "Monthly" = "month"
      ),
      selected = "none",
      width = "200px"
    ),
    uiOutput(ns("banner"))
  )
}

mod_time_average_server <- function(id, mm_qry_in, daterange_reactive) {
  moduleServer(id, function(input, output, session) {

    # Return a reactive metamet object (averaged or raw)
    averaged_mm <- reactive({

      req(mm_qry_in())

      mm_raw <- mm_qry_in()

      if (input$time_avg == "none") {
        return(mm_raw)
      }

      mm_avg <- metamet::time_average(
        mm_in = mm_raw,
        avg.time = input$time_avg,
        report_end_interval = TRUE
      )

      # Convert back to long format so the rest of the app works
      mm_avg <- metamet_reshape(mm_avg, "long")

      # Rebuild helper columns for plotting
      data.table::setkeyv(mm_avg$dt, c("name_icos", "site", "TIMESTAMP"))
      mm_avg$dt[, row_name := as.factor(rownames(mm_avg$dt))]
      mm_avg$dt$datect_num <- as.numeric(mm_avg$dt$TIMESTAMP)

      mm_avg
    })

    # Banner showing which averaging is active
    output$banner <- renderUI({
      req(input$time_avg != "none")
      div(
        style = "padding:6px; background:#e6ffe6; border-left:4px solid #2e7d32;",
        strong("Time averaging applied by: "), input$time_avg
      )
    })

    return(averaged_mm)
  })
}
