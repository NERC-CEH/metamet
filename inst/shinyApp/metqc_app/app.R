# install.packages("pak")
# library(pak)
# pak::pak("NERC-CEH/metamet")

# here::i_am("R/metqc_app.R")
library(metamet)
library(here)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggiraph)
# library(ROracle)
library(mgcv)
library(DT)
library(data.table)
library(lubridate)
library(ggExtra)
library(openair)
library(powerjoin)
library(pins)
library(glue)
library(shinycssloaders)
library(shinyalert)
library(stringr)
library(forcats)
library(shinyvalidate)
library(markdown)
# source(here("R", "imputation.R"))
# source(here("R", "plotting.R"))
# source(here("R", "metqc_app.R"))

# # to run
# rm(list=ls(all=TRUE))
# devtools::load_all()
# metqcApp()
Sys.setenv(DBNAME = "budbase.nerc-bush.ac.uk/BUA")
Sys.setenv(DBUID = "BU_FIELD_SITES")
Sys.setenv(DBPWD = "0ig2mtYUL9")
Sys.getenv("DBUID")

# metqcApp <- function(...) {
# Reading in the gap-filling methods and codes----
v_names <- readRDS(file = here("data", "v_mainmet_name.rds"))

gf_choices <- setNames(df_method$method, df_method$method_longname)

# Define UI for the app
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Met Data Validation",
    tags$li(
      class = "dropdown",
      actionLink(
        "change_user",
        textOutput('user_name_text'),
        style = "font-weight: bold;color:white;"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem("Dashboard", tabName = "dashboard", icon = icon('database')),
      menuItem("Download", tabName = "download", icon = icon('download')),
      menuItem(
        "Information",
        tabName = "information",
        icon = icon('info'),
        menuSubItem('Gap-fill methods', tabName = 'gapfill_guide'),
        menuSubItem('App guide', tabName = 'app_guide'),
        menuSubItem('Data process guide', tabName = 'data_guide')
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Data Selection",
            status = "success",
            solidHeader = TRUE,
            helpText(
              "Select your required processing start and end times below."
            ),
            column(
              width = 6,
              uiOutput("start_date") #,
              #textOutput("date_warning")
            ),
            column(
              width = 3,
              numericInput(
                "shour",
                value = 00,
                label = "Hour (24 hour)",
                min = 0,
                max = 23,
                step = 1
              )
            ),
            column(
              width = 3,
              numericInput(
                "smin",
                value = 00,
                label = "Minute",
                min = 0,
                max = 59,
                step = 1
              )
            ),
            column(
              width = 6,
              uiOutput("end_date"),
              tags$style(HTML(".datepicker {z-index:99999 !important;}"))
            ),
            column(
              width = 3,
              numericInput(
                "ehour",
                value = 00,
                label = "Hour  (24 hour)",
                min = 0,
                max = 23,
                step = 1
              )
            ),
            column(
              width = 3,
              numericInput(
                "emin",
                value = 00,
                label = "Minute",
                min = 0,
                max = 59,
                step = 1
              )
            ),
            actionButton("retrieve_data", "Retrieve from database"),
            actionButton("compare_vars", "Compare variables"),
          ),
          hidden(
            div(
              id = "validation_calendar_outer",
              box(
                id = 'validation_calendar',
                title = "Validation Calendar",
                status = "success",
                solidHeader = TRUE,
                shinycssloaders::withSpinner(plotOutput("heatmap_plot"))
              )
            )
          )
        ),
        hidden(
          fluidRow(
            id = "extracted_data",
            box(
              title = "Extracted Data",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              shinycssloaders::withSpinner(uiOutput("mytabs")),
              selectInput(
                "select_imputation",
                label = h5("Gap-Filling Method"),
                choices = gf_choices
              ),
              actionButton("impute", label = "Impute selection"),
              actionButton(
                "finished_check",
                label = "Finished checking variable for date range."
              ),
              checkboxGroupInput(
                "qc_tokeep",
                "Do not alter data estimated by",
                choiceNames = df_method$method_longname,
                choiceValues = df_method$qc
              ),
              uiOutput("impute_extra_info"),
              actionButton("reset", label = "Restart app"),
              #actionButton("submitchanges", "Submit changes to file"),
              actionButton("submitchanges_cloud", "Submit changes")
            ),
          )
        ),
      ),
      tabItem(
        tabName = 'download',
        fluidRow(
          box(
            id = 'download_box',
            title = 'Data download',
            status = "success",
            solidHeader = TRUE,
            selectInput(
              'download_file',
              'Data to download:',
              choices = c(
                'Level 1' = 'lev1',
                'Level 2' = 'lev2',
                'CEDA' = 'ceda'
              )
            ),
            downloadButton('download_data', label = 'Download')
          )
        )
      ),
      tabItem(
        tabName = "information",
      ),
      tabItem(
        tabName = "gapfill_guide",
        includeMarkdown(here::here("vignettes/gap_fill_methods.md"))
      ),
      tabItem(
        tabName = "app_guide",
        includeMarkdown(here::here("vignettes/app_user_guide.md"))
      ) # ,
      # tabItem(
      #   tabName = "data_guide",
      #   includeHTML(
      #     here::here("vignettes/metdb_shiny_version.html")
      #   )
      # )
    )
  )
)

server <- function(input, output, session) {
  ##########################
  #shinyvalidate statements#
  #########################
  iv <- InputValidator$new()

  iv$add_rule("sdate", sv_required())
  iv$add_rule("edate", sv_required())
  iv$enable()

  # list of possible users - hard-coded for now
  v_usernames <- c(
    "plevy",
    "dunhar",
    "karung",
    "leav",
    "matj",
    "MauGre",
    "mcoy",
    "neimul",
    "sarle",
    "wilfinc",
    "jamcas"
  )

  # a modal dialog where the user can enter their user name.
  username_modal <- modalDialog(
    title = "Enter user name",
    selectInput('input_username', 'Select from:', v_usernames),
    easyClose = F,
    footer = tagList(
      actionButton("ok", "OK")
    )
  )

  # Show the model on start up ...
  showModal(username_modal)

  # allow user to trigger modal with button press
  observeEvent(input$change_user, {
    showModal(username_modal)
  })

  ###
  ##Observe event for shinyvalidate dates
  ##
  observeEvent(input$retrieve_data, label = "validator for dates", {
    if (!iv$is_valid()) {
      showModal(modalDialog("Please fill in both dates.", easyClose = TRUE))
      return()
    }

    if (input$edate < input$sdate) {
      showModal(modalDialog(
        title = "Invalid Dates",
        "⚠️ End date must not be earlier than start date.",
        easyClose = TRUE
      ))
    }
  })

  # output$date_warning <- renderText({
  #   browser()
  #   req(input$start_date, input$end_date, input$retrieve_data)
  #   if (input$end_date < input$start_date) {
  #     "Warning: End date must not be earlier than start date."
  #   } else {
  #     "" # No warning
  #   }
  # })

  observeEvent(input$ok, {
    removeModal()
    # save the username
    username <<- input$input_username
    output$user_name_text <- renderText({
      paste0('Current user:  ', input$input_username)
    })
  })

  disable('compare_vars')

  # Reading in Level 1 data from pin on Connect server
  system.time(mm1 <- readRDS(file = here("data-raw/UK-AMO", "mm1.rds")))
  # Read in the previously validated data from pin on Connect server
  system.time(mm2 <- readRDS(file = here("data-raw/UK-AMO", "mm2.rds")))
  dim(mm2$dt)
  dim(mm1$dt)

  # Here we join the existing Level 2 data with new Level 1 data.
  # Where records already exist in the Level 2 data, these are preserved
  # and only new Level 1 data is added to the resulting data frame.

  mm2 <- join(mm1, mm2)
  dim(mm2$dt)

  date_of_first_new_record <- as.POSIXct(Sys.Date() - 225, tz = "UTC")
  date_of_last_new_record <- max(mm1$dt$DATECT, na.rm = TRUE)

  # v_names <<- dbListFields(con, table_name)
  v_names_for_box <- v_names[
    !v_names %in%
      c("DATECT", "TIMESTAMP", "datect_num", "checked", "pred")
  ]

  v_names_checklist <- reactiveValues()

  # Format the dates for R----
  df_proc <- data.frame(
    start_date = "1995/01/01 00:00",
    end_date = "2026/12/31 00:00"
  )
  df_proc$start_date <- as.POSIXct(
    df_proc$start_date,
    format = "%Y/%m/%d %H:%M",
    tz = "UTC"
  )
  df_proc$end_date <- as.POSIXct(Sys.Date() - 2, tz = "UTC")
  #   df_proc$end_date,
  #   format = "%Y/%m/%d %H:%M", tz = "UTC"
  # )

  # Create a reactive element with the earliest start date
  first_start_date <- reactive({
    min(as.Date(df_proc$start_date))
  })

  # Create a reactive element with the latest end date
  last_end_date <- reactive({
    max(as.Date(df_proc$end_date))
  })

  # Create a date input for the user to select start date
  output$start_date <- renderUI({
    dateInput(
      "sdate",
      value = as.Date(date_of_first_new_record, tz = "UTC"),
      #value = as.Date(date_of_first_new_record, tz = "UTC"),
      min = first_start_date(),
      max = last_end_date(),
      label = "Start date"
    )
  })

  # Create a date input for the user to select end date
  output$end_date <- renderUI({
    dateInput(
      "edate",
      value = as.Date(date_of_last_new_record, tz = "UTC"),
      # value = as.Date(strptime("01/03/2022", "%d/%m/%Y"), tz = "UTC"),
      min = first_start_date(),
      max = last_end_date(),
      label = "End date"
    )
  })

  # Create a dataframe with the start and end dates,
  df_daterange <- eventReactive(input$retrieve_data, {
    start_date_ch <- paste(
      sprintf("%02d", day(input$sdate)),
      "/",
      sprintf("%02d", month(input$sdate)),
      "/",
      year(input$sdate),
      " ",
      sprintf("%02d", input$shour),
      ":",
      sprintf("%02d", input$smin),
      sep = ""
    )
    start_date <- as.POSIXct(
      strptime(start_date_ch, "%d/%m/%Y %H:%M"),
      tz = "UTC"
    )
    end_date_ch <- paste(
      sprintf("%02d", day(input$edate)),
      "/",
      sprintf("%02d", month(input$edate)),
      "/",
      year(input$edate),
      " ",
      sprintf("%02d", input$ehour),
      ":",
      sprintf("%02d", input$emin),
      sep = ""
    )
    end_date <- as.POSIXct(
      strptime(end_date_ch, "%d/%m/%Y %H:%M"),
      tz = "UTC"
    )
    list(
      start_date = start_date,
      end_date = end_date,
      start_date_ch = start_date_ch,
      end_date_ch = end_date_ch
    )
  })

  # The optional rendering of UI elements depending on which
  # imputation method has been selected
  output$impute_extra_info <- renderUI({
    req(input$select_imputation)
    if (input$select_imputation == "time") {
      sliderInput(
        "intslider",
        label = "Smoothness (number of knots in cr spline):",
        min = 1,
        max = 50,
        value = 10,
        step = 1
      )
    } else if (input$select_imputation == "regn") {
      selectInput(
        "select_covariate",
        label = h5("Covariate"),
        choices = v_names_for_box
      )
    }
  })

  # Data retrieval functionality-----
  observeEvent(input$retrieve_data, {
    for (i in 1:length(v_names)) {
      v_names_checklist[[v_names[i]]] <- FALSE
    }

    # enabling previously disabled buttons
    shinyjs::show("extracted_data")
    shinyjs::show("validation_calendar_outer")

    ##* WIP: create a subsetting function in metamet. mm_qry here will be the
    ## return value  of this function
    mm_qry <<- metamet::subset_by_date(
      mm2,
      start_date = df_daterange()$start_date,
      end_date = df_daterange()$end_date
    )

    mm_qry$dt$checked <<- as.factor(rownames(mm_qry$dt))
    mm_qry$dt$datect_num <<- as.numeric(mm_qry$dt$DATECT)

    # Add a tab to the plotting panel for each variable that has been selected by the user.
    output$mytabs <- renderUI({
      my_tabs <- lapply(paste(v_names_for_box), function(i) {
        tabPanel(
          i,
          value = i,
          tags$style(HTML(paste0(
            '.tabbable > .nav > li > a[data-value=',
            i,
            '] {border: transparent;background-color:',
            ifelse(v_names_checklist[[i]] == TRUE, '#bcbcbc', 'transparent'),
            ';}'
          ))),
          girafeOutput(paste0(i, "_interactive_plot")),
        )
      })
      do.call(tabsetPanel, c(my_tabs, id = "plotTabs"))
    })

    observe(
      lapply(paste(v_names_for_box), function(i) {
        output[[paste0(i, "_interactive_plot")]] <-
          renderGirafe(plotting_function(i))
      })
    )

    # Creating a calendar heatmap plot that will be plotted depending on the tab selected in plotTabs
    heatmap_plot_selected <- reactive({
      req(input$plotTabs)
      plot_heatmap_calendar(mm_qry$dt_qc)
    })

    output$heatmap_plot <- renderPlot(heatmap_plot_selected())

    enable('compare_vars')
  })

  # compare variables modal
  observeEvent(input$compare_vars, {
    plot_data <- reactive({
      data.frame(x = mm_qry$dt[, input$x_var], y = mm_qry$dt[, input$y_var])
    })

    output$compare_vars_plot <- renderPlot({
      ggplot(data = plot_data(), aes(x = x, y = y)) +
        geom_point() +
        labs(x = input$x_var, y = input$y_var) +
        theme_bw()
    })

    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            column(
              6,
              selectInput('x_var', 'X variable:', choices = v_names_for_box)
            ),
            column(
              6,
              selectInput(
                'y_var',
                'Y variable:',
                choices = v_names_for_box,
                selected = v_names_for_box[2]
              )
            )
          ),
          fluidRow(
            shinycssloaders::withSpinner(plotOutput("compare_vars_plot"))
          )
        ),
        footer = modalButton("Close"),
        easyClose = FALSE,
        size = "l"
      )
    )
  })

  # Creating reactive variables-----
  selected_state <- reactive({
    input[[paste0(input$plotTabs, "_interactive_plot_selected")]]
  })

  # Impute button functionality----
  observeEvent(input$impute, {
    if (is.null(selected_state())) {
      shinyjs::alert("Please select a point to impute.")
    } else {
      mm_qry <<- metamet::impute(
        v_y = input$plotTabs,
        mm = mm_qry,
        method = input$select_imputation,
        qc_tokeep = as.numeric(input$qc_tokeep),
        x = input$select_covariate,
        k = input$intslider,
        plot_graph = FALSE,
        selection = mm_qry$dt$checked %in% selected_state()
      )

      # Re-plotting plot after imputation is confirmed to illustrate changes
      shinyjs::show("plotted_data")
      enable("reset")
      enable("impute")
      enable("finished_check")

      # Creating a reactive plot that will be plotted depending on the tab selected in plotTabs
      plot_selected <- reactive({
        req(input$plotTabs)
        plotting_function(input$plotTabs)
      })
      # Re-render
      output[[paste0(
        input$plotTabs,
        "_interactive_plot"
      )]] <- renderGirafe(plot_selected())
    }
  })

  # Reset button functionality----
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Are you sure you want to restart the app? All progress will be lost",
      footer = tagList(
        actionButton("confirm_reset", "I want to restart the app."),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))

    observeEvent(input$confirm_reset, {
      session$reload()
    })
  })

  # Finished checking, close tab functionality----
  observeEvent(input$finished_check, {
    # Insert validation flag for date range here
    v_names_checklist[[input$plotTabs]] <- TRUE
  })

  output$download_data <- downloadHandler(
    filename = function() {
      if (input$download_file == 'lev1') {
        paste("level_1-", Sys.Date(), ".zip", sep = "")
      } else if (input$download_file == 'lev2') {
        paste("level_2-", Sys.Date(), ".zip", sep = "")
      } else if (input$download_file == 'ceda') {
        paste("ceda-", Sys.Date(), ".zip", sep = "")
      }
    },
    content = function(file) {
      if (input$download_file == 'lev1') {
        runjs(
          'document.getElementById("download_data").textContent="Preparing download...";'
        )
        shinyjs::disable("download_data")
        tmpdir <- tempdir()
        setwd(tempdir())
        fs <- c('level_1-data.csv', 'level_1-qc.csv')
        data.table::fwrite(mm1$dt, 'level_1-data.csv')
        data.table::fwrite(mm1$dt_qc, 'level_1-qc.csv')
        zip(zipfile = file, files = fs)
        runjs(
          'document.getElementById("download_data").textContent="Download";'
        )
        shinyjs::enable("download_data")
      } else if (input$download_file == 'lev2') {
        runjs(
          'document.getElementById("download_data").textContent="Preparing download...";'
        )
        shinyjs::disable("download_data")
        tmpdir <- tempdir()
        setwd(tempdir())
        fs <- c('level_2-data.csv', 'level_2-qc.csv')
        data.table::fwrite(mm2$dt, 'level_2-data.csv')
        data.table::fwrite(mm2$dt_qc, 'level_2-qc.csv')
        zip(zipfile = file, files = fs)
        runjs(
          'document.getElementById("download_data").textContent="Download";'
        )
        shinyjs::enable("download_data")
      } else if (input$download_file == 'ceda') {
        runjs(
          'document.getElementById("download_data").textContent="Preparing download...";'
        )
        shinyjs::disable("download_data")
        tmpdir <- tempdir()
        setwd(tempdir())
        fs <- c('ceda-data.csv')
        df_ceda <- format_for_ceda(mm2)
        data.table::fwrite(df_ceda, 'ceda-data.csv')
        zip(zipfile = file, files = fs)
        runjs(
          'document.getElementById("download_data").textContent="Download";'
        )
        shinyjs::enable("download_data")
      }
    }
  )

  # Writing validated data to pin---- From main Dashboard
  observeEvent(input$submitchanges_cloud, {
    # Update button text
    runjs(
      'document.getElementById("submitchanges_cloud").textContent="Submitting changes...";'
    )

    # disable button while working
    shinyjs::disable("submitchanges_cloud")
    shinyjs::disable("edit_table_cols")

    # update lev2 with mm_qry
    mm_qry$dt_qc$validator <- username

    # overwrite existing data with changes in query
    mm2 <<- join(mm2, mm_qry)

    ##* WIP: temp stop writing to pins
    # write to pin on Connect server
    # pin_write(board, mm2, name = "plevy/level2_data", type = "rds")

    # write CEDA formatted data to pin
    df_ceda <- format_for_ceda(mm2)
    # pin_write(board, df_ceda, name = "plevy/ceda_data", type = "rds")

    # time_diff <- difftime(
    #   as.POSIXct(Sys.time()),
    #   as.POSIXct(pins::pin_meta(board, 'plevy/level2_data')$created),
    #   units = 'mins'
    # )

    # if (time_diff < 2) {
    #   shinyalert(
    #     title = "Data successfully saved to cloud",
    #     size = "m",
    #     closeOnEsc = TRUE,
    #     closeOnClickOutside = TRUE,
    #     html = FALSE,
    #     type = "success",
    #     showConfirmButton = TRUE,
    #     showCancelButton = FALSE,
    #     confirmButtonText = "OK",
    #     confirmButtonCol = "#AEDEF4",
    #     timer = 10000,
    #     imageUrl = "",
    #     animation = TRUE
    #   )
    # } else {
    #   shinyalert(
    #     title = "Error saving data",
    #     text = "Data took over 2 minutes to write. Data may not have saved correctly to the cloud.",
    #     size = "m",
    #     closeOnEsc = FALSE,
    #     closeOnClickOutside = FALSE,
    #     html = FALSE,
    #     type = "error",
    #     showConfirmButton = FALSE,
    #     showCancelButton = TRUE,
    #     cancelButtonText = "Cancel",
    #     timer = 10000,
    #     imageUrl = "",
    #     animation = TRUE
    #   )
    # }

    # remove button activation and reactivate button
    runjs(
      'document.getElementById("submitchanges_cloud").textContent="Submit";'
    )
    shinyjs::enable("submitchanges_cloud")
  })
}

shinyApp(ui, server)
# }
