library(metamet)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(shinyvalidate)
library(shinyFiles)
library(ggiraph)

source("mod_metadata_maker.R", local = TRUE)

# Set the gap-filling methods and codes----
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
      menuItem(
        "Metamet Maker",
        tabName = "metadata_maker",
        icon = icon("table")
      ),
      menuItem("Choose file", tabName = "upload", icon = icon("upload")),
      # with shinyfiles lib
      shinyFilesButton(
        id = "file",
        label = "Open metamet .rds file",
        title = "Select a file",
        multiple = FALSE
      ),
      menuItem(
        "Choose date range",
        tabName = "dashboard",
        icon = icon('database')
      ),
      menuItem("Download", tabName = "download", icon = icon('download')),
      menuItem(
        "Information",
        tabName = "information",
        icon = icon('info'),
        menuSubItem('Gap-fill methods', tabName = 'gapfill_guide'),
        menuSubItem('App guide', tabName = 'app_guide'),
        menuSubItem('Data process guide', tabName = 'data_guide')
      )
    ),
    tags$ul(
      class = "sidebar-menu",
      tags$li(
        actionLink(
          "stop_app",
          tagList(icon("stop"), tags$span("Stop App"))
        )
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
              uiOutput("start_date")
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
              checkboxInput(
                "scale_ref",
                label = "Rescale reference to observations",
                value = FALSE
              ),
              sliderInput(
                "point_size",
                label = "Point size:",
                min = 0.5,
                max = 8,
                value = 3,
                step = 0.5
              ),
              uiOutput("missing_comment_banner"),
              shinycssloaders::withSpinner(uiOutput("mytabs")),
              selectInput(
                "select_imputation",
                label = h5("Gap-Filling Method"),
                choices = gf_choices
              ),
              uiOutput("qc_comment_box"),
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
              actionButton("submitchanges", "Submit changes")
            ),
          )
        ),
      ),

      # upload file tab
      tabItem(
        tabName = "upload",
        verbatimTextOutput("status")
      ),
      tabItem(
        tabName = "metadata_maker",
        mod_metadata_maker_ui("mm_maker")
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
        tabName = "gapfill_guide"
        # includeMarkdown(here::here("vignettes/gap_fill_methods.md"))
      ),
      tabItem(
        tabName = "app_guide"
        # includeMarkdown(here::here("vignettes/app_user_guide.md"))
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
  # increase input file size limit to 200 MB
  options(shiny.maxRequestSize = 200 * 1024^2)

  # maps all drives that users have on windows
  get_drives <- function() {
    if (.Platform$OS.type == "windows") {
      drives <- letters
      drives <- paste0(drives, ":/")
      drives <- drives[dir.exists(drives)]
      return(setNames(drives, drives))
    } else {
      return(c(home = fs::path_home()))
    }
  }

  roots <- get_drives()
  v_extdata <- system.file("extdata", package = "metamet")
  if (nzchar(v_extdata) && dir.exists(v_extdata)) {
    roots <- c("Package data" = v_extdata, roots)
  }
  default_root <- names(roots)[1L]

  shinyFileChoose(
    input,
    "file",
    roots = roots,
    session = session,
    filetypes = c("rds"),
    defaultRoot = default_root,
    defaultPath = ""
  )

  mod_metadata_maker_server(
    "mm_maker",
    v_roots = roots,
    default_root = default_root
  )

  # Non-reactive code
  # Format the start and end dates----
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

  # Reactive expression to load the RDS file
  uploaded <- reactive({
    req(input$file)
    fileinfo <- parseFilePaths(roots, input$file)
    req(nrow(fileinfo) > 0)
    fname <- as.character(fileinfo$datapath)
    mm <- readRDS(fname)
    if (!identical(attr(mm, "format"), "long")) {
      mm <- metamet_reshape(mm, "long")
    }
    # restore qc_comment if present
    if (!is.null(mm$dt_qc_comment)) {
      mm$dt <- merge(
        mm$dt,
        mm$dt_qc_comment,
        by = c("site", "TIMESTAMP", "name_icos"),
        all.x = TRUE
      )
    }
    v_names <- unique(mm$dt_meta[type != "time" & type != "site", name_icos])
    date_of_first_new_record <- mm$dt[, min(TIMESTAMP, na.rm = TRUE)]
    date_of_last_new_record <- mm$dt[, max(TIMESTAMP, na.rm = TRUE)]
    setkeyv(mm$dt, c("name_icos", "site", "TIMESTAMP"))
    list(
      mm = mm,
      v_names = v_names,
      date_of_first_new_record = date_of_first_new_record,
      date_of_last_new_record = date_of_last_new_record,
      fname = fname
    )
  })

  # Simple status message instead of displaying the object
  output$status <- renderText({
    if (is.null(input$file)) {
      return("No file selected yet.")
    }
    req(uploaded())
    paste("Loaded file:", basename(uploaded()$fname))
  })

  # confirms upload was succesful and then switches to calendar selection
  observeEvent(uploaded(), {
    showNotification(
      paste("Upload successful:", basename(uploaded()$fname)),
      type = "message",
      duration = 4
    )
    updateTabItems(session, "tabs", "dashboard")
  })

  ##########################
  #shinyvalidate statements#
  #########################
  iv <- InputValidator$new()

  iv$add_rule("sdate", sv_required())
  iv$add_rule("edate", sv_required())
  iv$enable()

  # save the username
  username <<- Sys.info()[["user"]]
  print(paste("Proceeding with", username, "as data validator"))

  observeEvent(input$stop_app, {
    stopApp()
  })

  session$onSessionEnded(function() {
    stopApp()
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

  disable('compare_vars')

  v_names_checklist <- reactiveValues()
  v_missing_comments <- reactiveValues()
  qc_comments <- reactiveValues()

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
    print(as.Date(uploaded()$date_of_first_new_record, tz = "UTC"))
    dateInput(
      "sdate",
      value = as.Date(uploaded()$date_of_first_new_record, tz = "UTC"),
      min = first_start_date(),
      max = last_end_date(),
      label = "Start date"
    )
  })

  # Create a date input for the user to select end date
  output$end_date <- renderUI({
    print(as.Date(uploaded()$date_of_last_new_record, tz = "UTC"))
    dateInput(
      "edate",
      value = as.Date(uploaded()$date_of_last_new_record, tz = "UTC"),
      min = first_start_date(),
      max = last_end_date(),
      label = "End date"
    )
  })

  # Create a dataframe with the start and end dates,
  df_daterange <- eventReactive(input$retrieve_data, {
    start_date_ch <- paste(
      sprintf("%02d", lubridate::day(input$sdate)),
      "/",
      sprintf("%02d", lubridate::month(input$sdate)),
      "/",
      lubridate::year(input$sdate),
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
      sprintf("%02d", lubridate::day(input$edate)),
      "/",
      sprintf("%02d", lubridate::month(input$edate)),
      "/",
      lubridate::year(input$edate),
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
        choices = uploaded()$v_names
      )
    }
  })

  # Data retrieval functionality-----
  observeEvent(input$retrieve_data, {
    for (i in 1:length(uploaded()$v_names)) {
      v_names_checklist[[uploaded()$v_names[i]]] <- FALSE
    }

    # enabling previously disabled buttons
    shinyjs::show("extracted_data")

    mm_qry <<- metamet::subset_by_date(
      uploaded()$mm,
      start_date = df_daterange()$start_date,
      end_date = df_daterange()$end_date
    )
    setkeyv(mm_qry$dt, c("name_icos", "site", "TIMESTAMP"))
    mm_qry$dt[, row_name := as.factor(rownames(mm_qry$dt))]
    mm_qry$dt$datect_num <<- as.numeric(mm_qry$dt$TIMESTAMP)

    if (!"qc_orig" %in% names(mm_qry$dt)) {
      mm_qry$dt[, qc_orig := qc]
    }

    # Add a tab to the plotting panel for each variable that has been selected by the user.
    output$mytabs <- renderUI({
      my_tabs <- lapply(paste(uploaded()$v_names), function(i) {
        replicates <- unique(mm_qry$dt[name_icos == i, var_name])
        tabPanel(
          i,
          value = i,
          tags$style(HTML(paste0(
            '.tabbable > .nav > li > a[data-value=', i, '] {',
            if (v_names_checklist[[i]] == TRUE) {
              'background-color:#bcbcbc;'   # finished checking
            } else {
              'background-color:transparent;'
            },
            '}'
          ))),
          if (length(replicates) > 1L) {
            checkboxGroupInput(
              paste0(i, "_replicates"),
              label = "Replicates to show:",
              choices = replicates,
              selected = replicates,
              inline = TRUE
            )
          },
          girafeOutput(paste0(i, "_interactive_plot")),
        )
      })
      do.call(tabsetPanel, c(my_tabs, id = "plotTabs"))
    })

    observe(
      lapply(paste(uploaded()$v_names), function(i) {
        output[[paste0(i, "_interactive_plot")]] <-
          renderGirafe(metamet:::ggiraph_plot(
            i,
            scale_ref = input$scale_ref,
            point_size = input$point_size,
            vars_to_show = input[[paste0(i, "_replicates")]]
          ))
      })
    )

    enable('compare_vars')
  })

  # compare variables modal
  # Registered at server top level (not inside the observeEvent) so it re-renders
  # whenever input$x_var / input$y_var change after the modal is open.
  output$compare_vars_plot <- renderPlot({
    req(input$x_var, input$y_var)
    x_data <- mm_qry$dt[name_icos == input$x_var, .(TIMESTAMP, site, x = value)]
    y_data <- mm_qry$dt[name_icos == input$y_var, .(TIMESTAMP, site, y = value)]
    plot_data <- merge(x_data, y_data, by = c("TIMESTAMP", "site"))
    ggplot(plot_data, aes(x = x, y = y)) +
      geom_point() +
      labs(x = input$x_var, y = input$y_var) +
      theme_bw()
  })

  observeEvent(input$compare_vars, {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            column(
              6,
              selectInput('x_var', 'X variable:', choices = uploaded()$v_names)
            ),
            column(
              6,
              selectInput(
                'y_var',
                'Y variable:',
                choices = uploaded()$v_names,
                selected = uploaded()$v_names[1]
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
      current_var <- input$plotTabs
      # get comment from the UI
      comment <- input$qc_comment

      # store it only now (not on every keystroke)
      qc_comments[[current_var]] <- comment

      # store the comment
      row_ids <- selected_state()
      if (!"qc_comment" %in% names(mm_qry$dt)) {
        mm_qry$dt[, qc_comment := NA_character_]
      }

      # only write comment to data if provided (makes it optional)
      if (!is.null(comment) && comment != "") {
        mm_qry$dt[row_name %in% row_ids, qc_comment := comment]
      }

      mm_qry <<- metamet::impute(
        v_y = input$plotTabs,
        mm = mm_qry,
        method = input$select_imputation,
        qc_tokeep = as.numeric(input$qc_tokeep),
        x = input$select_covariate,
        k = input$intslider,
        plot_graph = FALSE,
        row_selected = selected_state()
      )

      # Re-plotting plot after imputation is confirmed to illustrate changes
      shinyjs::show("plotted_data")
      enable("reset")
      enable("impute")
      enable("finished_check")

      # Creating a reactive plot for the variable selected in plotTabs
      plot_selected <- reactive({
        req(input$plotTabs)
        metamet:::ggiraph_plot(
          input$plotTabs,
          scale_ref = input$scale_ref,
          point_size = input$point_size,
          vars_to_show = input[[paste0(input$plotTabs, "_replicates")]]
        )
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

  # dynamic qc_comment box to address each variable that has been imputed
  output$qc_comment_box <- renderUI({
    req(input$plotTabs)
    var <- input$plotTabs

    # Load existing comment if present
    existing <- qc_comments[[var]]
    if (is.null(existing)) existing <- ""

    textAreaInput(
      "qc_comment",
      label = paste0("Reason for imputation for ", var, " (optional)"),
      value = existing,
      placeholder = paste("Explain why data for", var, "was changed..."),
      width = "100%",
      rows = 3
    )
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
      if (input$download_file == 'lev2') {
        runjs(
          'document.getElementById("download_data").textContent="Preparing download...";'
        )
        shinyjs::disable("download_data")
        tmpdir <- tempdir()
        setwd(tempdir())
        fs <- c('level_2-data.csv', 'level_2-qc.csv')
        data.table::fwrite(mm$dt, 'level_2-data.csv')
        data.table::fwrite(mm$dt_qc, 'level_2-qc.csv')
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
        df_ceda <- format_for_ceda(mm)
        data.table::fwrite(df_ceda, 'ceda-data.csv')
        zip(zipfile = file, files = fs)
        runjs(
          'document.getElementById("download_data").textContent="Download";'
        )
        shinyjs::enable("download_data")
      }
    }
  )

  # Writing validated data to file---- From main Dashboard
  observeEvent(input$submitchanges, {
    # Update button text
    runjs(
      'document.getElementById("submitchanges").textContent="Submitting changes...";'
    )

    # disable button while working
    shinyjs::disable("submitchanges")
    shinyjs::disable("edit_table_cols")

    # missing comment warning for variables that were imputed
    for (v in uploaded()$v_names) {
      rows_var <- mm_qry$dt[
        name_icos == v &
          !is.na(qc) & qc != 0L &
          !is.na(qc_orig) & qc_orig != qc
      ]

      # If no imputed rows then no comment is needed
      if (nrow(rows_var) == 0) {
        v_missing_comments[[v]] <- FALSE
        next
      }

      # If imputed rows exist then require comments
      if (any(is.na(rows_var$qc_comment) | rows_var$qc_comment == "")) {
        v_missing_comments[[v]] <- TRUE
      } else {
        v_missing_comments[[v]] <- FALSE
      }
    }


    # Identify which rows were invalidated (qc != 0)
    imputed_rows <- mm_qry$dt[
      !is.na(qc) & qc != 0L & !is.na(qc_orig) & qc_orig != qc
    ]

    # update lev2 with mm_qry
    # update validator on imputed rows (qc != 0 means imputed or flagged)
    mm_qry$dt[!is.na(qc) & qc != 0L, validator := username]

    # Make a copy for saving, so the app keeps row_name for plotting
    mm_qry_save <- data.table::copy(mm_qry)

    # Remove internal columns ONLY from the saved copy
    mm_qry_save$dt[, c("row_name", "datect_num") := NULL]

    # Overwrite existing data with changes in query
    mm <- join(uploaded()$mm, mm_qry_save)

    fname <- uploaded()$fname
    print(uploaded()$fname)
    saveRDS(
      mm,
      file = paste0(
        fs::path_ext_remove(fname),
        "_qc_by_",
        username,
        "_on_",
        Sys.Date(),
        ".",
        fs::path_ext(fname)
      )
    )
    df_ceda <- format_for_ceda(mm)
    saveRDS(
      df_ceda,
      file = paste0(
        fs::path_ext_remove(fname),
        "_qc_by_",
        username,
        "_on_",
        Sys.Date(),
        "_ceda.",
        fs::path_ext(fname)
      )
    )

    # notify user that file was created
    showNotification(
      "Your file was successfully created.",
      type = "message",
      duration = 5
    )

    # remove button activation and reactivate button
    runjs(
      'document.getElementById("submitchanges").textContent="Submit";'
    )
    shinyjs::enable("submitchanges")
  })
}

shinyApp(ui, server)
