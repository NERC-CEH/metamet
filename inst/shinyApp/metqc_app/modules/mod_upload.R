#' Upload module
#'
#' UI and server components for loading existing metamet `.rds` files.
#'
#' @name mod_upload
NULL

#' @rdname mod_upload
#' @param id Module id.
#'
#' @return A Shiny UI element.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "upload",
    h4("Upload an existing metamet .rds file"),
    shinyFilesButton(
      ns("file"),
      label = "Browse for .rds file",
      title = "Select a file",
      multiple = FALSE
    ),
    br(),
    verbatimTextOutput(ns("status")),
    uiOutput(ns("loaded_file_banner"))
  )
}

#' @rdname mod_upload
#' @param id Module id.
#' @param roots Named vector of root directories used by
#'   `shinyFiles::shinyFileChoose()`.
#' @param default_root Default root directory name.
#' @param root_session Parent Shiny session used for application-wide
#'   navigation and updates.
#'
#' @return A reactive expression containing the uploaded metamet object
#'   and associated metadata.
#' @noRd
mod_upload_server <- function(id, roots, default_root, root_session) {
  moduleServer(id, function(input, output, session) {
    shinyFileChoose(
      input,
      "file",
      roots = roots,
      session = session,
      filetypes = c("rds"),
      defaultRoot = default_root,
      defaultPath = ""
    )

    uploaded <- reactive({
      req(input$file)

      fileinfo <- parseFilePaths(roots, input$file)
      req(nrow(fileinfo) > 0)

      fname <- as.character(fileinfo$datapath)
      mm <- readRDS(fname)

      if (!identical(attr(mm, "format"), "long")) {
        mm <- metamet_reshape(mm, "long")
      }

      v_names <- unique(
        mm$dt_meta[type != "time" & type != "site", name_icos]
      )

      date_of_first_new_record <- mm$dt[, min(TIMESTAMP, na.rm = TRUE)]
      date_of_last_new_record <- mm$dt[, max(TIMESTAMP, na.rm = TRUE)]

      data.table::setkeyv(
        mm$dt,
        c("name_icos", "site", "TIMESTAMP")
      )

      list(
        mm = mm,
        v_names = v_names,
        date_of_first_new_record = date_of_first_new_record,
        date_of_last_new_record = date_of_last_new_record,
        fname = fname
      )
    })

    output$status <- renderText({
      if (is.null(input$file)) {
        return("No file selected yet.")
      }

      req(uploaded())
      paste("Loaded file:", basename(uploaded()$fname))
    })

    output$loaded_file_banner <- renderUI({
      req(uploaded())

      tagList(
        br(),
        box(
          width = 12,
          status = "success",
          solidHeader = TRUE,
          title = tags$span(icon("file"), "Loaded file"),
          div(
            style = "font-size:16px; font-weight:600; margin-bottom:10px;",
            basename(uploaded()$fname)
          ),
          actionButton(
            session$ns("remove_file"),
            "Remove file and start again",
            icon = icon("trash"),
            class = "btn btn-danger"
          )
        )
      )
    })

    observeEvent(input$remove_file, {
      mm_qry <<- NULL
      if (exists("mm_qry", envir = .GlobalEnv, inherits = FALSE)) {
        rm("mm_qry", envir = .GlobalEnv)
      }

      shinyjs::hide("extracted_data")
      shinyjs::reset(session$ns("file"))

      shinyjs::enable(selector = "a[data-value='metadata_maker']")
      shinyjs::enable(selector = "a[data-value='upload']")

      updateTabItems(
        root_session,
        "tabs",
        "upload"
      )

      showNotification(
        "Metamet file removed. You can now load a new one.",
        type = "message"
      )
    })

    observeEvent(uploaded(), {
      showNotification(
        paste("Upload successful:", basename(uploaded()$fname)),
        type = "message",
        duration = 4
      )

      updateTabItems(
        root_session,
        "tabs",
        "dashboard"
      )
    })

    uploaded
  })
}
