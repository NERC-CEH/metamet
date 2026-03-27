#' Upload Module UI
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Upload data"),
    fileInput(ns("csv_file"), "Upload your file(s)"),

    checkboxInput(ns("use_metadata"), "Custom metadata (upload)", value = FALSE),
    uiOutput(ns("metadata_ui")),
    checkboxInput(ns("use_custom_site"), "Upload custom site file", value = FALSE),
    uiOutput(ns("site_file_ui")),
    uiOutput(ns("site_select_ui")),
    br(),
    uiOutput(ns("file_preview_ui")),
    br(), br(),
    fluidRow(
      column(12, actionButton(ns("load_preview"), "Load and preview file(s)", class = "btn-primary")),
      column(12, actionButton(ns("remove_file"), "Remove file", class = "btn-danger"))
    )
  )
}

# upload server module defining logic
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_rv <- reactiveValues(
      data = NULL,
      metadata = NULL,
      site = NULL,
      mm = NULL
    )

# helpers to reduce issues with xlsx
    standardise_site_col <- function(df) {
      site_col <- intersect(c("site", "site_id"), names(df))[1]

      if (is.null(site_col)) {
        stop("No 'site' or 'site_id' column found.")
      }

      names(df)[names(df) == site_col] <- "site"
      df
    }

    read_excel_smart <- function(path, expected_sheet = NULL) {
      sheets <- readxl::excel_sheets(path)

      if (!is.null(expected_sheet) && expected_sheet %in% sheets) {
        message("Reading sheet: ", expected_sheet)
        return(readxl::read_excel(path, sheet = expected_sheet) |> as.data.frame())
      }

      warning(paste0("Sheet '", expected_sheet, "' not found. Using: ", sheets[1]))
      readxl::read_excel(path, sheet = sheets[1]) |> as.data.frame()
    }

# Load built-in site and metadata files if they exist
    builtin_dt_site <- tryCatch({
      path <- system.file("tests/testthat/data-raw/dt_site.csv", package = "metamet")
      warning(paste("dt_site path:", path))
      readr::read_csv(path, show_col_types = FALSE) |> as.data.frame()
    }, error = function(e) NULL)

    builtin_dt_meta <- tryCatch({
      path <- system.file("tests/testthat/data-raw/dt_meta.xlsx", package = "metamet")
      warning(paste("dt_meta path:", path))
      if (path == "") stop("dt_meta.xlsx not found")

      read_excel_smart(path, expected_sheet = "dt_meta") |>
        as.data.frame()

    }, error = function(e) {
      message("Failed to load metadata: ", e$message)
      NULL
    })

# UI ####################################################################################

# allows to upload custom site file
    output$site_file_ui <- renderUI({
      if (isTRUE(input$use_custom_site)) {
        fileInput(ns("site_file"), "Upload site file")
      }
    })

# dropdown with site selection
    output$site_select_ui <- renderUI({
      if (!is.null(builtin_dt_site)) {
        site_col <- intersect(c("site", "site_id"), names(builtin_dt_site))[1]
        choices <- unique(builtin_dt_site[[site_col]])
        selectInput(ns("site_select"), "Select site", choices = choices)
      }
    })
# allows to upload custom metadata file
    output$metadata_ui <- renderUI({
      if (isTRUE(input$use_metadata)) {
        fileInput(ns("metadata_file"), "Upload metadata file")
      }
    })

# Observe events for loading and previewing files
    observeEvent(input$load_preview, {
      req(input$csv_file)

      # Data
      data_df <- readr::read_csv(input$csv_file$datapath, show_col_types = FALSE) |> as.data.frame()
      data_rv$data <- data_df

      # Metadata
      if (isTRUE(input$use_metadata)) {
        req(input$metadata_file)

        ext <- tools::file_ext(input$metadata_file$name)

        chosen_meta <- if (ext == "csv") {
          readr::read_csv(input$metadata_file$datapath, show_col_types = FALSE)
        } else {
          read_excel_smart(input$metadata_file$datapath, "dt_meta")
        }

      } else {
        chosen_meta <- builtin_dt_meta
      }

      # Site
      if (isTRUE(input$use_custom_site)) {
        req(input$site_file)

        ext <- tools::file_ext(input$site_file$name)

        chosen_site <- if (ext == "csv") {
          readr::read_csv(input$site_file$datapath, show_col_types = FALSE)
        } else {
          read_excel_smart(input$site_file$datapath, "dt_site")
        }

      } else {
        chosen_site <- builtin_dt_site
      }

      # Standardise
      #chosen_meta <- standardise_site_col(chosen_meta)
      chosen_site <- standardise_site_col(chosen_site)

      # Filter by site if site selection exists
      req(input$site_select)
      site_id <- input$site_select

      chosen_meta <- chosen_meta[chosen_meta$site == site_id, , drop = FALSE]
      chosen_site <- chosen_site[chosen_site$site == site_id, , drop = FALSE]

      # Build metamet object with error handling and debug output
      mm_obj <- tryCatch({

        # Debug
        message("---- DEBUG: Inspecting arguments before metamet ----")
        message("data_df type: ", class(data_df))
        message("chosen_meta type: ", class(chosen_meta))
        message("chosen_site type: ", class(chosen_site))
        message("site_id: ", site_id)
        message("nrow data_df: ", nrow(data_df))
        message("nrow chosen_meta: ", nrow(chosen_meta))
        message("nrow chosen_site: ", nrow(chosen_site))
        message("Column names data_df: ", paste(names(data_df), collapse = ", "))
        message("Column names chosen_meta: ", paste(names(chosen_meta), collapse = ", "))
        message("Column names chosen_site: ", paste(names(chosen_site), collapse = ", "))

        # show first few rows for data, metadata & site
        print(head(data_df, 5))
        print(head(chosen_meta, 5))
        print(head(chosen_site, 5))

        # make sure it's a df not a tibble!
        chosen_site <- as.data.frame(chosen_site)
        # call metamet
        metamet::metamet(
          dt = data_df,
          dt_meta = chosen_meta,
          dt_site = chosen_site,
          site_id = site_id
        )

      }, error = function(e) {
        message("❌ metamet failed with error: ", e$message)
        # keep debugging info
        message("---- END DEBUG ----")
        NULL
      })

      data_rv$mm <- mm_obj
    })

# Preview of loaded files
    output$file_preview_ui <- renderUI({
      req(data_rv$data)

      tagList(
        h4("Data"),
        DT::dataTableOutput(ns("data_preview")),

        if (!is.null(data_rv$mm)) {
          tagList(
            h4("Metamet summary"),
            verbatimTextOutput(ns("mm_summary")),

            h4("dt"),
            DT::dataTableOutput(ns("mm_dt")),

            h4("dt_meta"),
            DT::dataTableOutput(ns("mm_meta")),

            h4("dt_site"),
            DT::dataTableOutput(ns("mm_site"))
          )
        }
      )
    })

    output$data_preview <- DT::renderDataTable({
      DT::datatable(head(data_rv$data, 10))
    })

    output$mm_summary <- renderPrint({
      req(data_rv$mm)
      data_rv$mm
    })

    output$mm_dt <- DT::renderDataTable({
      DT::datatable(head(data_rv$mm$dt, 10))
    })

    output$mm_meta <- DT::renderDataTable({
      DT::datatable(head(data_rv$mm$dt_meta, 10))
    })

    output$mm_site <- DT::renderDataTable({
      DT::datatable(head(data_rv$mm$dt_site, 10))
    })

    return(
      reactive({
        list(
          data = data_rv$data,
          metadata = data_rv$metadata,
          site = data_rv$site,
          mm = data_rv$mm
        )
      })
    )

  })
}
