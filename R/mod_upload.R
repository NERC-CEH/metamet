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
    checkboxInput(ns("use_era5"), "Add ERA5 ref data", value = FALSE),
    br(),
    fluidRow(
      column(12, actionButton(ns("load_preview"), "Load and preview file(s)", class = "btn-primary")),
      column(12, actionButton(ns("remove_file"), "Remove file", class = "btn-danger"))
    ),
    br(), br(),
    uiOutput(ns("file_preview_ui"))
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

      # Load data
      data_df <- readr::read_csv(input$csv_file$datapath, show_col_types = FALSE) |> as.data.frame()
      data_rv$data <- data_df

      # Load metadata
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

      # Load site
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

      # Standardise site
      chosen_site <- standardise_site_col(chosen_site)

      # Filter by selected site
      req(input$site_select)
      site_id <- input$site_select
      chosen_meta <- chosen_meta[chosen_meta$site == site_id, , drop = FALSE]
      chosen_site <- chosen_site[chosen_site$site == site_id, , drop = FALSE]

      # Build metamet object with error handling
      mm_obj <- tryCatch({
        metamet::metamet(
          dt = data_df,
          dt_meta = chosen_meta,
          dt_site = chosen_site,
          site_id = site_id
        )
      }, error = function(e) {
        message("❌ metamet failed: ", e$message)
        NULL
      })

      # Optionally add ERA5 data
      if (!is.null(mm_obj) && isTRUE(input$use_era5)) {
        mm_obj <- tryCatch({
          era5_path <- system.file("tests/testthat/data-raw/dt_era5.csv", package = "metamet")
          if (era5_path == "") stop("ERA5 file not found")
          mm_tmp <- add_era5(mm_obj, fname_era5 = era5_path)
          mm_tmp <- apply_qc(join(mm_tmp, mm_obj))
          if ("TS" %in% names(mm_tmp$dt)) mm_tmp <- impute(mm_tmp)
          mm_tmp
        }, error = function(e) {
          message("ERA5 failed: ", e$message)
          mm_obj
        })
      }

      # Save to reactiveValues
      data_rv$mm <- mm_obj
    })  # end observeEvent(input$load_preview)

    # File preview UI
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

    # Render outputs
    output$data_preview <- DT::renderDataTable({
      DT::datatable(head(data_rv$data, 10))
    })
    output$mm_summary <- renderPrint({ req(data_rv$mm); data_rv$mm })
    output$mm_dt <- DT::renderDataTable({ DT::datatable(head(data_rv$mm$dt, 10)) })
    output$mm_meta <- DT::renderDataTable({ DT::datatable(head(data_rv$mm$dt_meta, 10)) })
    output$mm_site <- DT::renderDataTable({ DT::datatable(head(data_rv$mm$dt_site, 10)) })

    # Return reactive list
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
  })  # end moduleServer(
}  # end mod_upload_server(
