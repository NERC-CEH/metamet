#' Upload Module UI
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Upload data"),
    fileInput(ns("csv_file"), "Upload your file(s)"),

    checkboxInput(
      ns("use_metadata"),
      "Custom metadata (upload)",
      value = FALSE
    ),
    uiOutput(ns("metadata_ui")),

    checkboxInput(
      ns("use_custom_site"),
      "Upload custom site file",
      value = FALSE
    ),
    uiOutput(ns("site_file_ui")),
    uiOutput(ns("site_select_ui")),

    checkboxInput(ns("use_era5"), "Add ERA5 ref data", value = FALSE),

    br(),
    fluidRow(
      column(
        12,
        actionButton(
          ns("load_preview"),
          "Load and preview file(s)",
          class = "btn-primary"
        )
      ),
      column(
        12,
        actionButton(ns("remove_file"), "Remove file", class = "btn-danger")
      )
    ),

    br(),
    br(),
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

    # -------------------------
    # Helpers
    # -------------------------
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
        return(
          readxl::read_excel(path, sheet = expected_sheet) |> as.data.frame()
        )
      }

      readxl::read_excel(path, sheet = sheets[1]) |> as.data.frame()
    }

    # -------------------------
    # Built-in data
    # -------------------------
    builtin_dt_site <- tryCatch(
      {
        path <- system.file(
          "tests/testthat/data-raw/dt_site.csv",
          package = "metamet"
        )
        readr::read_csv(path, show_col_types = FALSE) |> as.data.frame()
      },
      error = function(e) NULL
    )

    builtin_dt_meta <- tryCatch(
      {
        path <- system.file(
          "tests/testthat/data-raw/dt_meta.xlsx",
          package = "metamet"
        )
        read_excel_smart(path, "dt_meta")
      },
      error = function(e) NULL
    )

    # -------------------------
    # UI
    # -------------------------
    output$metadata_ui <- renderUI({
      if (isTRUE(input$use_metadata)) {
        fileInput(ns("metadata_file"), "Upload metadata file")
      }
    })

    output$site_file_ui <- renderUI({
      if (isTRUE(input$use_custom_site)) {
        fileInput(ns("site_file"), "Upload site file")
      }
    })

    output$site_select_ui <- renderUI({
      req(builtin_dt_site)
      site_col <- intersect(c("site", "site_id"), names(builtin_dt_site))[1]
      selectInput(
        ns("site_select"),
        "Select site",
        choices = unique(builtin_dt_site[[site_col]])
      )
    })

    # -------------------------
    # Load data
    # -------------------------
    observeEvent(input$load_preview, {
      req(input$csv_file)

      data_df <- readr::read_csv(
        input$csv_file$datapath,
        show_col_types = FALSE
      ) |>
        as.data.frame()

      data_rv$data <- data_df

      # Metadata
      chosen_meta <- if (isTRUE(input$use_metadata)) {
        req(input$metadata_file)
        read_excel_smart(input$metadata_file$datapath, "dt_meta")
      } else {
        builtin_dt_meta
      }

      # Site
      chosen_site <- if (isTRUE(input$use_custom_site)) {
        req(input$site_file)
        read_excel_smart(input$site_file$datapath, "dt_site")
      } else {
        builtin_dt_site
      }

      chosen_site <- standardise_site_col(chosen_site)

      # Filter by site
      req(input$site_select)
      site_id <- input$site_select

      chosen_meta <- chosen_meta[chosen_meta$site == site_id, , drop = FALSE]
      chosen_site <- chosen_site[chosen_site$site == site_id, , drop = FALSE]

      # Save
      data_rv$metadata <- chosen_meta
      data_rv$site <- chosen_site

      # Build metamet object
      data_rv$mm <- tryCatch({
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
    })

    # -------------------------
    # Preview
    # -------------------------
    output$file_preview_ui <- renderUI({
      req(data_rv$data)
      tagList(
        h4("Raw data"),
        DT::dataTableOutput(ns("data_preview"))
      )
    })

    output$data_preview <- DT::renderDataTable({
      DT::datatable(head(data_rv$data, 10))
    })

    # -------------------------
    # Return
    # -------------------------
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
