# Shiny module: metadata maker
# Six-step wizard that guides the user from a raw data file to a saved
# metamet RDS ready for QC in the main app workflow.
#
# Steps:
#   1. Load data file (plain CSV / Campbell TOA5 / Old Campbell / CEDA)
#   2. Site information (site ID, lat/lon, validity dates)
#   3. Map data columns to ICOS variable names
#   4. Set units, QC ranges, and imputation method per variable (skippable)
#   5. ERA5 reference data (optional)
#   6. Review summary and download as metamet .rds

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- UI ---------------------------------------------------------------------

mod_metadata_maker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Step 1: load file ---------------------------------------------------
    div(
      id = ns("step1"),
      box(
        title = "Step 1: Load data file",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        radioButtons(
          ns("format"),
          "File format:",
          choices = c(
            "Plain CSV (with header row)" = "csv",
            "Campbell TOA5" = "toa5",
            "Old Campbell (.dat + .dld)" = "oldcampbell",
            "CEDA BADC-CSV" = "ceda"
          )
        ),
        shinyFilesButton(
          ns("dat_file"),
          label = "Select data file",
          title = "Select the data file",
          multiple = FALSE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'oldcampbell'", ns("format")),
          br(),
          shinyFilesButton(
            ns("dld_file"),
            label = "Select .dld metadata file",
            title = "Select the DLD file",
            multiple = FALSE
          )
        ),
        br(),
        br(),
        actionButton(ns("load_file"), "Load & preview"),
        br(),
        br(),
        uiOutput(ns("table_select_ui")),
        uiOutput(ns("ts_col_ui")),
        uiOutput(ns("preview_ui")),
        uiOutput(ns("next_1_ui"))
      )
    ),
    # ---- Step 2: site information --------------------------------------------
    shinyjs::hidden(
      div(
        id = ns("step2"),
        box(
          title = "Step 2: Site information",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          radioButtons(
            ns("site_source"),
            "Site details source:",
            choices = c(
              "Enter manually" = "manual",
              "Load from CSV file" = "file"
            ),
            selected = "manual",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'file'", ns("site_source")),
            shinyFilesButton(
              ns("site_csv"),
              label = "Select dt_site CSV",
              title = "Select a dt_site CSV file",
              multiple = FALSE
            ),
            br(),
            br(),
            uiOutput(ns("site_select_ui"))
          ),
          hr(),
          fluidRow(
            column(
              4,
              textInput(ns("site_id"), "Site ID", placeholder = "e.g. UK-WHM")
            ),
            column(
              8,
              textInput(
                ns("site_long_name"),
                "Site name",
                placeholder = "e.g. Whim Moss"
              )
            )
          ),
          fluidRow(
            column(
              4,
              numericInput(
                ns("lat"),
                "Latitude (°N)",
                value = NA,
                min = -90,
                max = 90,
                step = 0.001
              )
            ),
            column(
              4,
              numericInput(
                ns("lon"),
                "Longitude (°E)",
                value = NA,
                min = -180,
                max = 180,
                step = 0.001
              )
            ),
            column(
              4,
              numericInput(ns("elev"), "Elevation (m)", value = 0, step = 1)
            )
          ),
          fluidRow(
            column(
              6,
              dateInput(
                ns("start_date"),
                "Metadata valid from",
                value = Sys.Date() - 365L * 30L
              )
            ),
            column(
              6,
              dateInput(
                ns("end_date"),
                "Metadata valid to",
                value = Sys.Date() + 365L * 5L
              )
            )
          ),
          actionButton(ns("back_2"), "Back"),
          actionButton(
            ns("next_2"),
            "Continue to variable mapping",
            class = "btn-primary"
          )
        )
      )
    ),
    # ---- Step 3: variable mapping -------------------------------------------
    shinyjs::hidden(
      div(
        id = ns("step3"),
        box(
          title = "Step 3: Map data columns to ICOS variable names",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          radioButtons(
            ns("meta_source"),
            "Variable metadata source:",
            choices = c(
              "Map manually" = "manual",
              "Load dt_meta from CSV file" = "file"
            ),
            selected = "manual",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'file'", ns("meta_source")),
            shinyFilesButton(
              ns("meta_csv"),
              label = "Select dt_meta CSV",
              title = "Select a dt_meta CSV file",
              multiple = FALSE
            ),
            br(),
            br(),
            uiOutput(ns("meta_load_ui"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("meta_source")),
            helpText(
              "Select the ICOS standard variable each data column represents.",
              "Set to '— Skip —' to exclude a column."
            ),
            uiOutput(ns("mapping_ui"))
          ),
          br(),
          actionButton(ns("back_3"), "Back"),
          actionButton(ns("next_3"), "Continue", class = "btn-primary")
        )
      )
    ),
    # ---- Step 4: variable details -------------------------------------------
    shinyjs::hidden(
      div(
        id = ns("step4"),
        box(
          title = "Step 4: Variable details",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          helpText(
            "Confirm or adjust units, QC range, and default imputation method.",
            "Fields are pre-filled from the ICOS standard where available."
          ),
          uiOutput(ns("details_ui")),
          br(),
          actionButton(ns("back_4"), "Back"),
          actionButton(
            ns("next_4"),
            "Continue",
            class = "btn-primary"
          )
        )
      )
    ),
    # ---- Step 5: ERA5 reference data ----------------------------------------
    shinyjs::hidden(
      div(
        id = ns("step5"),
        box(
          title = "Step 5: ERA5 reference data (optional)",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          radioButtons(
            ns("era5_source"),
            NULL,
            choices = c(
              "Add ERA5 reference data" = "yes",
              "Skip (ERA5 not available)" = "no"
            ),
            selected = "no"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'yes'", ns("era5_source")),
            shinyFilesButton(
              ns("era5_file"),
              label = "Select ERA5 CSV",
              title = "Select an ERA5 CSV file",
              multiple = FALSE
            ),
            br(),
            br(),
            uiOutput(ns("era5_status_ui"))
          ),
          br(),
          actionButton(ns("back_5"), "Back"),
          actionButton(
            ns("next_5"),
            "Continue to review",
            class = "btn-primary"
          )
        )
      )
    ),
    # ---- Step 6: review and save --------------------------------------------
    shinyjs::hidden(
      div(
        id = ns("step6"),
        box(
          title = "Step 6: Save metamet file",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          uiOutput(ns("summary_ui")),
          br(),
          downloadButton(ns("save_rds"), "Download as metamet .rds"),
          br(),
          br(),
          actionButton(ns("back_6"), "Back"),
          actionButton(ns("restart"), "Start anew", class = "btn-warning")
        )
      )
    )
  )
}

# ---- Server -----------------------------------------------------------------

mod_metadata_maker_server <- function(id, v_roots, default_root = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- ICOS reference table (loaded once) ----------------------------------
    dt_icos_ref <- local({
      v_path <- system.file("extdata/dt_meta.csv", package = "metamet")
      if (!nzchar(v_path)) {
        return(data.table::data.table())
      }
      dt <- data.table::fread(
        v_path,
        na.strings = c("", "NA", "#N/A"),
        showProgress = FALSE
      )
      dt <- dt[
        !is.na(name_icos) &
          nzchar(name_icos) &
          !is.na(type) &
          type != "time" &
          type != "site",
        .(
          name_icos,
          long_name_icos,
          units_icos,
          type,
          range_min,
          range_max,
          imputation_method,
          name_era5,
          long_name_era5,
          units_era5
        )
      ]
      unique(dt, by = "name_icos")
    })

    v_icos_choices <- c(
      "— Skip —" = "",
      setNames(
        dt_icos_ref$name_icos,
        paste0(dt_icos_ref$name_icos, " — ", dt_icos_ref$long_name_icos)
      )
    )

    v_method_choices <- setNames(df_method$method, df_method$method_longname)

    # ---- reactive state ------------------------------------------------------
    rv <- reactiveValues(
      format = NULL,
      dt_raw = NULL,
      l_raw_tables = NULL,
      multiple_tables = FALSE,
      v_data_cols = NULL,
      v_mapped_cols = NULL,
      v_mapping = NULL,
      dt_meta = NULL,
      dt_site = NULL,
      dt_site_from_file = NULL,
      dt_meta_from_file = NULL,
      skipped_step4 = FALSE,
      era5_path = NULL
    )

    # ---- helpers -------------------------------------------------------------
    safe_id <- function(prefix, col_nm) {
      paste0(prefix, gsub("[^A-Za-z0-9]", "_", col_nm))
    }

    go_to <- function(n) {
      for (i in 1:6) {
        v_step_id <- ns(paste0("step", i))
        if (i == n) {
          shinyjs::show(id = v_step_id, asis = TRUE)
        } else {
          shinyjs::hide(id = v_step_id, asis = TRUE)
        }
      }
    }

    prefill_site <- function(row) {
      updateTextInput(session, "site_id", value = row$site[1L] %||% "")
      updateTextInput(
        session,
        "site_long_name",
        value = row$long_name[1L] %||% ""
      )
      updateNumericInput(session, "lat", value = row$lat[1L] %||% NA)
      updateNumericInput(session, "lon", value = row$lon[1L] %||% NA)
      updateNumericInput(session, "elev", value = row$elev[1L] %||% 0)
    }

    # ---- file chooser setup -------------------------------------------------
    shinyFileChoose(
      input,
      "dat_file",
      roots = v_roots,
      session = session,
      filetypes = c("dat", "csv", "txt"),
      defaultRoot = default_root,
      defaultPath = ""
    )
    shinyFileChoose(
      input,
      "dld_file",
      roots = v_roots,
      session = session,
      filetypes = c("dld"),
      defaultRoot = default_root,
      defaultPath = ""
    )
    shinyFileChoose(
      input,
      "site_csv",
      roots = v_roots,
      session = session,
      filetypes = c("csv"),
      defaultRoot = default_root,
      defaultPath = ""
    )
    shinyFileChoose(
      input,
      "meta_csv",
      roots = v_roots,
      session = session,
      filetypes = c("csv"),
      defaultRoot = default_root,
      defaultPath = ""
    )
    shinyFileChoose(
      input,
      "era5_file",
      roots = v_roots,
      session = session,
      filetypes = c("csv"),
      defaultRoot = default_root,
      defaultPath = ""
    )

    # ---- step 1: load file --------------------------------------------------
    observeEvent(input$load_file, {
      req(input$dat_file)
      dat_info <- parseFilePaths(v_roots, input$dat_file)
      req(nrow(dat_info) > 0)
      dat_path <- as.character(dat_info$datapath)
      fmt <- input$format
      rv$format <- fmt

      dt_loaded <- tryCatch(
        {
          if (fmt == "csv") {
            data.table::fread(
              dat_path,
              na.strings = c("", "NA"),
              showProgress = FALSE
            )
          } else if (fmt == "toa5") {
            metamet:::import_campbell_data(dat_path)
          } else if (fmt == "oldcampbell") {
            dld_info <- parseFilePaths(v_roots, input$dld_file)
            validate(need(
              nrow(dld_info) > 0,
              "Please also select the .dld metadata file."
            ))
            metamet::read_old_campbell_dat(
              dat_path,
              as.character(dld_info$datapath)
            )
          } else {
            metamet::read_ceda_csv(dat_path)
          }
        },
        error = function(e) {
          showNotification(
            paste("Error loading file:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          NULL
        }
      )

      if (is.null(dt_loaded)) {
        return()
      }

      # Old Campbell returns a named list of data.tables
      if (is.list(dt_loaded) && !is.data.frame(dt_loaded)) {
        rv$l_raw_tables <- dt_loaded
        rv$dt_raw <- dt_loaded[[1L]]
        rv$multiple_tables <- length(dt_loaded) > 1L
      } else {
        rv$dt_raw <- dt_loaded
        rv$multiple_tables <- FALSE
      }

      if (fmt != "csv") {
        rv$v_data_cols <- setdiff(names(rv$dt_raw), "TIMESTAMP")
      }
    })

    # Table selector (old Campbell with multiple output tables)
    output$table_select_ui <- renderUI({
      req(rv$multiple_tables)
      tagList(
        selectInput(
          ns("table_select"),
          "Multiple output tables found — select which to use:",
          choices = names(rv$l_raw_tables),
          selected = names(rv$l_raw_tables)[1L]
        ),
        actionButton(ns("use_table"), "Use selected table")
      )
    })

    observeEvent(input$use_table, {
      req(rv$l_raw_tables, input$table_select)
      rv$dt_raw <- rv$l_raw_tables[[input$table_select]]
      rv$v_data_cols <- setdiff(names(rv$dt_raw), "TIMESTAMP")
    })

    # Timestamp column selector (plain CSV only)
    output$ts_col_ui <- renderUI({
      req(rv$dt_raw, isTRUE(rv$format == "csv"))
      tagList(
        hr(),
        fluidRow(
          column(
            6,
            selectInput(
              ns("ts_col"),
              "Timestamp column:",
              choices = names(rv$dt_raw)
            )
          ),
          column(
            6,
            textInput(
              ns("ts_format"),
              "Format if auto-detect fails:",
              placeholder = "%Y-%m-%d %H:%M:%S"
            )
          )
        )
      )
    })

    # Data preview
    output$preview_ui <- renderUI({
      req(rv$dt_raw)
      tagList(
        hr(),
        h5("Preview (first 6 rows):"),
        tableOutput(ns("preview_table"))
      )
    })

    output$preview_table <- renderTable({
      req(rv$dt_raw)
      dt <- head(rv$dt_raw, 6L)
      # integer64 columns (e.g. compact timestamps) display as 0 in renderTable;
      # convert to character so the raw values are visible
      v_i64 <- names(dt)[vapply(dt, bit64::is.integer64, logical(1L))]
      if (length(v_i64)) {
        dt <- data.table::copy(dt)
        dt[, (v_i64) := lapply(.SD, as.character), .SDcols = v_i64]
      }
      dt
    })

    # "Continue" button — only appears once data is loaded
    output$next_1_ui <- renderUI({
      req(rv$dt_raw)
      tagList(
        hr(),
        actionButton(
          ns("next_1"),
          "Continue to site information",
          class = "btn-primary"
        )
      )
    })

    # Step 1 → 2: parse plain-CSV timestamp then advance
    observeEvent(input$next_1, {
      req(rv$dt_raw)

      if (isTRUE(rv$format == "csv")) {
        req(input$ts_col)
        ts_col <- input$ts_col
        v_ts_raw <- as.character(rv$dt_raw[[ts_col]])
        v_result <- as.POSIXct(rep(NA_real_, nrow(rv$dt_raw)), tz = "UTC")

        for (fmt in c(
          "%Y%m%d%H%M%S",
          "%d/%m/%y %H:%M",
          "%d/%m/%Y %H:%M",
          "%d/%m/%y %H:%M:%S",
          "%d/%m/%Y %H:%M:%S",
          "%Y-%m-%d %H:%M",
          "%Y-%m-%d %H:%M:%S"
        )) {
          v_fill <- is.na(v_result) & !is.na(v_ts_raw)
          if (!any(v_fill)) {
            break
          }
          v_parsed <- as.POSIXct(strptime(v_ts_raw[v_fill], fmt, tz = "UTC"))
          v_ok <- !is.na(v_parsed)
          v_result[which(v_fill)[v_ok]] <- v_parsed[v_ok]
        }

        # Fallback to user-supplied format string
        ts_fmt_input <- trimws(input$ts_format %||% "")
        if (anyNA(v_result) && nzchar(ts_fmt_input)) {
          v_fill2 <- is.na(v_result)
          v_parsed2 <- as.POSIXct(
            strptime(v_ts_raw[v_fill2], ts_fmt_input, tz = "UTC")
          )
          v_ok2 <- !is.na(v_parsed2)
          v_result[which(v_fill2)[v_ok2]] <- v_parsed2[v_ok2]
        }

        if (anyNA(v_result)) {
          showNotification(
            paste0(
              sum(is.na(v_result)),
              " timestamps could not be parsed. Try specifying the format manually."
            ),
            type = "warning",
            duration = 8
          )
          return()
        }

        dt_new <- data.table::copy(rv$dt_raw)
        data.table::set(dt_new, j = "TIMESTAMP", value = v_result)
        if (ts_col != "TIMESTAMP") {
          dt_new[, (ts_col) := NULL]
        }
        rv$dt_raw <- dt_new
        rv$v_data_cols <- setdiff(names(rv$dt_raw), "TIMESTAMP")
      }

      go_to(2L)
    })

    # ---- step 2: site info --------------------------------------------------
    observeEvent(input$back_2, go_to(1L))

    # Load dt_site from CSV and pre-fill inputs
    observeEvent(input$site_csv, {
      info <- parseFilePaths(v_roots, input$site_csv)
      req(nrow(info) > 0)
      path <- as.character(info$datapath)

      dt <- tryCatch(
        data.table::fread(path, na.strings = c("", "NA"), showProgress = FALSE),
        error = function(e) {
          showNotification(
            paste("Error reading site file:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          NULL
        }
      )
      req(dt)

      v_required <- c("site", "long_name", "lat", "lon", "elev")
      v_missing <- setdiff(v_required, names(dt))
      if (length(v_missing)) {
        showNotification(
          paste(
            "dt_site CSV is missing columns:",
            paste(v_missing, collapse = ", ")
          ),
          type = "error",
          duration = 8
        )
        return()
      }

      rv$dt_site_from_file <- dt

      if (data.table::uniqueN(dt$site) == 1L) {
        prefill_site(dt[1L])
      }
    })

    # If the file has multiple sites, show a selector
    output$site_select_ui <- renderUI({
      req(rv$dt_site_from_file)
      v_sites <- unique(rv$dt_site_from_file$site)
      if (length(v_sites) <= 1L) {
        return(NULL)
      }
      selectInput(
        ns("site_select_from_file"),
        "Select site:",
        choices = v_sites,
        selected = v_sites[1L]
      )
    })

    observeEvent(input$site_select_from_file, {
      req(rv$dt_site_from_file, input$site_select_from_file)
      row <- rv$dt_site_from_file[site == input$site_select_from_file]
      req(nrow(row) > 0)
      prefill_site(row)
    })

    observeEvent(input$next_2, {
      if (!nzchar(trimws(input$site_id %||% ""))) {
        showNotification("Please enter a site ID.", type = "warning")
        return()
      }
      rv$dt_site <- data.table::data.table(
        site = input$site_id,
        long_name = input$site_long_name %||% "",
        lon = input$lon %||% NA_real_,
        lat = input$lat %||% NA_real_,
        elev = input$elev %||% NA_real_
      )
      go_to(3L)
    })

    # ---- step 3: variable mapping -------------------------------------------
    observeEvent(input$back_3, go_to(2L))

    # Load dt_meta from CSV (file path)
    observeEvent(input$meta_csv, {
      info <- parseFilePaths(v_roots, input$meta_csv)
      req(nrow(info) > 0)
      path <- as.character(info$datapath)

      dt <- tryCatch(
        data.table::fread(
          path,
          na.strings = c("", "NA", "#N/A"),
          showProgress = FALSE
        ),
        error = function(e) {
          showNotification(
            paste("Error reading dt_meta file:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          NULL
        }
      )
      req(dt)

      v_required <- c("site", "name_dt", "type", "name_icos", "units_local")
      v_missing <- setdiff(v_required, names(dt))
      if (length(v_missing)) {
        showNotification(
          paste(
            "dt_meta CSV is missing columns:",
            paste(v_missing, collapse = ", ")
          ),
          type = "error",
          duration = 8
        )
        return()
      }

      site_val <- trimws(input$site_id %||% "")
      if (nzchar(site_val) && "site" %in% names(dt)) {
        dt <- dt[site == site_val]
      }

      rv$dt_meta_from_file <- dt
    })

    # Match summary for file-loaded dt_meta
    output$meta_load_ui <- renderUI({
      req(rv$dt_meta_from_file, rv$dt_raw)

      dt_vars <- rv$dt_meta_from_file[
        !is.na(type) & type != "time" & type != "site"
      ]
      v_name_dt <- dt_vars$name_dt
      v_raw_cols <- names(rv$dt_raw)

      v_matched <- intersect(v_name_dt, v_raw_cols)
      v_unmatched <- setdiff(v_name_dt, v_raw_cols)

      tagList(
        tags$p(
          tags$b(paste0(nrow(rv$dt_meta_from_file), " rows loaded.")),
          paste0(
            " ",
            length(v_matched),
            " variable(s) matched to data columns."
          )
        ),
        if (length(v_matched)) {
          tags$p(
            tags$b("Matched: "),
            paste(v_matched, collapse = ", ")
          )
        },
        if (length(v_unmatched)) {
          tags$p(
            style = "color: #b8860b;",
            tags$b("Not found in data: "),
            paste(v_unmatched, collapse = ", ")
          )
        }
      )
    })

    # Mapping dropdowns (manual path)
    output$mapping_ui <- renderUI({
      req(rv$v_data_cols, rv$dt_raw)
      tagList(
        fluidRow(
          column(3, tags$b("Data column")),
          column(3, tags$b("Sample values")),
          column(6, tags$b("ICOS variable"))
        ),
        hr(),
        lapply(rv$v_data_cols, function(col_nm) {
          v_vals <- rv$dt_raw[[col_nm]]
          v_non_na <- v_vals[!is.na(v_vals)]
          sample_str <- if (length(v_non_na) == 0L) {
            "—"
          } else if (is.numeric(v_non_na)) {
            paste(signif(head(v_non_na, 3L), 3L), collapse = ", ")
          } else {
            paste(head(as.character(v_non_na), 3L), collapse = ", ")
          }
          fluidRow(
            column(
              3,
              tags$span(col_nm, style = "font-family: monospace;")
            ),
            column(
              3,
              tags$span(
                sample_str,
                style = "color: #666; font-size: 0.85em; font-family: monospace;"
              )
            ),
            column(
              6,
              selectInput(
                ns(safe_id("map_", col_nm)),
                label = NULL,
                choices = v_icos_choices,
                selected = ""
              )
            )
          )
        })
      )
    })

    observeEvent(input$next_3, {
      if (input$meta_source == "file") {
        # File path: use loaded dt_meta, skip step 4
        if (is.null(rv$dt_meta_from_file) || !nrow(rv$dt_meta_from_file)) {
          showNotification(
            "Please select a dt_meta CSV file first.",
            type = "warning"
          )
          return()
        }
        rv$dt_meta <- rv$dt_meta_from_file
        rv$v_mapped_cols <- rv$dt_meta_from_file[
          !is.na(type) & type != "time" & type != "site",
          name_dt
        ]
        rv$skipped_step4 <- TRUE
        go_to(5L)
      } else {
        # Manual path: build mapping then go to step 4
        req(rv$v_data_cols)

        v_icos_selected <- vapply(
          rv$v_data_cols,
          function(col_nm) {
            input[[safe_id("map_", col_nm)]] %||% ""
          },
          character(1L)
        )
        names(v_icos_selected) <- rv$v_data_cols

        if (!any(v_icos_selected != "")) {
          showNotification(
            "Please map at least one variable.",
            type = "warning"
          )
          return()
        }

        v_keep <- v_icos_selected != ""
        rv$v_mapped_cols <- rv$v_data_cols[v_keep]
        rv$v_mapping <- v_icos_selected[v_keep]
        rv$skipped_step4 <- FALSE
        go_to(4L)
      }
    })

    # ---- step 4: variable details -------------------------------------------
    observeEvent(input$back_4, go_to(3L))

    output$details_ui <- renderUI({
      req(rv$v_mapped_cols, rv$v_mapping)
      tagList(
        fluidRow(
          column(2, tags$b("ICOS name")),
          column(2, tags$b("Data column")),
          column(2, tags$b("Units")),
          column(2, tags$b("Range min")),
          column(2, tags$b("Range max")),
          column(2, tags$b("Imputation method"))
        ),
        hr(),
        lapply(rv$v_mapped_cols, function(col_nm) {
          icos_nm <- rv$v_mapping[[col_nm]]
          ref_row <- dt_icos_ref[name_icos == icos_nm]

          default_units <- if (
            nrow(ref_row) && !is.na(ref_row$units_icos[1L])
          ) {
            ref_row$units_icos[1L]
          } else {
            ""
          }
          default_rmin <- if (nrow(ref_row) && !is.na(ref_row$range_min[1L])) {
            ref_row$range_min[1L]
          } else {
            NA_real_
          }
          default_rmax <- if (nrow(ref_row) && !is.na(ref_row$range_max[1L])) {
            ref_row$range_max[1L]
          } else {
            NA_real_
          }
          default_method <- if (
            nrow(ref_row) && !is.na(ref_row$imputation_method[1L])
          ) {
            ref_row$imputation_method[1L]
          } else {
            "time"
          }

          fluidRow(
            column(2, tags$span(icos_nm, style = "font-weight: bold;")),
            column(
              2,
              tags$span(
                col_nm,
                style = "font-family: monospace; font-size: 0.9em;"
              )
            ),
            column(
              2,
              textInput(
                ns(safe_id("units_", col_nm)),
                label = NULL,
                value = default_units
              )
            ),
            column(
              2,
              numericInput(
                ns(safe_id("rmin_", col_nm)),
                label = NULL,
                value = default_rmin
              )
            ),
            column(
              2,
              numericInput(
                ns(safe_id("rmax_", col_nm)),
                label = NULL,
                value = default_rmax
              )
            ),
            column(
              2,
              selectInput(
                ns(safe_id("method_", col_nm)),
                label = NULL,
                choices = v_method_choices,
                selected = default_method
              )
            )
          )
        })
      )
    })

    observeEvent(input$next_4, {
      req(rv$v_mapped_cols, rv$v_mapping)

      start_ch <- paste(format(input$start_date, "%d/%m/%Y"), "00:00")
      end_ch <- paste(format(input$end_date, "%d/%m/%Y"), "00:00")

      # TIMESTAMP row
      dt_ts_row <- data.table::data.table(
        site = input$site_id,
        start_date = start_ch,
        end_date = end_ch,
        name_dt = "TIMESTAMP",
        name_local = "TIMESTAMP",
        long_name_local = "timestamp",
        units_local = NA_character_,
        type = "time",
        time_char_format = NA_character_,
        horizontal_id = NA_integer_,
        vertical_id = NA_integer_,
        replicate_id = NA_integer_,
        sensor_make = NA_character_,
        sensor_model = NA_character_,
        range_min = NA_real_,
        range_max = NA_real_,
        diff_ref_max = NA_real_,
        imputation_method = NA_character_,
        name_era5 = "time",
        long_name_era5 = NA_character_,
        units_era5 = NA_character_,
        name_icos = "TIMESTAMP",
        long_name_icos = "timestamp",
        units_icos = NA_character_
      )

      # site row (required by restrict() to keep the site column in dt)
      dt_site_row <- data.table::data.table(
        site = input$site_id,
        start_date = start_ch,
        end_date = end_ch,
        name_dt = "site",
        name_local = "site",
        long_name_local = "site",
        units_local = NA_character_,
        type = "site",
        time_char_format = NA_character_,
        horizontal_id = NA_integer_,
        vertical_id = NA_integer_,
        replicate_id = NA_integer_,
        sensor_make = NA_character_,
        sensor_model = NA_character_,
        range_min = NA_real_,
        range_max = NA_real_,
        diff_ref_max = NA_real_,
        imputation_method = NA_character_,
        name_era5 = "site",
        long_name_era5 = NA_character_,
        units_era5 = NA_character_,
        name_icos = "site",
        long_name_icos = "site",
        units_icos = NA_character_
      )

      # One row per mapped measurement variable
      l_var_rows <- lapply(rv$v_mapped_cols, function(col_nm) {
        icos_nm <- rv$v_mapping[[col_nm]]
        ref_row <- dt_icos_ref[name_icos == icos_nm]

        units <- input[[safe_id("units_", col_nm)]] %||% NA_character_
        rmin <- input[[safe_id("rmin_", col_nm)]]
        rmax <- input[[safe_id("rmax_", col_nm)]]
        method <- input[[safe_id("method_", col_nm)]] %||% "time"

        data.table::data.table(
          site = input$site_id,
          start_date = start_ch,
          end_date = end_ch,
          name_dt = col_nm,
          name_local = col_nm,
          long_name_local = if (nrow(ref_row)) {
            ref_row$long_name_icos[1L]
          } else {
            col_nm
          },
          units_local = if (is.null(units) || !nzchar(units)) {
            NA_character_
          } else {
            units
          },
          type = if (nrow(ref_row)) ref_row$type[1L] else "arbitrary",
          time_char_format = NA_character_,
          horizontal_id = NA_integer_,
          vertical_id = NA_integer_,
          replicate_id = NA_integer_,
          sensor_make = NA_character_,
          sensor_model = NA_character_,
          range_min = if (is.null(rmin) || is.na(rmin)) {
            NA_real_
          } else {
            as.numeric(rmin)
          },
          range_max = if (is.null(rmax) || is.na(rmax)) {
            NA_real_
          } else {
            as.numeric(rmax)
          },
          diff_ref_max = NA_real_,
          imputation_method = method,
          name_era5 = if (nrow(ref_row) && !is.na(ref_row$name_era5[1L])) {
            ref_row$name_era5[1L]
          } else {
            NA_character_
          },
          long_name_era5 = if (
            nrow(ref_row) && !is.na(ref_row$long_name_era5[1L])
          ) {
            ref_row$long_name_era5[1L]
          } else {
            NA_character_
          },
          units_era5 = if (nrow(ref_row) && !is.na(ref_row$units_era5[1L])) {
            ref_row$units_era5[1L]
          } else {
            NA_character_
          },
          name_icos = icos_nm,
          long_name_icos = if (nrow(ref_row)) {
            ref_row$long_name_icos[1L]
          } else {
            col_nm
          },
          units_icos = if (nrow(ref_row) && !is.na(ref_row$units_icos[1L])) {
            ref_row$units_icos[1L]
          } else {
            NA_character_
          }
        )
      })

      rv$dt_meta <- data.table::rbindlist(
        c(list(dt_ts_row, dt_site_row), l_var_rows)
      )

      go_to(5L)
    })

    # ---- step 5: ERA5 -------------------------------------------------------
    observeEvent(input$back_5, {
      if (isTRUE(rv$skipped_step4)) go_to(3L) else go_to(4L)
    })

    observeEvent(input$era5_file, {
      info <- parseFilePaths(v_roots, input$era5_file)
      req(nrow(info) > 0)
      rv$era5_path <- as.character(info$datapath)
    })

    output$era5_status_ui <- renderUI({
      req(rv$era5_path)
      tags$p(
        style = "color: #2e7d32;",
        paste("Selected:", basename(rv$era5_path))
      )
    })

    observeEvent(input$next_5, go_to(6L))

    # ---- step 6: review and save --------------------------------------------
    observeEvent(input$back_6, go_to(5L))

    observeEvent(input$restart, {
      rv$format <- NULL
      rv$dt_raw <- NULL
      rv$l_raw_tables <- NULL
      rv$multiple_tables <- FALSE
      rv$v_data_cols <- NULL
      rv$v_mapped_cols <- NULL
      rv$v_mapping <- NULL
      rv$dt_meta <- NULL
      rv$dt_site <- NULL
      rv$dt_site_from_file <- NULL
      rv$dt_meta_from_file <- NULL
      rv$skipped_step4 <- FALSE
      rv$era5_path <- NULL
      go_to(1L)
    })

    output$summary_ui <- renderUI({
      req(rv$dt_meta, rv$dt_site, rv$dt_raw)
      n_vars <- nrow(rv$dt_meta[type != "time" & type != "site"])
      v_icos_names <- rv$dt_meta[type != "time" & type != "site", name_icos]
      ts_range <- tryCatch(
        {
          v_ts <- rv$dt_raw$TIMESTAMP
          paste(
            format(min(v_ts, na.rm = TRUE), "%Y-%m-%d"),
            "to",
            format(max(v_ts, na.rm = TRUE), "%Y-%m-%d")
          )
        },
        error = function(e) "unknown"
      )
      tagList(
        tags$p(
          tags$b("Site: "),
          paste0(rv$dt_site$site, " (", rv$dt_site$long_name, ")")
        ),
        tags$p(tags$b("Variables mapped: "), n_vars),
        tags$p(tags$b("Data date range: "), ts_range),
        tags$p(tags$b("Variables: "), paste(v_icos_names, collapse = ", ")),
        if (isTRUE(input$era5_source == "yes") && !is.null(rv$era5_path)) {
          tags$p(
            tags$b("ERA5 file: "),
            basename(rv$era5_path)
          )
        } else {
          tags$p(tags$b("ERA5: "), "not attached")
        }
      )
    })

    output$save_rds <- downloadHandler(
      filename = function() {
        site_safe <- gsub("[^A-Za-z0-9_-]", "_", input$site_id %||% "site")
        paste0(site_safe, "_", format(Sys.Date(), "%Y%m%d"), "_metamet.rds")
      },
      content = function(file) {
        req(rv$dt_meta, rv$dt_site, rv$dt_raw, rv$v_mapped_cols)

        v_keep_cols <- intersect(
          c("TIMESTAMP", rv$v_mapped_cols),
          names(rv$dt_raw)
        )
        dt_input <- rv$dt_raw[, v_keep_cols, with = FALSE]

        mm <- tryCatch(
          {
            m <- metamet:::new_metamet(
              dt = dt_input,
              dt_meta = rv$dt_meta,
              dt_site = rv$dt_site,
              site_id = input$site_id
            )
            m <- metamet:::restrict(m)
            m <- metamet:::convert_time_char_to_posix(m)

            if (isTRUE(input$era5_source == "yes") && !is.null(rv$era5_path)) {
              m <- tryCatch(
                metamet::add_era5(m, fname_era5 = rv$era5_path),
                error = function(e) {
                  showNotification(
                    paste("ERA5 attach failed:", conditionMessage(e)),
                    type = "warning",
                    duration = 8
                  )
                  m
                }
              )
            }

            m
          },
          error = function(e) {
            showNotification(
              paste("Error building metamet:", conditionMessage(e)),
              type = "error",
              duration = 10
            )
            NULL
          }
        )

        if (is.null(mm)) {
          return()
        }

        # Initialise dt_qc (may silently fail if ranges are NA — that is ok)
        mm <- tryCatch(metamet::apply_qc(mm), error = function(e) mm)

        saveRDS(mm, file)
      }
    )
  })
}
