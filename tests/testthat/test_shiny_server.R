app_file <- system.file("shinyApp/metqc_app/app.R", package = "metamet")

# ---- output$status --------------------------------------------------------

test_that("status output shows 'no file' message before any upload", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_file, {
    expect_equal(output$status, "No file selected yet.")
  })
})

# ---- df_daterange() -------------------------------------------------------

test_that("df_daterange parses date and time inputs to POSIXct", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_file, {
    session$setInputs(
      sdate = as.Date("2023-06-01"),
      edate = as.Date("2023-06-30"),
      shour = 0L,
      smin = 30L,
      ehour = 23L,
      emin = 30L,
      retrieve_data = 1L
    )
    res <- df_daterange()
    expect_s3_class(res$start_date, "POSIXct")
    expect_s3_class(res$end_date, "POSIXct")
    expect_equal(
      format(res$start_date, "%Y-%m-%d %H:%M", tz = "UTC"),
      "2023-06-01 00:30"
    )
    expect_equal(
      format(res$end_date, "%Y-%m-%d %H:%M", tz = "UTC"),
      "2023-06-30 23:30"
    )
  })
})

test_that("df_daterange start_date_ch is a correctly formatted string", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_file, {
    session$setInputs(
      sdate = as.Date("2023-01-05"),
      edate = as.Date("2023-01-06"),
      shour = 9L,
      smin = 5L,
      ehour = 0L,
      emin = 0L,
      retrieve_data = 1L
    )
    res <- df_daterange()
    expect_equal(res$start_date_ch, "05/01/2023 09:05")
  })
})

# ---- ggiraph_plot() -------------------------------------------------------

test_that("ggiraph_plot returns a girafe object for a valid variable", {
  skip_if_not_installed("ggiraph")
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))
  mm_s <- subset_by_date(
    mm_long,
    start_date = "2023-06-01 00:30:00",
    end_date = "2023-06-02 00:00:00"
  )
  mm_s$dt[, row_name := as.factor(.I)]

  assign("mm_qry", mm_s, envir = .GlobalEnv)
  on.exit(rm("mm_qry", envir = .GlobalEnv), add = TRUE)

  result <- ggiraph_plot("RH")
  expect_s3_class(result, "girafe")
})

test_that("ggiraph_plot does not mutate mm_qry$dt", {
  skip_if_not_installed("ggiraph")
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))
  mm_s <- subset_by_date(
    mm_long,
    start_date = "2023-06-01 00:30:00",
    end_date = "2023-06-02 00:00:00"
  )
  mm_s$dt[, row_name := as.factor(.I)]
  cols_before <- names(mm_s$dt)

  assign("mm_qry", mm_s, envir = .GlobalEnv)
  on.exit(rm("mm_qry", envir = .GlobalEnv), add = TRUE)

  ggiraph_plot("RH")
  expect_equal(names(mm_qry$dt), cols_before)
})
