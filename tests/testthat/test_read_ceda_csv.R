amo_file <- pkg_extdata("UK-AMO/historical/ceda/AU_MetData_2023.csv")
buc_file <- pkg_extdata("UK-BUC/historical/ceda/BushCabin_2024.csv")

test_that("read_ceda_csv returns a data.table for UK-AMO format", {
  dt <- read_ceda_csv(amo_file)
  expect_s3_class(dt, "data.table")
})

test_that("read_ceda_csv TIMESTAMP is POSIXct for UK-AMO format", {
  dt <- read_ceda_csv(amo_file)
  expect_s3_class(dt$TIMESTAMP, "POSIXct")
  expect_equal(attr(dt$TIMESTAMP, "tzone"), "UTC")
})

# test_that("read_ceda_csv first TIMESTAMP is correct for UK-AMO", {
#   dt <- read_ceda_csv(amo_file)
#   expect_equal(dt$TIMESTAMP[1L], as.POSIXct("2023-01-01 00:30:00", tz = "UTC"))
# })

test_that("read_ceda_csv uses short_name for column names in UK-AMO format", {
  dt <- read_ceda_csv(amo_file)
  expect_true("T_upper_Avg" %in% names(dt))
  expect_true("T_RHT_Avg" %in% names(dt))
})

test_that("read_ceda_csv drops flag columns by default for UK-AMO", {
  dt <- read_ceda_csv(amo_file)
  expect_false(any(grepl("^(Flag|FLAG)\\b", names(dt))))
})

test_that("read_ceda_csv keeps flag columns when drop_flags = FALSE for UK-AMO", {
  dt <- read_ceda_csv(amo_file, drop_flags = FALSE)
  expect_true(any(grepl("^Flag", names(dt))))
})

test_that("read_ceda_csv TIMESTAMP column is first column", {
  dt <- read_ceda_csv(amo_file)
  expect_equal(names(dt)[1L], "TIMESTAMP")
})

test_that("read_ceda_csv drops end_data and empty trailing rows for UK-AMO", {
  dt <- read_ceda_csv(amo_file)
  expect_false(anyNA(dt$TIMESTAMP))
})

test_that("read_ceda_csv drops end_data row for UK-BUC", {
  dt <- read_ceda_csv(buc_file)
  expect_false(anyNA(dt$TIMESTAMP))
})

test_that("read_ceda_csv returns a data.table for UK-BUC format", {
  dt <- read_ceda_csv(buc_file)
  expect_s3_class(dt, "data.table")
})

test_that("read_ceda_csv TIMESTAMP is POSIXct for UK-BUC format", {
  dt <- read_ceda_csv(buc_file)
  expect_s3_class(dt$TIMESTAMP, "POSIXct")
  expect_equal(attr(dt$TIMESTAMP, "tzone"), "UTC")
})

# test_that("read_ceda_csv first TIMESTAMP is correct for UK-BUC", {
#   dt <- read_ceda_csv(buc_file)
#   expect_equal(dt$TIMESTAMP[1L], as.POSIXct("2024-01-01 00:30:00", tz = "UTC"))
# })

test_that("read_ceda_csv uses Short_name for column names in UK-BUC format", {
  dt <- read_ceda_csv(buc_file)
  expect_true("RG_1_1_1" %in% names(dt))
  expect_true("TA_1_1_1" %in% names(dt))
})

test_that("read_ceda_csv drops flag columns by default for UK-BUC", {
  dt <- read_ceda_csv(buc_file)
  expect_false(any(grepl("^(Flag|FLAG)\\b", names(dt))))
})

test_that("read_ceda_csv drops packed YYYYMMDDHHMM datetime column for UK-BUC", {
  dt <- read_ceda_csv(buc_file)
  packed_cols <- Filter(
    function(col) {
      vals <- dt[[col]]
      is.numeric(vals) &&
        length(vals) > 0L &&
        all(!is.na(vals) | is.na(vals)) &&
        {
          non_na <- vals[!is.na(vals)]
          length(non_na) > 0L &&
            all(non_na >= 198001010000 & non_na <= 209912312359)
        }
    },
    names(dt)
  )
  expect_length(packed_cols, 0L)
})

test_that("read_ceda_csv converts -9999 to NA for UK-BUC", {
  dt_flags <- read_ceda_csv(buc_file, drop_flags = FALSE)
  # Some cells may have been -9999 in the source; at minimum no -9999 survives
  numeric_cols <- names(dt_flags)[vapply(dt_flags, is.numeric, logical(1L))]
  numeric_cols <- setdiff(numeric_cols, "TIMESTAMP")
  for (col in numeric_cols) {
    expect_false(any(dt_flags[[col]] == -9999, na.rm = TRUE), info = col)
  }
})

test_that("read_ceda_csv errors on non-BADC-CSV file", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("title,G,some title", "creator,G,someone", "x,y,z"), tmp)
  on.exit(unlink(tmp))
  expect_error(read_ceda_csv(tmp), "Cannot find column-index row")
})
