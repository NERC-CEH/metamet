dat_file <- pkg_extdata("UK-WHM/current/whim_met_2026_01_05.dat")
dld_file <- pkg_extdata("UK-WHM/current/Whim23X260609.dld")

# ---- DLD parser -----------------------------------------------------------

test_that(".parse_campbell_dld finds two Output_Table definitions", {
  tables <- metamet:::.parse_campbell_dld(dld_file)
  expect_length(tables, 2L)
  expect_setequal(names(tables), c("120", "226"))
})

test_that(".parse_campbell_dld table 120 has 21 column names", {
  tables <- metamet:::.parse_campbell_dld(dld_file)
  expect_length(tables[["120"]], 21L)
})

test_that(".parse_campbell_dld table 226 has 10 column names", {
  tables <- metamet:::.parse_campbell_dld(dld_file)
  expect_length(tables[["226"]], 10L)
})

test_that(".parse_campbell_dld renames _RTM columns", {
  tables <- metamet:::.parse_campbell_dld(dld_file)
  expect_true("day_of_year" %in% tables[["120"]])
  expect_true("time_hhmm" %in% tables[["120"]])
  expect_false("_RTM" %in% tables[["120"]])
})

test_that(".parse_campbell_dld renames numeric column 1 to table_id", {
  tables <- metamet:::.parse_campbell_dld(dld_file)
  expect_equal(tables[["120"]][1L], "table_id")
  expect_equal(tables[["226"]][1L], "table_id")
})

# ---- read_old_campbell_dat ------------------------------------------------

test_that("read_old_campbell_dat returns a named list", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_type(result, "list")
  expect_setequal(names(result), c("120", "227"))
})

test_that("read_old_campbell_dat returns data.tables", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_s3_class(result[["120"]], "data.table")
  expect_s3_class(result[["227"]], "data.table")
})

test_that("TIMESTAMP columns are POSIXct UTC", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_s3_class(result[["120"]]$TIMESTAMP, "POSIXct")
  expect_s3_class(result[["227"]]$TIMESTAMP, "POSIXct")
  expect_equal(attr(result[["120"]]$TIMESTAMP, "tzone"), "UTC")
})

test_that("table 120 first TIMESTAMP is 2025-12-22 10:15 UTC", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_equal(
    result[["120"]]$TIMESTAMP[1L],
    as.POSIXct("2025-12-22 10:15:00", tz = "UTC")
  )
})

test_that("table 227 first TIMESTAMP is 2025-12-22 10:08 UTC", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_equal(
    result[["227"]]$TIMESTAMP[1L],
    as.POSIXct("2025-12-22 10:08:00", tz = "UTC")
  )
})

test_that("year rollover: last timestamps are in 2026", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  last_120 <- result[["120"]]$TIMESTAMP[nrow(result[["120"]])]
  last_227 <- result[["227"]]$TIMESTAMP[nrow(result[["227"]])]
  expect_equal(as.integer(format(last_120, "%Y", tz = "UTC")), 2026L)
  expect_equal(as.integer(format(last_227, "%Y", tz = "UTC")), 2026L)
})

test_that("table 120 has TIMESTAMP plus 18 measurement columns", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_equal(ncol(result[["120"]]), 19L)
  expect_equal(names(result[["120"]])[1L], "TIMESTAMP")
})

test_that("table 227 has TIMESTAMP plus 7 measurement columns", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_equal(ncol(result[["227"]]), 8L)
  expect_equal(names(result[["227"]])[1L], "TIMESTAMP")
})

test_that("table 120 measurement column names match DLD", {
  result <- read_old_campbell_dat(dat_file, dld_file)
  expect_true("AirTemp_AVG" %in% names(result[["120"]]))
  expect_true("PAR_AVG" %in% names(result[["120"]]))
  expect_true("Rainfall_TOT" %in% names(result[["120"]]))
})

test_that("table_id argument returns a single data.table", {
  result <- read_old_campbell_dat(dat_file, dld_file, table_id = 120)
  expect_s3_class(result, "data.table")
  expect_false(is.list(result) && !is.data.frame(result))
})

test_that("table_id with unknown ID errors", {
  expect_error(
    read_old_campbell_dat(dat_file, dld_file, table_id = 999),
    "not found"
  )
})

test_that("explicit year overrides filename inference", {
  result_auto <- read_old_campbell_dat(dat_file, dld_file)
  result_explicit <- read_old_campbell_dat(dat_file, dld_file, year = 2025)
  expect_identical(
    result_auto[["120"]]$TIMESTAMP,
    result_explicit[["120"]]$TIMESTAMP
  )
})
