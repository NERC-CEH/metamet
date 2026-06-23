# this test checks that the add data flag excel file exists and that cols and
# formatting is ok
test_that("data flag spreadsheet exists and has correct format", {
  # path to excel file flag spreadsheet
  qc_file <- pkg_extdata("data_flag_template.xlsx")
  # check if you can read the file
  data_flag_sheet <- readxl::read_excel(
    path = qc_file
  )
  # does the file exist?
  expect_true(
    fs::file_exists(qc_file),
    info = sprintf("File not found at: %s", qc_file)
  )
  # file extention check (xlsx or xl)
  ext <- tolower(fs::path_ext(qc_file))
  expect_true(
    ext %in% c("xlsx", "xls"),
    info = sprintf("File must be .xlsx or .xls, but found: .%s", ext)
  )
})

test_that("QC sheet exists and has the correct cols", {
  qc_file <- pkg_extdata("data_flag_template.xlsx")
  skip_if_not(fs::file_exists(qc_file), "QC file missing")
  # check the name of the sheet
  sheets <- readxl::excel_sheets(qc_file)
  expect_true(
    "QC" %in% sheets,
    info = sprintf(
      "Sheet QC not found. The only sheets present are: %s",
      paste(sheets, collapse = ", ")
    )
  )
  # Read QC sheet
  qc_df <- readxl::read_excel(qc_file, sheet = "QC")
  # check cols names in QC sheet
  required_cols <- c("start_time", "end_time", "var_name", "comment")
  missing_cols <- setdiff(required_cols, names(qc_df))
  expect_equal(
    length(missing_cols),
    0,
    info = sprintf(
      "QC sheet is missing the required columns: %s",
      paste(missing_cols, collapse = ", ")
    )
  )
})
