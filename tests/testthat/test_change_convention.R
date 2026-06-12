test_that("changing naming convention works", {
  # mm without qc or ref
  dim(mm1$dt)
  dim(mm1$dt_qc)
  dim(mm1$dt_ref)

  names(mm1$dt)
  mm1_icos <- suppressWarnings(change_naming_convention(
    mm1,
    name_convention = "name_icos"
  ))
  names(mm1_icos$dt)
  mm1_era5 <- suppressWarnings(change_naming_convention(
    mm1,
    name_convention = "name_era5"
  ))
  names(mm1_era5$dt)

  expect_s3_class(mm1, "metamet")
  expect_s3_class(mm1$dt, "data.table")
  # check that the time variables are correctly named
  expect_s3_class(mm1_icos$dt[, TIMESTAMP], "POSIXct")
  expect_s3_class(mm1_era5$dt[, time], "POSIXct")
})

test_that("change_naming_convention converts units in wide format", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  # Set known values: temp = 10 hPa, flux = 3 cm
  mm$dt[, temp := 10.0]
  mm$dt[, flux := 3.0]

  mm_icos <- suppressWarnings(change_naming_convention(mm, "name_icos"))

  # 10 hPa -> kPa = 1.0; 3 cm -> m = 0.03
  expect_equal(mm_icos$dt$TA, 1.0, tolerance = 1e-10)
  expect_equal(mm_icos$dt$NEE, 0.03, tolerance = 1e-10)
})

test_that("change_naming_convention converts units in dt_ref", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  mm$dt_ref[, temp := 5.0]
  mm$dt_ref[, flux := 200.0]

  mm_icos <- suppressWarnings(change_naming_convention(mm, "name_icos"))

  expect_equal(mm_icos$dt_ref$TA, 0.5, tolerance = 1e-10)
  expect_equal(mm_icos$dt_ref$NEE, 2.0, tolerance = 1e-10)
})

test_that("change_naming_convention: string alias produces no numeric change", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "degC", NA_character_)]
  mm$dt_meta[, units_icos := c(NA_character_, "degree_C", NA_character_)]
  mm$dt[, temp := 25.0]

  mm_icos <- suppressWarnings(change_naming_convention(mm, "name_icos"))

  # degC and degree_C both normalise to "degC" -> no numeric conversion
  expect_equal(mm_icos$dt$TA, 25.0)
})

test_that("change_naming_convention respects convert_units = FALSE", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  mm$dt[, temp := 10.0]
  mm$dt[, flux := 3.0]

  mm_icos <- suppressWarnings(change_naming_convention(
    mm,
    "name_icos",
    convert_units = FALSE
  ))

  # Names changed but values unchanged
  expect_equal(mm_icos$dt$TA, 10.0)
  expect_equal(mm_icos$dt$NEE, 3.0)
})

test_that("change_naming_convention convert_units = 'obs' converts value but not ref", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  mm$dt[, temp := 10.0]
  mm$dt[, flux := 3.0]
  mm$dt_ref[, temp := 10.0]
  mm$dt_ref[, flux := 3.0]

  mm_icos <- suppressWarnings(change_naming_convention(
    mm,
    "name_icos",
    convert_units = "obs"
  ))

  expect_equal(mm_icos$dt$TA, 1.0, tolerance = 1e-10) # converted
  expect_equal(mm_icos$dt$NEE, 0.03, tolerance = 1e-10) # converted
  expect_equal(mm_icos$dt_ref$TA, 10.0) # unchanged
  expect_equal(mm_icos$dt_ref$NEE, 3.0) # unchanged
})

test_that("change_naming_convention convert_units = 'ref' converts ref but not value", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  mm$dt[, temp := 10.0]
  mm$dt[, flux := 3.0]
  mm$dt_ref[, temp := 10.0]
  mm$dt_ref[, flux := 3.0]

  mm_icos <- suppressWarnings(change_naming_convention(
    mm,
    "name_icos",
    convert_units = "ref"
  ))

  expect_equal(mm_icos$dt$TA, 10.0) # unchanged
  expect_equal(mm_icos$dt$NEE, 3.0) # unchanged
  expect_equal(mm_icos$dt_ref$TA, 1.0, tolerance = 1e-10) # converted
  expect_equal(mm_icos$dt_ref$NEE, 0.03, tolerance = 1e-10) # converted
})

test_that("change_naming_convention silently skips when no units columns", {
  mm <- make_test_metamet()
  # No units_local / units_icos in dt_meta

  expect_no_warning(suppressWarnings(change_naming_convention(mm, "name_icos")))
  mm_icos <- suppressWarnings(change_naming_convention(mm, "name_icos"))
  expect_equal(mm_icos$dt$TA, 10.0)
  expect_equal(mm_icos$dt$NEE, 3.0)
})

test_that("change_naming_convention updates name_convention attribute", {
  mm <- make_test_metamet()
  mm_icos <- suppressWarnings(change_naming_convention(mm, "name_icos"))
  expect_equal(attr(mm_icos, "name_convention"), "name_icos")
})

test_that("change_naming_convention converts units in long format", {
  mm <- make_test_metamet()
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  mm$dt[, temp := 10.0]
  mm$dt[, flux := 3.0]

  mm_long <- metamet_reshape(mm, "long")
  mm_icos_long <- suppressWarnings(change_naming_convention(
    mm_long,
    "name_icos"
  ))

  expect_equal(mm_icos_long$dt[var_name == "TA", value], 1.0, tolerance = 1e-10)
  expect_equal(
    mm_icos_long$dt[var_name == "NEE", value],
    0.03,
    tolerance = 1e-10
  )
})

test_that("change_naming_convention: unit conversion round-trips local -> icos -> era5 -> local", {
  mm <- make_test_metamet()
  mm$dt_meta[, name_local := c("TIMESTAMP", "temp", "flux")]
  mm$dt_meta[, name_era5 := c("time", "t2m", "flux_era5")]
  mm$dt_meta[, units_local := c(NA_character_, "hPa", "cm")]
  mm$dt_meta[, units_icos := c(NA_character_, "kPa", "m")]
  mm$dt_meta[, units_era5 := c(NA_character_, "Pa", "mm")]
  mm$dt[, temp := 100.0]
  mm$dt[, flux := 3.0]

  mm_icos <- suppressWarnings(change_naming_convention(mm, "name_icos"))
  mm_era5 <- suppressWarnings(change_naming_convention(mm_icos, "name_era5"))
  mm_back <- suppressWarnings(change_naming_convention(mm_era5, "name_local"))

  # Values must be identical to originals within floating-point precision
  expect_equal(mm_back$dt$temp, 100.0, tolerance = 1e-10)
  expect_equal(mm_back$dt$flux, 3.0, tolerance = 1e-10)
  # Convention attribute must be restored
  expect_equal(attr(mm_back, "name_convention"), "name_local")
  # Intermediate values: 100 hPa -> 10 kPa -> 10000 Pa
  expect_equal(mm_icos$dt$TA, 10.0, tolerance = 1e-10)
  expect_equal(mm_era5$dt$t2m, 10000.0, tolerance = 1e-10)
})

test_that("change_naming_convention: real data, degC -> K adds 273.15", {
  mm_test <- suppressMessages(metamet(
    dt = pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv"),
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  ))

  orig_ta <- mm_test$dt[["TA_4_1_1"]]

  # Override units_icos for this column to K to test affine temperature conversion
  mm_test$dt_meta[name_dt == "TA_4_1_1", units_icos := "K"]

  mm_icos <- suppressWarnings(change_naming_convention(mm_test, "name_icos"))

  # TA_4_1_1 keeps the same column name: name_icos="TA" + ids 4_1_1 = "TA_4_1_1"
  # degC -> K: values must be exactly +273.15
  expect_equal(mm_icos$dt[["TA_4_1_1"]], orig_ta + 273.15, tolerance = 1e-6)
})

test_that("change_naming_convention: real data, ERA5 convention renames correctly", {
  mm_test <- suppressMessages(metamet(
    dt = pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv"),
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  ))

  orig_ta <- mm_test$dt[["TA_4_1_1"]]
  orig_ws <- mm_test$dt[["WS_6_1_1"]]

  # artificially change the ERA5 temperature and windspeed units to Kelvin
  # and cm/s as a more thorough test
  mm_test$dt_meta[name_icos == "TA", units_era5 := "K"]
  mm_test$dt_meta[name_icos == "WS", units_era5 := "cm/s"]

  mm_era5 <- suppressWarnings(change_naming_convention(mm_test, "name_era5"))

  # Time column renamed: DATECT -> time
  expect_s3_class(mm_era5$dt[["time"]], "POSIXct")
  # Temperature renamed: TA_4_1_1 -> t2m_4_1_1 (name_era5="t2m" + ids 4_1_1)
  expect_true("t2m_4_1_1" %in% names(mm_era5$dt))
  # Temperature values changed: K = degree_C + 273.15; cm = m * 100
  expect_equal(mm_era5$dt[["t2m_4_1_1"]], orig_ta + 273.15, tolerance = 1e-10)
  expect_equal(mm_era5$dt[["ws_6_1_1"]], orig_ws * 100, tolerance = 1e-10)
  # Convention attribute updated
  expect_equal(attr(mm_era5, "name_convention"), "name_era5")
})

test_that("change_naming_convention works on long-format objects", {
  mm_long <- suppressWarnings(metamet_reshape(mm1, "long"))
  original_var_names <- unique(mm_long$dt$var_name)

  mm_icos <- suppressWarnings(change_naming_convention(mm_long, "name_icos"))

  expect_equal(attr(mm_icos, "format"), "long")
  # var_name values should have changed
  expect_false(identical(
    sort(unique(mm_icos$dt$var_name)),
    sort(original_var_names)
  ))
  # dt_meta name_dt should match the new var_name values
  expect_setequal(
    unique(mm_icos$dt$var_name),
    mm_icos$dt_meta[name_dt %in% unique(mm_icos$dt$var_name), name_dt]
  )
})
