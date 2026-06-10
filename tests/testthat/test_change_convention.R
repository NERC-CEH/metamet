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

  # degC and degree_C both normalise to "celsius" -> no numeric conversion
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
