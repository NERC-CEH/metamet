# Normalise a data.table for value comparison: clear keys and sort columns
# so that dcast's alphabetical column ordering does not cause false failures.
normalise_dt <- function(dt) {
  dt <- data.table::copy(dt)
  data.table::setkey(dt, NULL)
  data.table::setcolorder(dt, sort(names(dt)))
  dt[order(site, TIMESTAMP)]
}

# ---- metamet_wide_to_long ---------------------------------------------

test_that("wide_to_long produces expected long-format columns", {
  mm <- make_test_metamet()
  mm_long <- metamet_wide_to_long(mm)

  expected_cols <- c(
    "site",
    "TIMESTAMP",
    "var_name",
    "value",
    "qc",
    "validator",
    "ref",
    "type",
    "name_icos"
  )
  expect_true(all(expected_cols %in% names(mm_long$dt)))
})

test_that("wide_to_long gives one row per variable per timestamp", {
  mm <- make_test_metamet()
  mm_long <- metamet_wide_to_long(mm)

  expect_equal(nrow(mm_long$dt), 2L)
  expect_setequal(as.character(mm_long$dt$var_name), c("temp", "flux"))
})

test_that("wide_to_long nulls out dt_qc and dt_ref", {
  mm <- make_test_metamet()
  mm_long <- metamet_wide_to_long(mm)

  expect_null(mm_long$dt_qc)
  expect_null(mm_long$dt_ref)
})

test_that("wide_to_long works when dt_qc and dt_ref are NULL", {
  mm <- make_test_metamet(include_qc = FALSE, include_ref = FALSE)
  expect_no_error(metamet_wide_to_long(mm))
})

test_that("wide_to_long merges qc values correctly", {
  mm <- make_test_metamet()
  mm_long <- metamet_wide_to_long(mm)

  expect_equal(mm_long$dt[var_name == "temp", qc], 0)
  expect_equal(mm_long$dt[var_name == "flux", qc], 1)
})

# ---- metamet_long_to_wide ---------------------------------------------

test_that("long_to_wide fails with informative error on wide input", {
  mm <- make_test_metamet()
  expect_error(metamet_long_to_wide(mm), "not a valid long-format")
})

test_that("long_to_wide restores dt_qc and dt_ref", {
  mm <- make_test_metamet()
  mm2 <- metamet_long_to_wide(metamet_wide_to_long(mm))

  expect_false(is.null(mm2$dt_qc))
  expect_false(is.null(mm2$dt_ref))
})

test_that("long_to_wide leaves dt_qc and dt_ref NULL when absent", {
  mm <- make_test_metamet(include_qc = FALSE, include_ref = FALSE)
  mm2 <- metamet_long_to_wide(metamet_wide_to_long(mm))

  expect_null(mm2$dt_qc)
  expect_null(mm2$dt_ref)
})

# ---- Round-trip -------------------------------------------------------

test_that("wide -> long -> wide round-trip preserves dt values", {
  mm <- make_test_metamet()
  mm2 <- metamet_long_to_wide(metamet_wide_to_long(mm))

  expect_equal(normalise_dt(mm2$dt), normalise_dt(mm$dt))
})

test_that("wide -> long -> wide round-trip preserves dt_qc values", {
  mm <- make_test_metamet()
  mm2 <- metamet_long_to_wide(metamet_wide_to_long(mm))

  expect_equal(normalise_dt(mm2$dt_qc), normalise_dt(mm$dt_qc))
})

test_that("wide -> long -> wide round-trip preserves dt_ref values", {
  mm <- make_test_metamet()
  mm2 <- metamet_long_to_wide(metamet_wide_to_long(mm))

  expect_equal(normalise_dt(mm2$dt_ref), normalise_dt(mm$dt_ref))
})

test_that("round-trip works without dt_qc and dt_ref", {
  mm <- make_test_metamet(include_qc = FALSE, include_ref = FALSE)
  mm2 <- metamet_long_to_wide(metamet_wide_to_long(mm))

  expect_equal(normalise_dt(mm2$dt), normalise_dt(mm$dt))
})

# ---- metamet_reshape dispatcher ---------------------------------------

test_that("metamet_reshape updates format attribute", {
  mm <- make_test_metamet()
  expect_equal(attr(mm, "format"), "wide")

  mm_long <- metamet_reshape(mm, "long")
  expect_equal(attr(mm_long, "format"), "long")

  mm_wide <- metamet_reshape(mm_long, "wide")
  expect_equal(attr(mm_wide, "format"), "wide")
})

test_that("metamet_reshape returns unchanged object if already in target format", {
  mm <- make_test_metamet()
  mm2 <- metamet_reshape(mm, "wide")

  expect_identical(mm, mm2)
})

test_that("metamet_reshape warns and assumes wide if format attribute is missing", {
  mm <- make_test_metamet()
  attr(mm, "format") <- NULL

  expect_warning(metamet_reshape(mm, "long"), "no format attribute")
})
