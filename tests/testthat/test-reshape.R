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

# ---- Round-trip helpers ---------------------------------------------------

# Like normalise_dt but also renames a non-standard time column to TIMESTAMP
# so that originals with e.g. "Timestamp" can be compared after round-trip.
normalise_wide <- function(dt, time_col = "TIMESTAMP") {
  dt <- data.table::copy(dt)
  if (time_col %in% names(dt) && time_col != "TIMESTAMP")
    data.table::setnames(dt, time_col, "TIMESTAMP")
  data.table::setkey(dt, NULL)
  data.table::setcolorder(dt, sort(names(dt)))
  dt[order(site, TIMESTAMP)]
}

# ---- Round-trip: wide -> long -> wide (real site data) -------------------

test_that("wide->long->wide round-trip preserves AMO dt values", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  v_time <- mm$dt_meta[type == "time", name_dt]
  mm2 <- suppressWarnings(metamet_long_to_wide(metamet_wide_to_long(mm)))

  expect_equal(normalise_wide(mm2$dt, v_time), normalise_wide(mm$dt, v_time))
})

test_that("wide->long->wide round-trip preserves AMO dt_qc values", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  v_time <- mm$dt_meta[type == "time", name_dt]
  mm2 <- suppressWarnings(metamet_long_to_wide(metamet_wide_to_long(mm)))

  expect_false(is.null(mm2$dt_qc))
  expect_equal(normalise_wide(mm2$dt_qc, v_time), normalise_wide(mm$dt_qc, v_time))
})

test_that("wide->long->wide round-trip preserves AMO dt_ref values", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  v_time <- mm$dt_meta[type == "time", name_dt]
  mm2 <- suppressWarnings(metamet_long_to_wide(metamet_wide_to_long(mm)))

  expect_false(is.null(mm2$dt_ref))
  expect_equal(normalise_wide(mm2$dt_ref, v_time), normalise_wide(mm$dt_ref, v_time))
})

test_that("wide->long->wide round-trip preserves EBU dt values", {
  mm <- readRDS(pkg_extdata("UK-EBU/UK-EBU_BM_mm_2023.rds"))
  v_time <- mm$dt_meta[type == "time", name_dt]
  mm2 <- suppressWarnings(metamet_long_to_wide(metamet_wide_to_long(mm)))

  # NA-site rows (all-NA padding artefacts) are intentionally dropped during
  # the wide->long step, so exclude them from the original before comparing.
  mm_dt_clean <- mm$dt[!is.na(mm$dt$site)]
  expect_equal(normalise_wide(mm2$dt, v_time), normalise_wide(mm_dt_clean, v_time))
})

test_that("wide->long->wide round-trip preserves WHM dt values (Timestamp -> TIMESTAMP rename)", {
  mm <- readRDS(pkg_extdata("UK-WHM/UK-WHM_BM_mm_2023.rds"))
  v_time <- mm$dt_meta[type == "time", name_dt]
  expect_false(v_time == "TIMESTAMP")  # confirm non-standard time name
  mm2 <- suppressWarnings(metamet_long_to_wide(metamet_wide_to_long(mm)))

  expect_equal(normalise_wide(mm2$dt, v_time), normalise_wide(mm$dt, v_time))
})

test_that("wide->long->wide round-trip preserves WHM dt_qc values", {
  mm <- readRDS(pkg_extdata("UK-WHM/UK-WHM_BM_mm_2023.rds"))
  v_time <- mm$dt_meta[type == "time", name_dt]
  mm2 <- suppressWarnings(metamet_long_to_wide(metamet_wide_to_long(mm)))

  expect_false(is.null(mm2$dt_qc))
  expect_equal(normalise_wide(mm2$dt_qc, v_time), normalise_wide(mm$dt_qc, v_time))
})

# ---- Round-trip: long -> wide -> long (combined multi-site data) ---------

test_that("long->wide->long round-trip preserves non-NA values in combined data", {
  mm <- readRDS(pkg_extdata("mm_amo_ebu_whm_2023.rds"))
  attr(mm, "format") <- "long"

  mm2 <- metamet_wide_to_long(metamet_long_to_wide(mm))

  # All non-NA observations from the original should be present and unchanged
  v_keys <- c("site", "TIMESTAMP", "var_name")
  dt_orig <- data.table::setkeyv(
    mm$dt[!is.na(value), c(v_keys, "value"), with = FALSE], v_keys
  )
  dt_rt <- data.table::setkeyv(
    mm2$dt[!is.na(value), c(v_keys, "value"), with = FALSE], v_keys
  )
  expect_equal(dt_rt, dt_orig)
})

test_that("long->wide->long round-trip has no duplicate keys in combined data", {
  mm <- readRDS(pkg_extdata("mm_amo_ebu_whm_2023.rds"))
  attr(mm, "format") <- "long"

  mm2 <- metamet_wide_to_long(metamet_long_to_wide(mm))
  n_dup <- nrow(mm2$dt[duplicated(mm2$dt[, .(site, TIMESTAMP, var_name)]), ])
  expect_equal(n_dup, 0L)
})

test_that("long->wide->long round-trip preserves all three sites", {
  mm <- readRDS(pkg_extdata("mm_amo_ebu_whm_2023.rds"))
  attr(mm, "format") <- "long"

  mm2 <- metamet_wide_to_long(metamet_long_to_wide(mm))
  expect_true(all(c("UK-AMO", "UK-EBU", "UK-WHM") %in% unique(mm2$dt$site)))
})

# ---- Real site data (wide -> long) ----------------------------------------

test_that("reshape to long on real AMO data has expected structure", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))

  expect_equal(attr(mm_long, "format"), "long")
  expect_true(all(.met_long_cols %in% names(mm_long$dt)))
  expect_equal(sum(is.na(mm_long$dt$TIMESTAMP)), 0L)
  expect_null(mm_long$dt_qc)
  expect_null(mm_long$dt_ref)
  n_dup <- nrow(mm_long$dt[duplicated(mm_long$dt[, .(site, TIMESTAMP, var_name)]), ])
  expect_equal(n_dup, 0L)
})

test_that("reshape to long produces n_timestamps * n_vars rows", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  v_time_name <- mm$dt_meta[type == "time", name_dt]
  v_data_cols <- setdiff(names(mm$dt), c("site", v_time_name))
  n_expected <- nrow(mm$dt) * length(v_data_cols)

  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))
  expect_equal(nrow(mm_long$dt), n_expected)
})

test_that("wide_to_long renames non-standard time column to TIMESTAMP", {
  mm <- readRDS(pkg_extdata("UK-WHM/UK-WHM_BM_mm_2023.rds"))
  v_time_name <- mm$dt_meta[type == "time", name_dt]
  expect_false(v_time_name == "TIMESTAMP")

  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))
  expect_true("TIMESTAMP" %in% names(mm_long$dt))
  expect_false(v_time_name %in% names(mm_long$dt))
  n_dup <- nrow(mm_long$dt[duplicated(mm_long$dt[, .(site, TIMESTAMP, var_name)]), ])
  expect_equal(n_dup, 0L)
})

# ---- Multi-site combined data --------------------------------------------

test_that("multi-site combined fixture has valid long-format structure", {
  mm <- readRDS(pkg_extdata("mm_amo_ebu_whm_2023.rds"))
  attr(mm, "format") <- "long"

  expect_true(all(.met_long_cols %in% names(mm$dt)))
  expect_true(all(c("UK-AMO", "UK-EBU", "UK-WHM") %in% unique(mm$dt$site)))
  expect_equal(sum(is.na(mm$dt$TIMESTAMP)), 0L)
  expect_null(mm$dt_qc)
  expect_null(mm$dt_ref)
})

test_that("multi-site combined fixture has no duplicate keys", {
  mm <- readRDS(pkg_extdata("mm_amo_ebu_whm_2023.rds"))
  dt_known <- mm$dt[!is.na(site)]
  n_dup <- nrow(dt_known[duplicated(dt_known[, .(site, TIMESTAMP, var_name)]), ])
  expect_equal(n_dup, 0L)
})
