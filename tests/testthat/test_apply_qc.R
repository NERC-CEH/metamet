test_that("apply_qc returns long-format metamet", {
  mm <- suppressWarnings(apply_qc(mm1))
  expect_equal(attr(mm, "format"), "long")
  expect_s3_class(mm, "metamet")
})

test_that("apply_qc accepts wide input and converts to long", {
  expect_false(identical(attr(mm1, "format"), "long"))
  mm <- suppressWarnings(apply_qc(mm1))
  expect_equal(attr(mm, "format"), "long")
})

test_that("apply_qc qc column contains only 0 and 1", {
  mm <- apply_qc(mm2)
  expect_true(all(mm$dt$qc %in% c(0, 1)))
})

test_that("apply_qc sets qc = 1 exactly where value is NA", {
  mm <- suppressWarnings(apply_qc(mm1))
  expect_true(all(mm$dt[is.na(value), qc] == 1))
  expect_true(all(mm$dt[!is.na(value), qc] == 0))
})

test_that("apply_qc sets out-of-range values to NA (more NAs than before)", {
  mm_long <- suppressWarnings(metamet_reshape(mm1, "long"))
  n_na_before <- sum(is.na(mm_long$dt$value))
  mm_after <- suppressWarnings(apply_qc(mm1))
  expect_gte(sum(is.na(mm_after$dt$value)), n_na_before)
})

test_that("apply_qc sets validator = 'auto' for flagged rows only", {
  mm <- suppressWarnings(apply_qc(mm1))
  expect_true(all(mm$dt[qc == 1L, validator] == "auto"))
  expect_true(all(is.na(mm$dt[qc == 0L, validator])))
})

test_that("apply_qc is idempotent", {
  mm_once <- apply_qc(mm2)
  mm_twice <- apply_qc(mm_once)
  expect_identical(mm_once$dt$value, mm_twice$dt$value)
  expect_identical(mm_once$dt$qc, mm_twice$dt$qc)
})

test_that("apply_qc produces no duplicate (TIMESTAMP, var_name) keys", {
  mm <- apply_qc(mm2)
  n_dup <- nrow(mm$dt[duplicated(mm$dt[, .(TIMESTAMP, var_name)]), ])
  expect_equal(n_dup, 0L)
})

test_that("apply_qc works on object that already has ERA5 ref data", {
  mm0 <- metamet(
    dt = pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv"),
    dt_meta = pkg_extdata("dt_meta.csv"),
    dt_site = pkg_extdata("dt_site.csv"),
    dt_qc = pkg_extdata("UK-AMO/UK-AMO_BM_qc_2026.csv"),
    site_id = "UK-AMO"
  )
  mm0 <- add_era5(
    mm0,
    fname_era5 = pkg_extdata("dt_era5.csv"),
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = FALSE,
    report_end_interval = TRUE,
    extra_rows = 3
  )
  mm <- apply_qc(mm0)
  expect_equal(attr(mm, "format"), "long")
  expect_true(all(mm$dt$qc %in% c(0, 1)))
  # ERA5 ref data should be present in the ref column
  expect_true(any(!is.na(mm$dt$ref)))
})
