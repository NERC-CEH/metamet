# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("reading metamet from files with QC works", {
  fname_dt1 <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2025.csv")
  fname_qc1 <- pkg_extdata("UK-AMO/UK-AMO_BM_qc_2025.csv")
  fname_dt2 <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv")
  fname_qc2 <- pkg_extdata("UK-AMO/UK-AMO_BM_qc_2026.csv")
  # half-hourly data
  mm_hh_1 <- metamet(
    dt = fname_dt1,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc = fname_qc1,
    site_id = "UK-AMO"
  )
  mm_hh_2 <- metamet(
    dt = fname_dt2,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc = fname_qc2,
    site_id = "UK-AMO"
  )

  mm1 <- time_average(mm_hh_1, avg.time = "1 day", extra_rows = 1)
  mm2 <- time_average(mm_hh_2, avg.time = "1 day", extra_rows = 1)

  mm_joined <- join(mm1, mm2)

  # dim(mm1$dt)
  # dim(mm2$dt)
  # mm_hh_1$dt[1:5, 1:5]
  # mm_hh_2$dt[1:5, 1:5]
  # mm1$dt[1:5, 1:5]
  # mm2$dt[1:5, 1:5]
  # dim(mm_joined$dt)

  time_name <- mm_joined$dt_meta[type == "time", name_dt]

  expect_s3_class(mm_joined, "metamet")
  expect_s3_class(mm_joined$dt_qc, "data.table")
  # dt_qc is always long: columns are site, TIMESTAMP, var_name, qc, validator, comment
  expect_true("var_name" %in% names(mm_joined$dt_qc))
  expect_true("qc" %in% names(mm_joined$dt_qc))
  expect_true("comment" %in% names(mm_joined$dt_qc))
  expect_equal(sum(is.na(mm_joined$dt_qc$TIMESTAMP)), 0)
  # should not be any duplicate times
  expect_equal(nrow(mm_joined$dt[duplicated(mm_joined$dt[, ..time_name]), ]), 0)
  # do we see the same precip qc codes in the original and joined data?
  expect_setequal(
    unique(c(
      mm_hh_1$dt_qc[var_name == "P_12_1_1", qc],
      mm_hh_2$dt_qc[var_name == "P_12_1_1", qc]
    )),
    unique(mm_joined$dt_qc[var_name == "P_12_1_1", qc])
  )
})
