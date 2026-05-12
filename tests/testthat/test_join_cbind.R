# test the cbind case of metamet::join
# adding columns as a full join; we might want to add an option to restrict to left join

test_that("joining metamet objects works", {
  fname_dt1 <- pkg_extdata(
    "UK-AMO/UK-AMO_BM_20260126_L04_F01.dat"
  )
  fname_dt2 <- pkg_extdata(
    "UK-AMO/UK-AMO_BM_20260126_L04_F02.dat"
  )

  mm1 <- metamet(
    dt = fname_dt1,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  )
  mm2 <- metamet(
    dt = fname_dt2,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  )

  mm1 <- time_average(mm1, avg.time = "5 min")
  mm2 <- time_average(mm2, avg.time = "5 min")
  dim(mm1$dt)
  dim(mm2$dt)

  mm_joined <- join(mm1, mm2)
  dim(mm_joined$dt)
  names(mm_joined$dt)

  expect_equal(sum(ncol(mm1$dt), ncol(mm2$dt)) - 2, ncol(mm_joined$dt))
  expect_equal(nrow(mm1$dt), nrow(mm_joined$dt))
  expect_equal(nrow(mm2$dt), nrow(mm_joined$dt))
})

test_that("join works on long-format objects (submitchanges scenario)", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))

  # simulate subset_by_date + impute as the app does
  mm_qry <- subset_by_date(
    mm_long,
    start_date = "2023-06-01 00:30:00",
    end_date = "2023-06-02 00:00:00"
  )

  mm_joined <- join(mm_long, mm_qry)

  expect_s3_class(mm_joined, "metamet")
  expect_equal(attr(mm_joined, "format"), "long")
  # full dataset row count is preserved (mm_qry is a strict subset)
  expect_equal(nrow(mm_joined$dt), nrow(mm_long$dt))
  # no duplicate keys introduced
  n_dup <- nrow(mm_joined$dt[
    duplicated(mm_joined$dt[, .(site, TIMESTAMP, var_name)]),
  ])
  expect_equal(n_dup, 0L)
})

test_that("join then reshape to wide does not crash (submitchanges full path)", {
  mm <- readRDS(pkg_extdata("UK-AMO/UK-AMO_BM_mm_2023.rds"))
  mm_long <- suppressWarnings(metamet_reshape(mm, "long"))

  mm_qry <- subset_by_date(
    mm_long,
    start_date = "2023-06-01 00:30:00",
    end_date = "2023-06-02 00:00:00"
  )
  mm_joined <- join(mm_long, mm_qry)

  # this is what submitchanges does before format_for_ceda — previously
  # crashed with "index 'type' exists but is invalid" from stale secondary
  # index left by power_full_join on dt_meta
  expect_no_error(mm_wide <- metamet_reshape(mm_joined, "wide"))
  expect_equal(attr(mm_wide, "format"), "wide")
  expect_true("TIMESTAMP" %in% names(mm_wide$dt))

  # format_for_ceda must not warn about recycled rows — dt_qc has fewer rows
  # than dt (only timestamps with non-NA QC codes), so cbind by position is wrong
  expect_no_warning(df_ceda <- metamet:::format_for_ceda(mm_wide))
  expect_s3_class(df_ceda, "data.table")
  # every row in the CEDA output corresponds to a row in mm_wide$dt
  expect_equal(nrow(df_ceda), nrow(mm_wide$dt))
})
