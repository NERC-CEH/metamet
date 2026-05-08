# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("reading metamet from files from Whim works", {
  fname_dt <- pkg_extdata("UK-WHM/whim_met_2002_2023.csv")
  fname_era5 <- pkg_extdata("UK-WHM/dt_era5.csv")
  dt <- fread(fname_dt)

  mm <- metamet(
    dt = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-WHM"
  )
  dim(mm$dt)

  mm <- subset_by_date(
    mm,
    start_date = "2023-01-01 00:30:00",
    end_date = "2024-01-01 00:00:00"
  )

  mm <- time_average(mm, avg.time = "30 min", extra_rows = 3)

  dim(mm$dt)

  mm <- add_era5(
    mm,
    fname_era5 = fname_era5,
    restrict_ref_to_obs = TRUE,
    restrict_obs_to_ref = TRUE,
    report_end_interval = TRUE,
    extra_rows = 3
  )

  dim(mm$dt)
  dim(mm$dt_ref)

  sum(is.na(mm$dt))
  mm <- apply_qc(mm)
  mm <- metamet_reshape(mm, "long")
  sum(is.na(mm$dt))

  summary(mm$dt)
  mm$dt[, sapply(.SD, function(x) sum(is.na(x))), .SDcols = names(mm$dt)]

  mm <- impute(
    v_y = c("WTD"),
    mm = mm,
    method = "time",
    fit = TRUE,
    plot_graph = TRUE
  )
  # saveRDS(mm, file = pkg_extdata("UK-WHM/UK-WHM_BM_mm_2023.rds"))

  time_name <- mm$dt_meta[type == "time", name_dt]

  expect_s3_class(mm, "metamet")
  expect_s3_class(mm$dt, "data.table")
  # in long format qc and ref are columns in mm$dt; dt_qc and dt_ref are NULL
  expect_true("qc" %in% names(mm$dt))
  expect_true("ref" %in% names(mm$dt))
  expect_equal(sum(is.na(mm$dt$TIMESTAMP)), 0)
  # should not be any duplicate keys
  expect_equal(
    nrow(mm$dt[duplicated(mm$dt[, .(site, TIMESTAMP, var_name)]), ]),
    0
  )
})
