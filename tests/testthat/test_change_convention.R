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
