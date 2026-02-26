test_that("subsetting by date works", {
  # mm without qc or ref
  dim(mm1$dt)
  dim(mm1$dt_qc)
  dim(mm1$dt_ref)

  names(mm1$dt)
  mm1_icos <- change_naming_convention(mm1, name_convention = "name_icos")
  names(mm1_icos$dt)
  mm1_era5 <- change_naming_convention(mm1, name_convention = "name_era5")
  names(mm1_era5$dt)

  expect_s3_class(mm1, "metamet")
  expect_s3_class(mm1$dt, "data.table")
  # check that the time variables are correctly named
  expect_s3_class(mm1_icos$dt[, TIMESTAMP], "POSIXct")
  expect_s3_class(mm1_era5$dt[, time], "POSIXct")
})
