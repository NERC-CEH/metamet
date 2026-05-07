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
