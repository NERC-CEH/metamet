# test the rbind case of metamet::join
# in the case of two consectutive time periods, we are just adding rows

test_that("joining metamet objects works", {
  fname_dt1 <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2025.csv")
  fname_dt2 <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv")

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

  mm_joined <- join(mm1, mm2)

  # in the case of two consectutive time periods, we are just adding rows
  expect_equal(sum(nrow(mm1$dt), nrow(mm2$dt)), nrow(mm_joined$dt))
  expect_equal(ncol(mm1$dt), ncol(mm_joined$dt))
  expect_equal(ncol(mm2$dt), ncol(mm_joined$dt))
})
