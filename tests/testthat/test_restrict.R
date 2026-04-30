# test that the restrict function works correctlt, so that:
# 1. dt_meta only has variables that exist in dt and v.v.
# 2. dt_site only has sites that exist in dt and v.v.

test_that("restrict function works", {
  fname_dt <- pkg_extdata("UK-AMO/UK-AMO_BM_dt_2026.csv")

  mm <- metamet(
    dt = fname_dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    site_id = "UK-AMO"
  )

  # check dt only has variables that exist in metadata
  v_name_meta <- mm$dt_meta$name_dt
  v_name_dt <- colnames(mm$dt)

  # check dt only has sites that exist in  site data
  v_site_dt <- unique(mm$dt[, site])
  v_site_site <- mm$dt_site$site

  expect_identical(sort(v_name_meta), sort(v_name_dt))
  expect_identical(sort(v_site_site), sort(v_site_dt))
})
