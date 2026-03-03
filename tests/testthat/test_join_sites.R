# test the helper constructor function with file paths as argument
# also testing handling time variables with different names in different files

test_that("joining metamet from different sites works", {
  mm_amo <- readRDS(
    file = testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_mm_2023.rds")
  )
  mm_ebu <- readRDS(
    file = testthat::test_path("data-raw/UK-EBU/UK-EBU_BM_mm_2023.rds")
  )
  mm_whm <- readRDS(
    file = testthat::test_path("data-raw/UK-WHM/UK-WHM_BM_mm_2023.rds")
  )

  mm_amo <- subset_by_date(mm_amo, "2023-09-01", "2023-09-02")
  mm_ebu <- subset_by_date(mm_ebu, "2023-09-01", "2023-09-02")
  mm_whm <- subset_by_date(mm_whm, "2023-09-01", "2023-09-02")

  mm_whm <- change_naming_convention(mm_whm, name_convention = "name_icos")

  mm_amo <- reshape_wide_to_long(mm_amo)
  mm_ebu <- reshape_wide_to_long(mm_ebu)
  mm_whm <- reshape_wide_to_long(mm_whm)

  dim(mm_amo$dt)
  dim(mm_ebu$dt)
  dim(mm_whm$dt)

  mm <- rbind_metamet(
    mm_amo,
    l_dt = list(mm_amo$dt, mm_ebu$dt, mm_whm$dt),
    l_dt_meta = list(mm_amo$dt_meta, mm_ebu$dt_meta, mm_whm$dt_meta),
    l_dt_site = list(mm_amo$dt_site, mm_ebu$dt_site, mm_whm$dt_site)
  )
  dim(mm$dt)
  names(mm$dt)

  p <- ggplot(
    mm$dt[name_icos == "TS", ],
    aes(TIMESTAMP, value, colour = var_name)
  )
  p <- p + geom_point()
  p <- p + geom_line(aes(y = ref), colour = "black")
  p <- p + facet_wrap(~site)

  p <- ggplot(
    mm$dt[name_icos == "PPFD_IN", ],
    aes(TIMESTAMP, value, colour = var_name)
  )
  p <- p + geom_line(aes(y = ref), colour = "black")
  p <- p + geom_line()
  p <- p + facet_wrap(~site)

  p <- ggplot(
    mm$dt[name_icos == "TA", ],
    aes(ref, value, colour = var_name)
  )
  p <- p + geom_abline()
  p <- p + geom_point()
  p <- p + facet_wrap(~site)

  expect_s3_class(mm, "metamet")
  expect_s3_class(mm$dt, "data.table")
  expect_equal(nrow(mm$dt), nrow(mm_amo$dt) + nrow(mm_ebu$dt) + nrow(mm_whm$dt))
  expect_equal(nrow(mm$dt_site), 3)
})
