fname_dt1 <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L03_F02.dat"
fname_dt2 <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L04_F01.dat"
fname_dt3 <- "data-raw/UK-AMO/UK-AMo_BM_20260203_L03_F02.dat"
fname_meta <- "data-raw/UK-AMO/dt_meta.xlsx"
fname_site <- "data-raw/UK-AMO/dt_site.csv"

mm1 <- metamet(
  dt = fname_dt1,
  dt_meta = fname_meta,
  dt_site = fname_site
)
mm2 <- metamet(
  dt = fname_dt2,
  dt_meta = fname_meta,
  dt_site = fname_site
)
mm3 <- metamet(
  dt = fname_dt3,
  dt_meta = fname_meta,
  dt_site = fname_site
)

usethis::use_data(mm1, mm2, mm3, overwrite = TRUE)
