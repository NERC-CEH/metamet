# This script is run to process raw data to create data objects
# stored in \data, which are built into the package.
# The contents of \data-raw are ignored when building.

# Define lookup table for methods, qc (codes) and method long names.

df_method <- data.frame(
  method = c(
    'raw',
    "missing",
    "time",
    "regn",
    "nightzero",
    "noneg",
    "zero",
    "era5"
  ),
  method_longname = c(
    'Original observation (raw data)',
    "Missing",
    "Time",
    "Regression with covariate",
    "Night-time zero",
    "No negative values",
    "All zeros",
    "Regression with ERA5 covariate"
  ),
  qc = c(0, 1, 2, 3, 4, 5, 6, 7) # 0 = raw, 1 = missing, >1 = missing but imputed
)

df_method$method_longname <- factor(
  df_method$method_longname,
  levels = c(
    'Original observation (raw data)',
    "Missing",
    "Time",
    "Regression with covariate",
    "Night-time zero",
    "No negative values",
    "All zeros",
    "Regression with ERA5 covariate"
  )
)

usethis::use_data(df_method, overwrite = TRUE)


# make some metamet objects from files
site_id <- "UK-AMO"
fname_dt1 <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L03_F02.dat"
fname_dt2 <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L04_F01.dat"
fname_dt3 <- "data-raw/UK-AMO/UK-AMo_BM_20260203_L03_F02.dat"
fname_meta <- "data-raw/dt_meta.xlsx"
fname_site <- "data-raw/dt_site.csv"

mm1 <- metamet(
  dt = fname_dt1,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = site_id
)
mm2 <- metamet(
  dt = fname_dt2,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = site_id
)
mm3 <- metamet(
  dt = fname_dt3,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = site_id
)

usethis::use_data(mm1, mm2, mm3, overwrite = TRUE)

# and copy the meta and site files to the test data
fs::file_copy(
  fname_meta,
  fs::path(testthat::test_path(), fname_meta),
  overwrite = TRUE
)
fs::file_copy(
  fname_site,
  fs::path(testthat::test_path(), fname_site),
  overwrite = TRUE
)
