library(pins)
library(powerjoin)
board <- board_connect()

# Reading in Level 1 data from pin on Connect server
l_lev1 <- pin_read(board, "plevy/level1_data")

# Read in the previously validated data from pin on Connect server
l_lev2 <- pin_read(board, "plevy/level2_data")

# Here we join the existing Level 2 data with new Level 1 data.
# Where records already exist in the Level 2 data, these are preserved
# and only new Level 1 data is added to the resulting data frame.

dim(l_lev1$dt_era5)
dim(l_lev2$dt_era5)

l_lev2$dt <- power_full_join(
  l_lev2$dt,
  l_lev1$dt,
  by = "DATECT",
  conflict = coalesce_xy
)
dim(l_lev2$dt)

l_lev2$dt_qc <- power_full_join(
  l_lev2$dt_qc,
  l_lev1$dt_qc,
  by = "DATECT",
  conflict = coalesce_xy
)
l_lev2$dt_era5 <- power_full_join(
  l_lev2$dt_era5,
  l_lev1$dt_era5,
  by = "DATECT",
  conflict = coalesce_xy
)

fname_dt <- here("data-raw/UK-AMO/le1v/UK-AMO_BM_dt_2025.csv")
fname_qc <- here("data-raw/UK-AMO/lev1/UK-AMO_BM_qc_2025.csv")
fname_meta <- here("data-raw/dt_meta.xlsx")
fname_site <- here("data-raw/dt_site.csv")
fname_era5 <- here("data-raw/UK-AMO/dt_era5.csv")

dt_meta <- readxl::read_excel(fname_meta)
dt_site <- fread(fname_site)

# UK-AMO half-hourly level 1 data
mm1 <- metamet(
  dt = l_lev1$dt,
  dt_qc = l_lev1$dt_qc,
  dt_meta = dt_meta,
  dt_site = dt_site,
  site_id = "UK-AMO"
)

# UK-AMO half-hourly level 1 data
mm2 <- metamet(
  dt = l_lev2$dt,
  dt_qc = l_lev2$dt_qc,
  dt_meta = dt_meta,
  dt_site = dt_site,
  site_id = "UK-AMO"
)
dim(mm1$dt)
dim(mm2$dt)

mm1 <- add_era5(
  mm1,
  fname_era5 = fname_era5,
  restrict_ref_to_obs = TRUE,
  restrict_obs_to_ref = TRUE
)
mm2 <- add_era5(
  mm2,
  fname_era5 = fname_era5,
  restrict_ref_to_obs = TRUE,
  restrict_obs_to_ref = TRUE
)

dim(mm1$dt)
dim(mm2$dt)

mm2 <- join(mm1, mm2)

dim(mm1$dt)
dim(mm2$dt)

mm1
mm2

saveRDS(mm1, file = here("data-raw/UK-AMO", "mm1.rds"))
saveRDS(mm2, file = here("data-raw/UK-AMO", "mm2.rds"))

fs::file_copy(
  here("data-raw/UK-AMO", "mm1.rds"),
  here("../metqc/data-raw/UK-AMO", "mm1.rds"),
  overwrite = TRUE
)
fs::file_copy(
  here("data-raw/UK-AMO", "mm2.rds"),
  here("../metqc/data-raw/UK-AMO", "mm2.rds"),
  overwrite = TRUE
)

system.time(mm1 <- readRDS(file = here("data-raw/UK-AMO", "mm1.rds")))
system.time(mm2 <- readRDS(file = here("data-raw/UK-AMO", "mm2.rds")))
system.time(mmr <- readRDS(file = here("data-raw/UK-AMO", "mm.rds")))
