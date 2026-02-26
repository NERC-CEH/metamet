library(here)
library(sloop)
library(devtools)
library(usethis)
library(pkgload)
library(pkgbuild)
# usethis::use_package_doc()

# add to .RBuildIgnore
usethis::use_build_ignore(c("data-raw"))
# add air to GitHub Actions
# usethis::use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml")

# # add the packages requiring importing to package.r
use_tidy_eval()
use_data_table()
use_package("data.table", "Depends")
use_package("ggplot2", "Depends")
# seems to produce an error with the coalesce_yx argument if we only Import
use_package("powerjoin", "Imports")
use_package("fs", "Imports", min_version = "1.5.0")
use_package("readxl", "Imports")
use_package("lubridate", "Imports")
use_package("shiny", "Imports")
use_package("openair", "Imports")
use_package("openairmaps", "Imports")


document()
pkgload::load_all()
devtools::test()
build(vignettes = FALSE)
check(vignettes = FALSE)
install(".")
pkgload::load_all()
detach("package:metamet", unload = TRUE)
build_site()

run_shiny()

s3_methods_generic("metamet")
s3_methods_class("metamet")

# test the constructor
mm1 <- new_metamet(dt = data.table(x = 1:1000, y = 1:1000), site_id = "UK-EBU")
mm1
str(mm1)
class(mm1)

# test the helper
# test with file paths as argument
fname_dt <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L03_F02.dat"
fname_meta <- "data-raw/dt_meta.xlsx"
fname_site <- "data-raw/dt_site.csv"
mm_from_files <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = "UK-AMO"
)
s3_dispatch(metamet(fname_dt))

# test with data frames as argument
df <- read.csv(fname_dt)
# df_meta <- read.csv(fname_meta)
df_site <- read.csv(fname_site)
mm_from_df <- metamet(df, dt_meta = fname_meta, dt_site = df_site)
class(mm)
str(mm)

# test with data tables as arguments
dt <- fread(fname_dt)
dt_meta <- fread(fname_meta)
dt_site <- fread(fname_site)
mm_from_dt <- metamet(dt, dt_meta = dt_meta, dt_site = dt_site)
class(mm_from_dt)
str(mm_from_dt)
# check all identical - they are not: df differ in class of TIMESTAMP
identical(mm_from_files$dt, mm_from_df$dt)
identical(mm_from_files$dt, mm_from_dt$dt)
identical(mm_from_df$dt, mm_from_dt$dt)
identical(
  summary(mm_from_df$dt$D_SNOW_4_1_1),
  summary(mm_from_dt$dt$D_SNOW_4_1_1)
)

class(mm_from_df$dt$TIMESTAMP)
class(mm_from_dt$dt$TIMESTAMP)

# check all identical - they are
identical(mm_from_files$dt_meta, mm_from_df$dt_meta)
identical(mm_from_files$dt_meta, mm_from_dt$dt_meta)
identical(mm_from_df$dt_meta, mm_from_dt$dt_meta)

# test with a mix of arguments
mm <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = dt_site,
  site_id = "UK-AMO"
)
class(mm)
str(mm)
summary(mm$dt)
dim(mm$dt)

# recreate from files only
fname_dt <- "data-raw/UK-AMO/UK-AMo_BM_20260203_L03_F02.dat"
fname_meta <- "data-raw/dt_meta.xlsx"
fname_site <- "data-raw/dt_site.csv"

mm <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = "UK-AMO"
)
dim(mm$dt)
summary(mm$dt)
sum(mm$dt$P_12_1_1)
mm_avg <- time_average(mm, avg.time = "6 hour")
sum(mm_avg$dt$P_12_1_1)
dim(mm_avg$dt)
print(mm_avg$dt)
class(mm_avg)
str(mm_avg)
summary(mm_avg$dt)

#  make formal tests for these:
print(mm)
summary(mm)
mm <- restrict(mm)
mm <- convert_time_char_to_posix(mm)

# test existing Level 1 data
fname_dt <- here("data-raw/UK-AMO/UK-AMO_BM_dt_2026.csv")
fname_dt <- here("data-raw/UK-AMO/UK-AMo_BM_20260203_L03_F02.dat")
fname_meta <- here("data-raw/dt_meta.xlsx")
fname_site <- here("data-raw/dt_site.csv")
rm(mm)
dt <- fname_dt
dt_meta <- fname_meta
dt_site <- fname_site
mm <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = "UK-AMO"
)

as.POSIXct(strptime("2025-01-02 00:00:00 UTC", "%Y-%m-%d %H:%M:%S"))
mm$dt[1:5, get(time_name)]
mm$dt[46:50, strptime(get(time_name), time_format)]

dt <- fread(fname_dt)
dt[, which(is.na(DATECT))]
summary(dt$DATECT)
summary(mm$dt$DATECT)
mm$dt[, which(is.na(DATECT))]
mm$dt[46:50, 1:5]
dt[46:50, 1:5]
mm <- copy(mm_in) # we need this to avoid modifying the original object

time_average(mm, avg.time = "3 hour", report_end_interval = TRUE)

l_mm <- list(mm1, mm2)

install.packages("openairmaps")
library(openairmaps)

mm_amo <- readRDS("data-raw/UK-AMO/mm.rds")
mm_amo <- subset_by_date(
  mm_amo,
  start_date = "2022-01-01",
  end_date = "2023-01-01"
)
mm_amo <- time_average(mm_amo, avg.time = "1 hour")
mm_amo
mm
# rename with ICOS names
setnames(
  mm$dt,
  mm$dt_meta[site == "UK-WHM", name_dt],
  mm$dt_meta[site == "UK-WHM", name_icos]
)
setnames(
  mm$dt_qc,
  mm$dt_meta[site == "UK-WHM", name_dt],
  mm$dt_meta[site == "UK-WHM", name_icos]
)
setnames(
  mm$dt_ref,
  mm$dt_meta[site == "UK-WHM", name_dt],
  mm$dt_meta[site == "UK-WHM", name_icos]
)
mm$dt[, which(duplicated(names(mm$dt))) := NULL]
mm$dt_qc[, which(duplicated(names(mm$dt_qc))) := NULL]
mm$dt_ref[, which(duplicated(names(mm$dt_ref))) := NULL]

names(mm$dt)
names(mm$dt_qc)
names(mm$dt_ref)

names(mm_amo$dt)
names(mm_amo$dt_qc)
names(mm_amo$dt_ref)

setnames(
  mm_amo$dt,
  mm_amo$dt_meta[site == "UK-AMO", name_dt],
  mm_amo$dt_meta[site == "UK-AMO", name_icos]
)
setnames(
  mm_amo$dt_qc,
  mm_amo$dt_meta[site == "UK-AMO", name_dt],
  mm_amo$dt_meta[site == "UK-AMO", name_icos]
)
setnames(
  mm_amo$dt_ref,
  mm_amo$dt_meta[site == "UK-AMO", name_dt],
  mm_amo$dt_meta[site == "UK-AMO", name_icos]
)
mm_amo$dt[, which(duplicated(names(mm_amo$dt))) := NULL]
mm_amo$dt_qc[, which(duplicated(names(mm_amo$dt_qc))) := NULL]
mm_amo$dt_ref[, which(duplicated(names(mm_amo$dt_ref))) := NULL]

# does not work - this uses names_dt, not standard_name
mmj <- join(mm_amo, mm)
names(mm)
names(mm_amo)
