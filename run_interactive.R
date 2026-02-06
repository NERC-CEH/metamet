library(sloop)
library(devtools)
library(usethis)
# usethis::use_package_doc()

# add to .RBuildIgnore
usethis::use_build_ignore(c("data-raw"))

# # add the packages requiring importing to package.r
use_tidy_eval()
use_data_table()
use_package("data.table", "Depends")
use_package("fs", "Imports", min_version = "1.5.0")
use_package("here", "Imports")
use_package("openair", "Imports")
use_package("lubridate", "Imports")


document()
build(vignettes = FALSE)
check(vignettes = FALSE)
pkgload::load_all()

s3_methods_generic("metamet")
s3_methods_class("metamet")

# test the constructor
mm1 <- new_metamet(dt = data.table(x = 1:1000, y = 1:1000))
mm1
str(mm1)
class(mm1)

# test the helper
# test with file paths as argument
fname_dt <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L03_F02.dat"
fname_meta <- "data-raw/UK-AMO/dt_meta.csv"
fname_site <- "data-raw/UK-AMO/dt_site.csv"
mm_from_files <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = fname_site
)
s3_dispatch(metamet(fname_dt))

# test with data frames as argument
df <- read.csv(fname_dt)
df_meta <- read.csv(fname_meta)
df_site <- read.csv(fname_site)
mm_from_df <- metamet(df, dt_meta = df_meta, dt_site = df_site)
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
mm <- metamet(dt = fname_dt, dt_meta = fname_meta, dt_site = dt_site)
class(mm)
str(mm)
summary(mm$dt)
dim(mm$dt)

# recreate from files only
fname_dt <- "data-raw/UK-AMO/UK-AMo_BM_20250822_L03_F02.dat"
fname_meta <- "data-raw/UK-AMO/dt_meta.csv"
fname_site <- "data-raw/UK-AMO/dt_site.csv"
mm <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = fname_site
)
mm <- time_average(mm, avg.time = "2 hour")
dim(mm$dt)
print(mm$dt)
class(mm)
str(mm)
summary(mm$dt)

# tested
mm <- restrict(mm)
mm <- convert_time_char_to_posix(mm)
