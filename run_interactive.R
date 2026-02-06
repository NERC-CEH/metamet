library(sloop)
library(devtools)
# usethis::use_package_doc()

document()
build(vignettes = FALSE)
check(vignettes = FALSE)
pkgload::load_all()

# test the constructor
mm1 <- new_metamet(dt = data.table(x = 1:1000, y = 1:1000))
mm1
str(mm1)
class(mm1)


# test the helper
# test with file paths as argument
fname_dt <- "data-raw/UK-AMO/UK-AMo_BM_20260126_L03_F02.dat"
fname_meta <- "data-raw/UK-AMO/dt_meta.csv"
fname_site <- "data-raw/UK-AMO/dt_site.csv"
mm <- metamet(fname_dt, dt_meta = fname_meta, dt_site = fname_site)

# test with data frames as argument
df <- read.csv(fname_dt)
df_meta <- read.csv(fname_meta)
df_site <- read.csv(fname_site)
mm <- metamet(df, dt_meta = df_meta, dt_site = df_site)
class(mm)
str(mm)

# test with data tables as arguments
dt <- fread(fname_dt)
dt_meta <- fread(fname_meta)
dt_site <- fread(fname_site)
mm <- metamet(dt, dt_meta = dt_meta, dt_site = dt_site)
class(mm)
str(mm)

# test with a mix of arguments
mm <- metamet(fname_dt, dt_meta = df_meta, dt_site = dt_site)
class(mm)
str(mm)


s3_dispatch(metamet(x))
s3_methods_generic("metamet")
s3_methods_class("metamet")

# Example Usage:
# df <- data.table(a = 1:5)
# metamet(df)       # Calls metamet.data.frame
# fwrite(df, "dummy_data.csv")
# metamet("dummy_data.csv") # Calls metamet.character
