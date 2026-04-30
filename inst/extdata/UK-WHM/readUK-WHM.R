library(metamet)
fname <- "data-raw/UK-WHM/whim_met_2002_2023.csv"
dt <- fread(fname)

new_names <- c(
  "Timestamp",
  "Rain",
  "LWS",
  "AirT",
  "RH",
  "PAR",
  "Total_solar",
  "Net_rad",
  "WS",
  "WD",
  "Soil_VWC",
  "Soil_T1",
  "Soil_T2",
  "WTD"
)
setnames(dt, names(dt), new_names)
fwrite(dt, fname)

v_units <- c(
  NA,
  "mm",
  "mV",
  "degree_C",
  "percent",
  "micromol / m^2 / s",
  "W / m^2",
  "W / m^2",
  "m / s",
  "degree",
  "percent",
  "degree_C",
  "degree_C",
  "cm"
)

dt_meta <- data.table(
  names_dt = names(dt),
  units = v_units
)
fwrite(dt_meta, "data-raw/UK-WHM/dt_meta.csv")

fname <- "data-raw/dt_meta.xlsx"
dt_meta <- setDT(readxl::read_excel(fname))
dt_meta[site == "UK-WHM"]
names(dt_meta)
v_col <- c(
  "site",
  "name_dt",
  "standard_name",
  "long_name",
  "units",
  "type",
  "time_char_format",
  # "start_date",
  # "end_date",
  # "horizontal_id",
  # "vertical_id",
  # "replicate_id",
  # "gf_time",
  # "gf_era5",
  # "gf_night_zero",
  # "diff_ref_max",
  # "sensor_make",
  # "sensor_model",
  "range_min",
  "range_max",
  "standard_name_era5",
  "era5_units",
  "imputation_method"
)
dt_meta[site == "UK-WHM", ..v_col]

dt_site <- data.table(
  site = "UK-WHM",
  long_name = "Whim Moss",
  lon = -3.27155,
  lat = -55.76566,
  elev = 316
)

fname_dt <- here("data-raw/UK-WHM/whim_met_2002_2023.csv")
fname_meta <- here("data-raw/dt_meta.xlsx")
fname_site <- here("data-raw/dt_site.csv")
fname_era5 <- here("data-raw/dt_era5.csv")

dt <- fread(fname_dt)
dt_site <- fread(fname_site)
dt_meta <- setDT(readxl::read_excel(fname_meta))

mm <- metamet(
  dt = dt,
  dt_meta = dt_meta,
  dt_site = dt_site,
  site_id = "UK-WHM"
)
dim(mm$dt)
