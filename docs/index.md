# metamet

# metamet

`metamet` is an R package which attempts to solve many of the problems
encountered in working with meteorological observation data. It provide
a system for:

- standardising metadata
- converting between file formats
- converting between variable naming conventions
- converting units
- automating QA/QC
- facilitating manual QA/QC via a shiny app
- imputing missing values or “gap-filling”

It does this by defining:

1.  a standardised generic data structure with enough complexity to hold
    both the observational data and the metadata, including
    site-specific, variable-specific and individual record-specific
    metadata; and
2.  methods/functions for converting data between formats, combining
    data from different sources, quality control and gap-filling.

## Installation

You can install the development version of metamet from
[GitHub](https://github.com/NERC-CEH/metamet) with:

``` r
install.packages("pak")
pak::pak("NERC-CEH/metamet")
```

## Example

Basic usage is to first create `metamet` objects from files or
pre-existing data frames or data tables. Because all the metadata
describing the observations is available in the structure, objects can
be processed relatively easily so as to:

- rename variables in standard naming conventions
- combine data from different sites
- apply quality control
- impute missing data.

``` r
library(metamet)
#> Loading required package: data.table
#> data.table 1.17.8 using 7 threads (see ?getDTthreads).  Latest news: r-datatable.com
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.5.2
fname_dt <- testthat::test_path("data-raw/UK-AMO/UK-AMO_BM_dt_2026.csv")
fname_meta <- testthat::test_path("data-raw/dt_meta.xlsx")
fname_site <- testthat::test_path("data-raw/dt_site.csv")

mm <- metamet(
  dt = fname_dt,
  dt_meta = fname_meta,
  dt_site = fname_site,
  site_id = "UK-AMO"
)
#> Loading file: tests/testthat/data-raw/UK-AMO/UK-AMO_BM_dt_2026.csv
#> Reading data table ...

# print the outline strucutre:
mm
#> metamet object
#> -------------
#> Data table (dt) - first 6 cols:
#>                    DATECT       TS      SWC          G         WTD WTD_4_1_1
#>                    <POSc>    <num>    <num>      <num>       <num>     <num>
#>    1: 2026-01-01 00:30:00 4.704447 66.78342 -5.2438167  0.03166667        NA
#>    2: 2026-01-01 01:00:00 4.710880 66.78225 -5.1350167  0.03091667        NA
#>    3: 2026-01-01 01:30:00 4.716867 66.78408 -5.0469667  0.02978333        NA
#>    4: 2026-01-01 02:00:00 4.722013 66.78121 -4.9933833  0.02945000        NA
#>    5: 2026-01-01 02:30:00 4.726623 66.78179 -4.9702167  0.02666667        NA
#>   ---                                                                       
#> 1484: 2026-01-31 22:00:00 4.702524 53.85774 -0.9039389 -0.18318600  -0.60073
#> 1485: 2026-01-31 22:30:00 4.743937 53.84866 -0.9908333 -0.18335267  -0.60073
#> 1486: 2026-01-31 23:00:00 4.773235 53.84151 -1.0347333 -0.18363933  -0.60073
#> 1487: 2026-01-31 23:30:00 4.783013 53.83243 -1.0497611 -0.18391933  -0.60073
#> 1488: 2026-02-01 00:00:00 4.785532 53.82335 -1.0490556 -0.18402600  -0.60073
#> 
#> Metadata (dt_meta) - first 6 cols:
#>      site start_date   end_date name_dt name_local long_name_local
#>    <char>     <POSc>     <POSc>  <char>     <char>          <char>
#> 1: UK-AMO 1995-01-01 2030-01-01    site       site            site
#> 2: UK-AMO 1995-01-01 2030-01-01  D_SNOW     D_SNOW      Snow depth
#> 3: UK-AMO 1995-01-01 2030-01-01  DATECT     DATECT       timestamp
#> 4: UK-AMO 1995-01-01 2030-01-01       G          G  Soil heat flux
#> 5: UK-AMO 1995-01-01 2030-01-01 G_4_1_1    G_4_1_1  Soil heat flux
#> 6: UK-AMO 1995-01-01 2030-01-01 G_4_1_2    G_4_1_2  Soil heat flux
#> 
#> Site information (dt_site):
#>      site        long_name    lon     lat  elev
#>    <char>           <char>  <num>   <num> <int>
#> 1: UK-AMO Auchencorth Moss -3.243 55.7923   120
#> 
#> QC information (dt_qc) - first 6 cols:
#> NULL
#> 
#> Reference data (dt_ref) - first 6 cols:
#> NULL
```

A typical workflow would go on to perform tasks such as adding reference
data from ECMWF ERA5 reanalysis, join with other `metamet` objects,
apply quality control algorithms, impute missing values by various
algorithms, and check the data manually for additional QC. This is
illustrated below.

``` r
mm <- add_era5(
  mm,
  fname_era5 = testthat::test_path("data-raw/dt_era5.csv")
)
mm <- join(mm, mm_old)
mm <- apply_qc(mm)
mm <- impute(mm = mm)
run_shiny()
```

Clearly a two-dimensional data table is not sufficient to hold all the
information. Instead we define a `metamet` data object as a set of
related data tables. We implement this as a list in R, containing five
data tables (prefix `dt_`) explained in the table below.

| Name | Type | Contains | Rows correspond to | Columns correspond to |
|----|----|----|----|----|
| dt | data.table | sensor data | time intervals | variables |
| dt_meta | data.table | variable- and time-specific meta data (like netCDF data attributes) e.g. coords for sensor locations | variables x time period | metadata variables |
| dt_site | data.table | site-specific meta data (like netCDF global attributes) | sites | metadata variables |
| dt_qc | data.table | QC codes | time intervals | variables |
| dt_ref | data.table | ref data e.g. era5 | time intervals | variables |
