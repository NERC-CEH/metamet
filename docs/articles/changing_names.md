# Change the naming convention of measurement variables

This vignette illustrates how to change the naming convention of
measurement variables.

## Local names in original data file

We start with the ad hoc names given locally to variables in the raw
data file we have read in.

``` r
library(metamet)

names(mm1$dt)
#> [1] "TIMESTAMP"    "P_12_1_1"     "D_SNOW_4_1_1" "site"
```

## Convert to ICOS naming convention

``` r
mm1_icos <- change_naming_convention(mm1, name_convention = "name_icos")
names(mm1_icos$dt)
#> [1] "TIMESTAMP"    "P_12_1_1"     "D_SNOW_4_1_1" "site"
```

## Convert to ERA5 naming convention

``` r
names(mm1_icos$dt)
#> [1] "TIMESTAMP"    "P_12_1_1"     "D_SNOW_4_1_1" "site"
mm1_era5 <- change_naming_convention(mm1, name_convention = "name_era5")
names(mm1_era5$dt)
#> [1] "time"      "tp_12_1_1" "sd_4_1_1"  "site"
```
