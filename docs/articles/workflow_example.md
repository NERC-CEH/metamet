# Workflow Example 1

This vignette illustrates the typical workflow for processing met data
with `metamet`. The basic steps are to:

- read the observation data
- create a corresponding metadata table `dt_meta` for the observed
  variables
- create a metadata table `dt_site` for each site with observations
- combine these into a single `metamet` object

## Example data

To illustrate, we will use some publicy available data from Whim Moss,
available from the [EIDC data
centre](https://catalogue.ceh.ac.uk/documents/ba9ecad2-1740-41e6-8f73-b308340d49fe).
Metadata are available in the supporting documentation, but not in a
machine-readable format, so we have to extract it manually the first
time. Thereafter it is available within the `metamet` object for all
future use.

Firstly, we load the `metamet` library, as well as the `here` library
which simplifies specifying file paths.

``` r
here::i_am("vignettes/workflow_example.Rmd")
library(metamet)
library(here)
```

### Observation data `dt`

Having downloaded the observation data, we can read it from a file and
display a few rows.

``` r
fname <- here("tests/testthat/data-raw/UK-WHM/whim_met_2002_2023.csv")
dt <- fread(fname)
dim(dt)
#> [1] 736321     14
dt
#>                Timestamp  Rain     LWS  AirT    RH   PAR Total_solar Net_rad
#>                   <char> <num>   <num> <num> <num> <num>       <num>   <num>
#>      1: 01/01/2003 00:00     0 6999.00 1.534  93.9    NA           0 -23.820
#>      2: 01/01/2003 00:15     0 6999.00 1.509  93.8    NA           0 -33.320
#>      3: 01/01/2003 00:30     0 6999.00 1.354  94.3    NA           0 -37.650
#>      4: 01/01/2003 00:45     0 6999.00 1.359  94.5    NA           0 -25.340
#>      5: 01/01/2003 01:00     0 6999.00 1.350  94.5    NA           0 -26.670
#>     ---                                                                     
#> 736317: 31/12/2023 23:00     0   18.96 2.796    NA     0           0  -1.570
#> 736318: 31/12/2023 23:15     0   18.66 2.848    NA     0           0  -3.031
#> 736319: 31/12/2023 23:30     0   18.51 2.845    NA     0           0  -4.105
#> 736320: 31/12/2023 23:45     0   18.32 2.902    NA     0           0  -5.239
#> 736321: 01/01/2024 00:00     0   17.85 2.959    NA     0           0  -2.587
#>            WS    WD Soil_VWC Soil_T1 Soil_T2   WTD
#>         <num> <num>    <num>   <num>   <num> <num>
#>      1: 5.605 153.8       NA   3.979   4.441    NA
#>      2: 5.713 160.8       NA   3.984   4.438    NA
#>      3: 5.388 157.7       NA   3.967   4.430    NA
#>      4: 5.675 157.1       NA   3.960   4.426    NA
#>      5: 5.320 155.8       NA   3.946   4.418    NA
#>     ---                                           
#> 736317: 1.237 218.6     91.2   5.090   4.911  4.48
#> 736318: 0.726 226.5     91.2   5.084   4.909  4.49
#> 736319: 0.738 212.5     91.2   5.085   4.907  4.50
#> 736320: 1.286 239.3     91.2   5.086   4.906  4.50
#> 736321: 0.859 253.6     91.2   5.086   4.912  4.53
```

The data consist of a timestamp column, and 13 variables observed every
15 minutes from 2003 until 2023, giving 736321 rows and 14 columns.

### Observation metadata `dt_meta`

``` r
knitr::kable(dt_meta[site == "UK-WHM", ..v_col], format = "html")
```

| site | name_dt | name_local | units_local | type | time_char_format | range_min | range_max | name_era5 | units_era5 | imputation_method |
|:---|:---|:---|:---|:---|:---|---:|---:|:---|:---|:---|
| UK-WHM | site | site | NA | site | NA | NA | NA | site | NA | era5 |
| UK-WHM | Timestamp | Timestamp | NA | time | %d/%m/%Y %H:%M | NA | NA | time | NA | NA |
| UK-WHM | Rain | Rain | mm | precipitation | NA | 0 | 50 | tp | mm | era5 |
| UK-WHM | LWS | LWS | dimensionless | arbitrary | NA | -99999 | 99999 | rh | 1 | era5 |
| UK-WHM | AirT | AirT | degree_C | temperature | NA | -40 | 50 | t2m | degree_C | era5 |
| UK-WHM | RH | RH | percent | humidity | NA | 30 | 120 | rh | percent | era5 |
| UK-WHM | PAR | PAR | micromol / m^2 / s | energy flux | NA | 0 | 2200 | ssrd | micromol / m^2 / s | era5 |
| UK-WHM | Total_solar | Total_solar | W / m^2 | energy flux | NA | 0 | 1200 | ssrd | W / m^2 | era5 |
| UK-WHM | Net_rad | Net_rad | W / m^2 | energy flux | NA | -500 | 1200 | rn | W / m^2 | era5 |
| UK-WHM | WS | WS | m / s | wind speed | NA | 0 | 30 | ws | m / s | era5 |
| UK-WHM | WD | WD | degree | wind direction | NA | 0 | 360 | wd | degree | era5 |
| UK-WHM | Soil_VWC | Soil_VWC | percent | soil moisture | NA | 0 | 100 | swvl1 | percent | era5 |
| UK-WHM | Soil_T1 | Soil_T1 | degree_C | temperature | NA | -20 | 50 | stl1 | degree_C | era5 |
| UK-WHM | Soil_T2 | Soil_T2 | degree_C | temperature | NA | -20 | 50 | stl1 | degree_C | era5 |
| UK-WHM | WTD | WTD | cm | height | NA | -10 | 10 | swvl1 | m | era5 |

### Site data `dt_site`

The site metadata required is minimal. Any additional variables can be
added, but at a minimum we require the site name, a uniquely identifying
code `site`, longitude, latitude (in degrees with a decimal fraction)
and elevation (in m). These are easily gleaned from the supporting
documentation and could be entered in excel, read in as a .csv text
file, or entered directly in R as below.

``` r
dt_site <- data.table(
  site = "UK-WHM",
  long_name = "Whim Moss",
  lon = -3.27155,
  lat = -55.76566,
  elev = 316
)
dt_site
#>      site long_name      lon       lat  elev
#>    <char>    <char>    <num>     <num> <num>
#> 1: UK-WHM Whim Moss -3.27155 -55.76566   316
```

In practice, we are likely to have multiple sites as in the table below,
and it is easiest to append rows to a .csv or excel file as new sites
are added.

``` r
fname <- here("data-raw/dt_site.csv")
dt_site <- fread(fname)
knitr::kable(dt_site, format = "html")
```

| site   | long_name        |      lon |      lat | elev |
|:-------|:-----------------|---------:|---------:|-----:|
| UK-AMO | Auchencorth Moss | -3.24300 | 55.79230 |  120 |
| UK-EBU | Easter Bush      | -3.20710 | 55.86740 |  119 |
| UK-WHM | Whim Moss        | -3.27155 | 55.76566 |  316 |

### - Create `metamet` object

The `html_vignette` template includes a basic CSS theme. To override
this theme you can specify your own CSS in the document metadata as
follows:

    output: 
      rmarkdown::html_vignette:
        css: mystyles.css

## Figures

The figure sizes have been customised so that you can easily put two
images side-by-side.

``` r
plot(1:10)
plot(10:1)
```

![](reference/figures/README-unnamed-chunk-9-1.png)![](reference/figures/README-unnamed-chunk-9-2.png)

You can enable figure captions by `fig_caption: yes` in YAML:

    output:
      rmarkdown::html_vignette:
        fig_caption: yes

Then you can use the chunk option `fig.cap = "Your figure caption."` in
**knitr**.

## More Examples

You can write math expressions, e.g. $`Y = X\beta + \epsilon`$,
footnotes[^1], and tables, e.g. using
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

|                   |  mpg | cyl |  disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:------------------|-----:|----:|------:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4         | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag     | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710        | 22.8 |   4 | 108.0 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive    | 21.4 |   6 | 258.0 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout | 18.7 |   8 | 360.0 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |
| Valiant           | 18.1 |   6 | 225.0 | 105 | 2.76 | 3.460 | 20.22 |   1 |   0 |    3 |    1 |
| Duster 360        | 14.3 |   8 | 360.0 | 245 | 3.21 | 3.570 | 15.84 |   0 |   0 |    3 |    4 |
| Merc 240D         | 24.4 |   4 | 146.7 |  62 | 3.69 | 3.190 | 20.00 |   1 |   0 |    4 |    2 |
| Merc 230          | 22.8 |   4 | 140.8 |  95 | 3.92 | 3.150 | 22.90 |   1 |   0 |    4 |    2 |
| Merc 280          | 19.2 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.30 |   1 |   0 |    4 |    4 |

Also a quote using `>`:

> “He who gives up \[code\] safety for \[code\] speed deserves neither.”
> ([via](https://twitter.com/hadleywickham/status/504368538874703872))

[^1]: A footnote here.
