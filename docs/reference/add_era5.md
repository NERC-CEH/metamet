# Add ERA5 reference data to a \`metamet\` object

Reads ERA5 meteorological data and attaches it to a \`metamet\` object
as reference data (\`dt_ref\`). Converts accumulated precipitation to a
rate, renames variables, and ensures temporal alignment between observed
and reference data. Optionally restricts date ranges and harmonizes time
resolution with the main observations.

## Usage

``` r
add_era5(
  mm,
  fname_era5 = "data-raw/UK-AMO/dt_era5.csv",
  restrict_ref_to_obs = TRUE,
  restrict_obs_to_ref = FALSE,
  report_end_interval = TRUE,
  first_date = NULL,
  last_date = NULL,
  extra_rows = 2
)
```

## Arguments

- mm:

  A \`metamet\` object containing observation data and metadata.

- fname_era5:

  Path to a CSV file containing ERA5 data.

- restrict_ref_to_obs:

  Logical. If \`TRUE\` (default), restricts reference data to the date
  range of observations.

- restrict_obs_to_ref:

  Logical. If \`TRUE\`, restricts observations to the date range of the
  reference data.

- report_end_interval:

  Logical. Should the time be reported at the end (\`TRUE\`, default) or
  start (\`FALSE\`) of the -averaging interval.

- first_date:

  Optional. Earliest date to include (overrides automatic date selection
  if set).

- last_date:

  Optional. Latest date to include (overrides automatic date selection
  if set).

- extra_rows:

  Integer. Extra rows to add when time-averaging for padding.

## Value

The \`metamet\` object \`mm\`, with processed ERA5 data available as
\`mm\$dt_ref\`.

## Details

\- Converts ERA5 precipitation ("tp") from a sum (in mm) to a rate
(mm/s). - Renames variables in ERA5 data to match main observations
according to \`dt_meta\`. - Optionally limits ERA5 data to match the
observation period, and vice versa. - Ensures reference and observed
data tables have matching time resolutions, aggregating or
disaggregating if needed. - Warns if dimensions become mismatched.

## See also

\-
[`rename_era5`](https://nerc-ceh.github.io/metamet/reference/rename_era5.md)
for variable renaming -
[`time_average_dt`](https://nerc-ceh.github.io/metamet/reference/time_average_dt.md)
for time-averaging implementation -
[`metamet`](https://nerc-ceh.github.io/metamet/reference/metamet.md) for
object construction

## Examples

``` r
# mm <- add_era5(mm)
```
