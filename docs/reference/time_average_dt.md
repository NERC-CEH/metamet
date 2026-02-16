# Time-average a data table

Aggregates meteorological data in a data table over specified time
intervals. Handles flexible variable naming, wind direction vector
averaging, and configurable reporting of interval start or end times.
Uses
[`openair::timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.html)
for aggregation with support for custom statistics (mean, median, sum,
etc.).

## Usage

``` r
time_average_dt(
  dt_in,
  avg.time = "30 min",
  statistic = "mean",
  first_date = NULL,
  last_date = NULL,
  time_name = NULL,
  wd_name = NULL,
  ws_name = NULL,
  report_end_interval = TRUE,
  extra_rows = 2
)
```

## Arguments

- dt_in:

  A data table containing meteorological observations with a time
  column.

- avg.time:

  Time interval for averaging (e.g., "30 min", "1 hour"). Default: "30
  min". Passed to
  [`openair::timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.html).

- statistic:

  Character string specifying the aggregation statistic ("mean",
  "median", "sum", etc.). Default: "mean". Use "median" for quality
  control codes.

- first_date:

  Optional POSIXct date. Earliest date to include. If `NULL`, uses
  minimum date in data.

- last_date:

  Optional POSIXct date. Latest date to include. If `NULL`, uses maximum
  date in data.

- time_name:

  Character string. Name of the time column in `dt_in` (e.g., "time").

- wd_name:

  Character string. Name of wind direction column, if present. Used for
  vector averaging.

- ws_name:

  Character string. Name of wind speed column, if present. Used for
  vector averaging.

- report_end_interval:

  Logical. If `TRUE` (default), timestamps report the end of the
  averaging interval. If `FALSE`, timestamps report the start of the
  interval (default `openair` behavior).

- extra_rows:

  Integer (default: 2). Number of extra time intervals to include before
  and after the specified date range, used for padding during averaging.

## Value

A data table with time-averaged values, preserving the original time
column name and variable structure. The "site" column is stored as
character.

## Details

\- Creates a temporary copy of input data to avoid modifying the
original by reference. - Renames the time column to "date" for
compatibility with
[`openair::timeAverage()`](https://openair-project.github.io/openair/reference/timeAverage.html). -
Optionally renames wind direction and wind speed columns for proper
vector averaging. - Pads the averaging window with extra intervals
(controlled by `extra_rows`) then restricts results to the original date
range. - Converts any factor columns (e.g., "site") to character for
consistency. - Uses "nocb" (next observation carried backward) to fill
leading missing values. - Restores original time and column names in the
output.

## See also

\-
[`time_average`](https://nerc-ceh.github.io/metamet/reference/time_average.md)
for averaging a \`metamet\` object -
[`add_era5`](https://nerc-ceh.github.io/metamet/reference/add_era5.md)
for applying time averaging to ERA5 reference data

## Examples

``` r
# dt_averaged <- time_average_dt(
#   dt_in = my_dt,
#   avg.time = "1 hour",
#   time_name = "time",
#   wd_name = "wind_direction",
#   ws_name = "wind_speed"
# )
```
