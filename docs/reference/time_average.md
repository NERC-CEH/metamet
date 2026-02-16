# Time-average a \`metamet\` object

Aggregates meteorological data in a \`metamet\` object over specified
time intervals. The function handles different variable types
appropriately: precipitation is summed, and other variables
(temperature, wind speed, etc.) are averaged. Wind direction is
vector-averaged if present.

## Usage

``` r
time_average(
  mm_in,
  avg.time = "30 min",
  report_end_interval = TRUE,
  extra_rows = 0
)
```

## Arguments

- mm_in:

  A \`metamet\` object containing at least \`dt\`, \`dt_meta\`, and
  \`dt_site\`.

- avg.time:

  Time interval for averaging; passed to \`openair::timeAverage()\`.
  Default is \`"30 min"\`.

- report_end_interval:

  Logical. If \`TRUE\` (default), the returned timestamps represent the
  end of the averaging interval. If \`FALSE\`, timestamps represent the
  start of the interval.

- extra_rows:

  Integer A number of time intervals to add before and after the data;
  usually truncated after averaging.

## Value

A \`metamet\` object with time-averaged \`dt\`, \`dt_qc\`, and
\`dt_ref\` tables (where applicable). The object structure is preserved.

## Details

The function uses \`openair::timeAverage()\` for the aggregation and
preserves the structure of the input object, including quality control
(\`dt_qc\`) and reference (\`dt_ref\`) tables if present.

## Examples

``` r
# mm_avg <- time_average(mm, avg.time = "1 hour")
```
