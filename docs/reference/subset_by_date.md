# Subset a \`metamet\` object by date

Subsets a \`metamet\` object to a date range between a start and end
date. This is applied to the observation data (\`dt\`) and the QC
(\`dt_qc\`) and reference data (\`dt_ref\`) if they are present.

## Usage

``` r
subset_by_date(
  mm,
  start_date = "2025-01-01 00:30:00",
  end_date = "2026-01-01 00:00:00"
)
```

## Arguments

- mm:

  A \`metamet\` object containing observation data and metadata.

- start_date:

  Earliest date to include in subset.

- end_date:

  Last date to include in subset.

## Value

The \`metamet\` object \`mm\`, restricted to between start and end
dates.

## Details

\- Limits all time-referenced data to between the start and end dates. -
Dates are inclusive. - Warns if date variable not present.

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
if (FALSE) { # \dontrun{
mm <- subset_by_date(
  mm,
  start_date = "2025-06-01 00:30:00",
  end_date   = "2025-06-02 00:00:00"
)
} # }
```
