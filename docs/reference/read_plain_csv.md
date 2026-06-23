# Read a plain-header CSV meteorological file

Reads a standard CSV file in which the first row is a column header. The
only preprocessing performed is normalising the timestamp column name:
any column whose name matches `"timestamp"` (case-insensitive) is
renamed to `"TIMESTAMP"` so that downstream metamet functions can locate
it consistently.

## Usage

``` r
read_plain_csv(fname, ...)
```

## Arguments

- fname:

  Path to the CSV file.

- ...:

  Additional arguments forwarded to
  [`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html),
  e.g. `sep`, `na.strings`.

## Value

A `data.table` with column names exactly as written in the file header.
No renaming is performed; the caller's `dt_meta` must use `name_dt`
values that match the actual column names. Downstream metamet functions
(`metamet_wide_to_long`, `convert_time_char_to_posix`) normalise the
time column to `TIMESTAMP` automatically based on `dt_meta`.

## Details

This reader is the appropriate choice for plain-CSV exports such as EIDC
data packages, where there is no multi-row BADC-CSV header and no
Campbell TOA5 instrument-info preamble.

## See also

[`read_ceda_csv`](https://nerc-ceh.github.io/metamet/reference/read_ceda_csv.md),
[`read_obs_autodetect`](https://nerc-ceh.github.io/metamet/reference/read_obs_autodetect.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- read_plain_csv(
  pkg_extdata("UK-WHM/historical/eidc/whim_met_2002_2023.csv")
)
} # }
```
