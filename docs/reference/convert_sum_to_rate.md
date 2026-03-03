# Convert a summed variable to a rate

Converts accumulated or summed meteorological variables (e.g.,
precipitation) to a rate by dividing by the time interval between
observations. This is useful for standardizing variables recorded as
cumulative sums to rate units (per second).

## Usage

``` r
convert_sum_to_rate(dt, v_var_to_convert = NA, time_name = NA)
```

## Arguments

- dt:

  A \`data.table\` containing the variables to convert. Modified by
  reference.

- v_var_to_convert:

  Character vector of column names in \`dt\` to convert from a sum to a
  rate. For example, \`c("P_12_1_1", "LWS_4_1_2")\`. If \`NA\`
  (default), no conversion is performed.

- time_name:

  Character string specifying the name of the time column in \`dt\`
  (e.g., \`"DATECT"\`). Used to calculate the interval length between
  the first two observations. If \`NA\` (default), an error will occur.

## Value

The \`data.table\` \`dt\` with specified columns converted to rates
(units per second). The input table is modified by reference and
invisibly returned.

## Details

The function calculates the time interval from the first two
observations and divides all specified variables by this interval length
(in seconds). Units are converted from their original units to rates per
second.

Note: Unit attributes in the metadata are not automatically updated;
ensure metadata reflects the new rate units after conversion.

## See also

[`add_era5`](https://nerc-ceh.github.io/metamet/reference/add_era5.md)
for an application converting ERA5 precipitation
