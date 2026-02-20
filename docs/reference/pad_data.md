# pad_data

Adds in any gaps in a data frame representing a time series

## Usage

``` r
pad_data(dt, by = NULL, time_name = NULL, v_dates = NULL)
```

## Arguments

- by:

  Time interval of series, Default: '30 min'

- time_name:

  Column name for POSIX date/time variable in df, Default: 'DATECT'

- v_dates:

  A vector of POSIX date/times, potentially from another df, to match
  with it.

- df:

  A data frame

## Value

OUTPUT_DESCRIPTION

## Details

DETAILS

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 #EXAMPLE1
 df <- pad_data(df)
 }
} # }
```
