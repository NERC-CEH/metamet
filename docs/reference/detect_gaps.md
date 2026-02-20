# detect_gaps

Detects any gaps in a data frame representing a time series

## Usage

``` r
detect_gaps(dt_in, expected_interval_mins = NULL, time_name = NULL)
```

## Arguments

- time_name:

  Column name for POSIX date/time variable in df

- df:

  A data frame

- by:

  Time interval of series, Default: '30 min'

## Value

OUTPUT_DESCRIPTION

## Details

DETAILS

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 #EXAMPLE1
 gaps <- detect_gaps(l_logr$df)
 }
} # }
```
