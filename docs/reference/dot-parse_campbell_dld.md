# Parse a Campbell .dld Final Storage Label file

Extracts Output_Table column definitions from the label section of a
Campbell Scientific .dld file.

## Usage

``` r
.parse_campbell_dld(fname_dld)
```

## Arguments

- fname_dld:

  Path to the .dld file.

## Value

A named list (keyed by declared table ID) where each element is a
character vector of column names. Column 1 (the stored table ID) is
renamed `"table_id"`; `_RTM` columns become `"day_of_year"` and
`"time_hhmm"`.
