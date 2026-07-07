# Read Campbell CR23X .dat files using .dld column definitions

Parses the headerless comma-separated data files produced by Campbell
Scientific CR23X dataloggers, using the accompanying Final Storage Label
(.dld) file to assign column names. Multiple output tables with
different sampling intervals coexist in the same .dat file; each is
returned as a separate element of a named list.

## Usage

``` r
read_old_campbell_dat(fname_dat, fname_dld, year = NULL, table_id = NULL)
```

## Arguments

- fname_dat:

  Path to the Campbell .dat file.

- fname_dld:

  Path to the corresponding .dld (Final Storage Label) file.

- year:

  Integer. Calendar year for the first data row. If `NULL` (default) it
  is inferred from the filename as described above.

- table_id:

  Integer or character. If supplied, return only the `data.table` for
  this table identifier. Otherwise a named list of `data.table`s is
  returned, one per table found in the file.

## Value

A named list of `data.table`s (names are the table identifiers as they
appear in column 1 of the .dat file), each with a leading `TIMESTAMP`
(POSIXct, UTC) column followed by the measurement variables named from
the .dld. A single `data.table` is returned when `table_id` is
specified.

## Details

The two `_RTM` (Real-Time) columns in each output-table definition are
treated as timestamp components: the first is day-of-year (1–366) and
the second is time of day in HHMM format (e.g. 1015 = 10:15). A
`TIMESTAMP` (POSIXct, UTC) column is synthesised from these plus the
year. Year-rollover within a file (day-of-year decreasing from ~365
to 1) is detected automatically.

When `year` is not supplied it is inferred from the filename pattern
`_YYYY_MM_DD`: if the first data row's day-of-year is \> 300 and the
filename month is \<= 3, the year is decremented by one (files
downloaded in early January commonly contain data from late December of
the prior year).

Output-table definitions are matched to data rows first by the table
identifier in column 1, then by column count as a fallback (used when
the logger program has been revised and the table number changed since
the .dld was generated).

## Examples

``` r
if (FALSE) { # \dontrun{
tables <- read_old_campbell_dat(
  pkg_extdata("UK-WHM/current/whim_met_2026_01_05.dat"),
  pkg_extdata("UK-WHM/current/Whim23X260609.dld")
)
dt_15min <- tables[["120"]] # 15-minute output table
dt_1min <- tables[["227"]] # 1-minute output table
} # }
```
