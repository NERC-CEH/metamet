# Rename variables in ERA5 reference data to match observation names

Renames columns in the ERA5 reference data table (\`dt_ref\`) within a
\`metamet\` object so they correspond to the column names in the primary
observation table (\`dt\`). The mapping is based on the \`dt_meta\`
metadata table.

## Usage

``` r
rename_era5(mm)
```

## Arguments

- mm:

  A \`metamet\` object containing at least \`dt\`, \`dt_meta\`,
  \`dt_site\`, and \`dt_ref\`.

## Value

The updated \`metamet\` object \`mm\`, with ERA5 reference data columns
renamed to be consistent with observations.

## Details

\- Sets the \`site\` column in \`dt_ref\` using the value from
\`dt_site\`. - Matches column names in \`dt\` to those in \`dt_ref\`
using the \`standard_name_era5\` field in \`dt_meta\`. - Reorders and
renames columns in \`dt_ref\` to match the structure of \`dt\`.

## See also

\-
[`add_era5`](https://nerc-ceh.github.io/metamet/reference/add_era5.md)
for populating ERA5 reference data

## Examples

``` r
# mm <- rename_era5(mm)
```
