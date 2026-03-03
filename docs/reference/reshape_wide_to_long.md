# Reshape a metamet object from wide to long format

This function converts the component data tables of a metamet (\`mm\`)
object—\`dt\`, \`dt_qc\`, and \`dt_ref\`—from wide format to long format
using \`data.table::melt()\`. It merges QC flags, reference values, and
metadata into a single long-format \`dt\` table and removes the
now-redundant \`dt_qc\` and \`dt_ref\` components.

## Usage

``` r
reshape_wide_to_long(mm)
```

## Arguments

- mm:

  A metamet object (a list-like structure) containing at least the
  elements \`dt\`, \`dt_qc\`, \`dt_ref\`, and \`dt_meta\`. Each
  component must be a \`data.table\` with expected columns: \* \`dt\`:
  \`site\`, \`TIMESTAMP\`, measurement columns \* \`dt_qc\`: \`site\`,
  \`TIMESTAMP\`, \`validator\`, QC columns \* \`dt_ref\`: \`site\`,
  \`TIMESTAMP\`, reference columns \* \`dt_meta\`: includes \`site\`,
  \`name_dt\`, \`type\`, \`name_icos\`

## Value

The modified metamet object, where: \* \`mm\$dt\` is now a long-format
table containing measurement values, QC flags, validator names,
reference values, and metadata. \* \`mm\$dt_qc\` and \`mm\$dt_ref\` are
set to \`NULL\` because their content has been merged into \`mm\$dt\`.

## Details

Internally, the function: 1. Melts each data table to long format. 2.
Sets keys and performs keyed merges for efficient joining. 3. Annotates
\`mm\$dt\` with QC, reference, and metadata fields.

This function is designed for harmonising metamet objects before
downstream validation and processing.

## Examples

``` r
if (FALSE) { # \dontrun{
mm_long <- reshape_wide_to_long(mm)
} # }
```
