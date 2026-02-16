# Apply Quality Control Checks to Meteorological Data

Applies quality control procedures to a \`metamet\` object by removing
out-of-range values and creating a quality code table. The function
flags missing or invalid values that fall outside acceptable ranges
defined in the metadata.

## Usage

``` r
apply_qc(mm0)
```

## Arguments

- mm0:

  A \`metamet\` object containing at least \`dt\` (data table),
  \`dt_meta\` (metadata table with min/max ranges), and \`dt_site\`
  (site information).

## Value

A modified \`metamet\` object with:

- dt:

  Data values, with out-of-range values set to NA

- dt_qc:

  New quality control table (1 = invalid/missing, 0 = valid)

All other fields from the input object are preserved.

## Details

The function performs the following steps:

1.  Removes values outside the min/max range specified in \`dt_meta\`

2.  Creates a quality control table (\`dt_qc\`) where 1 indicates a
    missing or out-of-range value, and 0 indicates a valid value

3.  Adds a validator field marked as "auto" (to be replaced by username
    when manually validated)

## See also

`remove_out_of_range` for the range-checking implementation

## Examples

``` r
# mm_qc <- apply_qc(mm)
```
