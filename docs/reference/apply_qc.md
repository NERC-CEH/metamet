# Apply Quality Control Checks to Meteorological Data

Applies range-based quality control to a \`metamet\` object by setting
out-of-range values to \`NA\` and recording QC codes in the \`qc\`
column. Accepts both wide and long format; always returns long format.

## Usage

``` r
apply_qc(mm0)
```

## Arguments

- mm0:

  A \`metamet\` object (wide or long format).

## Value

A long-format \`metamet\` object with updated \`value\`, \`qc\`, and
\`validator\` columns.

## Details

The function performs the following steps:

1.  Reshapes to long format if necessary.

2.  Sets values outside the min/max range specified in \`dt_meta\` to
    \`NA\`.

3.  Sets \`qc = 1\` for missing or invalid values, \`qc = 0\` for valid
    values.

4.  Sets \`validator = "auto"\` for flagged rows.

## Examples

``` r
# mm_qc <- apply_qc(mm)
```
