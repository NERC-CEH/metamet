# Populate convention columns in dt_meta from the package registry

Fills missing \`units\_\<conv\>\` and \`long_name\_\<conv\>\` values in
\`dt_meta\` from the package registry (\`l_conventions\`), for any
convention column (\`name_icos\`, \`name_era5\`, etc.) that is present.

## Usage

``` r
populate_convention_cols(dt_meta, overwrite = FALSE)
```

## Arguments

- dt_meta:

  A \`data.table\` of column-level metadata.

- overwrite:

  Logical (default \`FALSE\`). If \`FALSE\`, only fills cells that are
  currently \`NA\`. If \`TRUE\`, overwrites all cells with registry
  values where a match exists.

## Value

A \`data.table\` with convention columns populated.
