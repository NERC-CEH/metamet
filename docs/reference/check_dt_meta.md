# Check dt_meta convention columns for consistency

Validates that all sensors of the same variable share identical
convention units. For example, all rows with \`name_icos == "TA"\` must
have the same \`units_icos\`. Called automatically by \[new_metamet()\];
also available for direct use.

## Usage

``` r
check_dt_meta(dt_meta, check_against_standard = FALSE)
```

## Arguments

- dt_meta:

  A \`data.table\` of column-level metadata (the \`dt_meta\` slot of a
  metamet object).

- check_against_standard:

  Logical (default \`FALSE\`). If \`TRUE\`, also warns when a variable's
  convention units differ from the package registry (\`l_conventions\`).

## Value

\`dt_meta\` invisibly.
