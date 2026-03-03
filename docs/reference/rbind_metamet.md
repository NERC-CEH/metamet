# Combine lists of metamet data tables into a single metamet object

This function binds lists of data tables (\`l_dt\`, \`l_dt_meta\`,
\`l_dt_site\`) using \`data.table::rbindlist()\` and inserts the
combined tables into an existing metamet object (\`mm\`). It is
typically used to merge metamet data from multiple sites or time
periods.

## Usage

``` r
rbind_metamet(mm, l_dt, l_dt_meta, l_dt_site)
```

## Arguments

- mm:

  A metamet object into which combined tables will be stored. The object
  should contain (or be able to contain) elements \`dt\`, \`dt_meta\`,
  and \`dt_site\`.

- l_dt:

  A list of data tables to be row-bound into \`mm\$dt\`.

- l_dt_meta:

  A list of metadata tables to be row-bound into \`mm\$dt_meta\`.

- l_dt_site:

  A list of site information tables to be row-bound into
  \`mm\$dt_site\`.

## Value

The modified metamet object with combined \`dt\`, \`dt_meta\`, and
\`dt_site\` components.

## Examples

``` r
if (FALSE) { # \dontrun{
mm <- rbind_metamet(
  mm,
  l_dt = list(dt1, dt2),
  l_dt_meta = list(meta1, meta2),
  l_dt_site = list(site1, site2)
)
} # }
```
