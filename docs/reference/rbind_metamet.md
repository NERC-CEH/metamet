# Combine metamet objects or data-table lists into a single metamet object

Two calling conventions are supported:

## Usage

``` r
rbind_metamet(mm_or_list, l_dt, l_dt_meta, l_dt_site)
```

## Arguments

- mm_or_list:

  Either a list of \`metamet\` objects (new interface) or a single
  \`metamet\` object to receive combined tables (legacy interface).

- l_dt:

  (Legacy) A list of data tables to row-bind into \`mm\$dt\`.

- l_dt_meta:

  (Legacy) A list of metadata tables to row-bind into \`mm\$dt_meta\`.

- l_dt_site:

  (Legacy) A list of site tables to row-bind into \`mm\$dt_site\`.

## Value

A \`metamet\` object with combined data.

## Details

\*\*List of \`metamet\` objects\*\* (preferred):
\`rbind_metamet(list(mm1, mm2, mm3))\` All objects must have the same
\`format\` attribute (\`"wide"\` or \`"long"\`). For \*\*long-format\*\*
objects the combination is a plain \`rbindlist\` — rows from different
sites never share the same \`(site, TIMESTAMP, var_name)\` key, so no
join logic is needed. For \*\*wide-format\*\* objects \`Reduce(join,
l_mm)\` is used, which applies \`power_full_join\` with \`coalesce_yx\`
for correct handling of overlapping timestamps within a site.

\*\*Legacy four-argument form\*\* (kept for backward compatibility):
\`rbind_metamet(mm, l_dt, l_dt_meta, l_dt_site)\` Row-binds lists of
individual data tables into an existing \`metamet\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# New interface — list of metamet objects:
mm_all <- rbind_metamet(list(mm_amo_qc, mm_buc_qc, mm_ebu_qc, mm_whm_qc))

# Legacy interface:
mm <- rbind_metamet(
  mm,
  list(dt1, dt2), list(meta1, meta2), list(site1, site2)
)
} # }
```
