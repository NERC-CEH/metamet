# Join two \`metamet\` objects

Combine two \`metamet\` objects by performing a full join on \`site\`
and the time variable. If the time variable names differ between the two
objects, \`mm2\`'s time column and corresponding metadata row are
renamed to match \`mm1\` prior to joining.

## Usage

``` r
join(mm1, mm2)
```

## Arguments

- mm1:

  A \`metamet\` object (a list containing at least \`dt\`, \`dt_meta\`,
  and \`dt_site\`).

- mm2:

  A \`metamet\` object to be joined onto \`mm1\`. Values from \`mm2\`
  take precedence when conflicts occur.

## Value

A \`metamet\` object with merged \`dt\`, \`dt_meta\`, and \`dt_site\`
(the returned object has the structure of \`mm2\` but contains the
combined information from both inputs).

## Details

The join is performed using \`powerjoin::power_full_join()\` for the
main data table (\`dt\`), the metadata table (\`dt_meta\`) and the site
table (\`dt_site\`). Conflict resolution uses \`coalesce_yx\`, which
prefers values from the second object (\`mm2\`) when both objects
contain differing values for the same field.

## Examples

``` r
# mm_joined <- join(mm1, mm2)
```
