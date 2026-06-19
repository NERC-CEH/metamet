# Reshape a metamet object between wide and long format

Converts a \`metamet\` object to the target format. Wide format stores
one column per variable (the default on construction); long format
stores one row per variable with \`qc\`, \`ref\`, and metadata merged
into a single table.

## Usage

``` r
metamet_reshape(mm, format = c("wide", "long"))
```

## Arguments

- mm:

  A \`metamet\` object. Should have a \`"format"\` attribute of
  \`"wide"\` or \`"long"\` (set automatically on construction). Objects
  loaded from older \`.rds\` files without this attribute are assumed to
  be wide.

- format:

  Target format: \`"wide"\` or \`"long"\`.

## Value

The reshaped \`metamet\` object with the \`"format"\` attribute updated.

## Examples

``` r
if (FALSE) { # \dontrun{
mm_long <- metamet_reshape(mm, "long")
mm_wide <- metamet_reshape(mm_long, "wide")
} # }
```
