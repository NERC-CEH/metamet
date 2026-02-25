# Create a Polar Plot Map for a Selected Variable

Generates an interactive polar plot map using
\`openairmaps::polarMap()\` for a variable contained in a \`mm\` object.
The function extracts the appropriate time, wind speed, wind direction,
and precipitation fields from \`mm\$dt\` based on metadata stored in
\`mm\$dt_meta\`, merges site information, and produces a leaflet-based
polar map.

## Usage

``` r
polar_map(mm, var_name)
```

## Arguments

- mm:

  A list-like object containing at least: \* \`dt\`: a \`data.table\`
  with measurement data. \* \`dt_meta\`: a \`data.table\` describing
  variable types and names. \* \`dt_site\`: a \`data.table\` with site
  metadata including \`lat\`, \`lon\`, and \`site\`.

- var_name:

  A character string giving the name of the variable column in
  \`mm\$dt\` to be mapped.

## Value

A leaflet map widget produced by \`openairmaps::polarMap()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
polar_map(mm, "no2")
} # }
```
