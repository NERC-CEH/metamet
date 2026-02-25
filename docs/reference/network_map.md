# Create a Leaflet Map of Monitoring Network Sites

Produces a simple interactive leaflet map showing monitoring sites.
Popups are generated using \`openairmaps::buildPopup()\` and include
site name, code, and elevation. The function defaults to using
\`mm\$dt_site\` if no site table is supplied.

## Usage

``` r
network_map(dt_site = mm$dt_site)
```

## Arguments

- dt_site:

  A \`data.table\` containing site metadata, including \`lat\`, \`lon\`,
  \`site\`, \`long_name\`, and \`elev\`. Defaults to \`mm\$dt_site\` if
  available in the calling environment.

## Value

A leaflet map widget with markers for each site.

## Examples

``` r
if (FALSE) { # \dontrun{
network_map(mm$dt_site)
} # }
```
