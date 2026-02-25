#' Create a Polar Plot Map for a Selected Variable
#'
#' Generates an interactive polar plot map using `openairmaps::polarMap()`
#' for a variable contained in a `mm` object. The function extracts the
#' appropriate time, wind speed, wind direction, and precipitation fields
#' from `mm$dt` based on metadata stored in `mm$dt_meta`, merges site
#' information, and produces a leaflet-based polar map.
#'
#' @param mm A list-like object containing at least:
#'   * `dt`: a `data.table` with measurement data.
#'   * `dt_meta`: a `data.table` describing variable types and names.
#'   * `dt_site`: a `data.table` with site metadata including `lat`, `lon`, and `site`.
#' @param var_name A character string giving the name of the variable
#'   column in `mm$dt` to be mapped.
#'
#' @return A leaflet map widget produced by `openairmaps::polarMap()`.
#'
#' @examples
#' \dontrun{
#' polar_map(mm, "no2")
#' }
#'
#' @export
polar_map <- function(mm, var_name) {
  dt <- copy(mm$dt)
  # get the name and format of the time, precip, ws & wd variables
  time_name <- mm$dt_meta[type == "time", name_dt]
  precip_name <- mm$dt_meta[type == "precipitation", name_dt]
  wd_name <- mm$dt_meta[type == "wind direction", name_dt]
  ws_name <- mm$dt_meta[type == "wind speed" | type == "windspeed", name_dt]

  dt[, date := get(time_name)]
  dt[, wd := get(wd_name)]
  dt[, ws := get(ws_name)]

  dt <- dt[mm$dt_site, on = .(site = site)]

  openairmaps::polarMap(
    dt,
    pollutant = var_name,
    latitude = "lat",
    longitude = "lon",
    popup = "site"
  )
}

#' Create a Leaflet Map of Monitoring Network Sites
#'
#' Produces a simple interactive leaflet map showing monitoring sites.
#' Popups are generated using `openairmaps::buildPopup()` and include
#' site name, code, and elevation. The function defaults to using
#' `mm$dt_site` if no site table is supplied.
#'
#' @param dt_site A `data.table` containing site metadata, including
#'   `lat`, `lon`, `site`, `long_name`, and `elev`. Defaults to `mm$dt_site`
#'   if available in the calling environment.
#'
#' @return A leaflet map widget with markers for each site.
#'
#' @examples
#' \dontrun{
#' network_map(mm$dt_site)
#' }
#'
#' @export
network_map <- function(dt_site = mm$dt_site) {
  map_data <-
    dt_site |>
    # build a popup
    openairmaps::buildPopup(
      latitude = "lat",
      longitude = "lon",
      columns = c(
        "Name" = "long_name",
        "Site code" = "site",
        "Elevation (m)" = "elev"
      )
    ) |>
    # get unique sites
    dplyr::distinct(site, .keep_all = TRUE)

  # create a basic leaflet map
  leaflet::leaflet(map_data) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(popup = ~popup)
}
