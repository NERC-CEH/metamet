.normalize_unit_str <- function(u) {
  lookup <- c(
    "degree_C" = "degC", # alias for degC (UDUNITS2 standard symbol)
    "celsius" = "degC", # name -> symbol
    "degrees" = "degree",
    "dimensionless" = "1", # UDUNITS2 rejects "dimensionless"; "1" is correct
    "percent" = "%",
    "W / m^2" = "W/m^2",
    "W / m2" = "W/m^2",
    "micromol / m^2 / s" = "umol/m^2/s",
    "micromol m-2 s-1" = "umol/m^2/s",
    "m / s" = "m/s",
    "m s-1" = "m/s"
  )
  u <- trimws(u)
  ifelse(u %in% names(lookup), unname(lookup[u]), u)
}

.convert_col <- function(x, from, to) {
  tryCatch(
    units::drop_units(
      units::set_units(
        units::set_units(x, from, mode = "standard"),
        to,
        mode = "standard"
      )
    ),
    error = function(e) {
      warning(
        "Unit conversion '",
        from,
        "' -> '",
        to,
        "' failed: ",
        conditionMessage(e),
        " -- values left unconverted.",
        call. = FALSE
      )
      x
    }
  )
}
