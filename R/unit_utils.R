.normalize_unit_str <- function(u) {
  lookup <- c(
    "degC" = "celsius",
    "degree_C" = "celsius",
    "degrees" = "degree",
    "percent" = "%",
    "W / m^2" = "W/m^2",
    "W / m2" = "W/m^2",
    "micromol / m^2 / s" = "umol/m^2/s",
    "m / s" = "m/s"
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
