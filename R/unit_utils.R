#' Normalize informal or non‑standard unit strings
#'
#' Convert a user‑supplied unit string into a canonical UDUNITS2‑compatible
#' symbol. This helper standardizes common aliases (e.g., "celsius" → "degC"),
#' fixes spacing issues (e.g., "W / m^2" → "W/m^2"), and replaces rejected
#' names such as "dimensionless" with the correct UDUNITS2 symbol `"1"`.
#'
#' @param u Character scalar. A unit string that may contain informal names,
#'   spacing variations, or alternative spellings.
#'
#' @return A character scalar containing the normalized unit string. If the
#'   input does not match any known alias, it is returned unchanged.
#'
#' @details
#' UDUNITS2 is strict about unit symbols and rejects several common informal
#' forms. This function provides a lightweight normalization layer so that
#' downstream calls to \code{units::set_units()} are less likely to fail.
#'
#' @examples
#' .normalize_unit_str("celsius")
#' .normalize_unit_str("W / m^2")
#' .normalize_unit_str("micromol m-2 s-1")
#' .normalize_unit_str("unknown_unit")  # unchanged
#'
#' @keywords internal
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

#' Convert a numeric vector between units with graceful failure handling
#'
#' Convert a numeric vector from one unit to another using UDUNITS2, returning
#' plain numeric values. If the conversion fails (e.g., incompatible units),
#' a warning is issued and the original vector is returned unchanged.
#'
#' @param x A numeric vector, typically already associated with units or
#'   intended to be interpreted as having units.
#' @param from Character scalar giving the source unit.
#' @param to Character scalar giving the target unit.
#'
#' @return A numeric vector. On success, the values are converted from
#'   \code{from} to \code{to}. On failure, the original \code{x} is returned.
#'
#' @details
#' The function wraps \code{units::set_units()} and \code{units::drop_units()}
#' inside a \code{tryCatch()} block. This ensures that unit conversion errors
#' do not interrupt data processing pipelines.
#'
#' @examples
#' library(units)
#' x <- set_units(1:3, "m")
#' .convert_col(x, "m", "cm")
#'
#' # incompatible conversion → warning, original values returned
#' .convert_col(x, "m", "seconds")
#'
#' @seealso \code{units::set_units()}, \code{units::drop_units()}
#'
#' @keywords internal
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
