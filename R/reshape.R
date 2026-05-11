# ---- Constants --------------------------------------------------------

.met_keys <- c("site", "TIMESTAMP", "var_name")

.met_long_cols <- c(
  .met_keys,
  "value",
  "qc",
  "validator",
  "ref",
  "type",
  "name_icos"
)

# ---- Assertions -------------------------------------------------------

.assert_is_dt <- function(x, name = deparse(substitute(x))) {
  if (!data.table::is.data.table(x)) {
    stop(name, " must be a data.table", call. = FALSE)
  }
}

.assert_is_metamet_long <- function(dt) {
  .assert_is_dt(dt)
  missing <- setdiff(.met_long_cols, names(dt))
  if (length(missing)) {
    stop(
      "mm$dt is not a valid long-format metamet table; missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.assert_unique_keys <- function(dt, keys) {
  bad <- dt[, .N, by = keys][N > 1]
  if (nrow(bad)) {
    stop(
      "Non-unique keys detected for: ",
      paste(keys, collapse = ", "),
      call. = FALSE
    )
  }
}

# ---- Internal transforms ----------------------------------------------

#' @keywords internal
metamet_wide_to_long <- function(mm) {
  .assert_is_dt(mm$dt)
  .assert_is_dt(mm$dt_meta)

  time_name <- unique(mm$dt_meta[type == "time", name_dt])
  if (length(time_name) != 1) {
    stop(
      "Expected exactly one time variable in dt_meta; found: ",
      length(time_name),
      call. = FALSE
    )
  }

  mm$dt <- data.table::melt(
    mm$dt,
    id.vars = c("site", time_name),
    variable.name = "var_name"
  )
  if (time_name != "TIMESTAMP") {
    data.table::setnames(mm$dt, time_name, "TIMESTAMP")
  }

  data.table::setkeyv(mm$dt, .met_keys)
  data.table::setkeyv(mm$dt_meta, c("site", "name_dt"))

  mm$dt[
    mm$dt_meta,
    `:=`(type = type, name_icos = name_icos),
    on = c(var_name = "name_dt")
  ]

  if (!is.null(mm$dt_qc)) {
    .assert_is_dt(mm$dt_qc)
    dt_qc <- data.table::melt(
      mm$dt_qc,
      id.vars = c("site", time_name, "validator"),
      variable.name = "var_name",
      value.name = "qc"
    )
    if (time_name != "TIMESTAMP") {
      data.table::setnames(dt_qc, time_name, "TIMESTAMP")
    }
    data.table::setkeyv(dt_qc, .met_keys)
    mm$dt[dt_qc, `:=`(qc = qc, validator = validator)]
    mm$dt_qc <- NULL
  } else {
    mm$dt[, `:=`(qc = NA_real_, validator = NA_character_)]
  }

  if (!is.null(mm$dt_ref)) {
    .assert_is_dt(mm$dt_ref)
    dt_ref <- data.table::melt(
      mm$dt_ref,
      id.vars = c("site", time_name),
      variable.name = "var_name",
      value.name = "ref"
    )
    if (time_name != "TIMESTAMP") {
      data.table::setnames(dt_ref, time_name, "TIMESTAMP")
    }
    data.table::setkeyv(dt_ref, .met_keys)
    mm$dt[dt_ref, ref := ref]
    mm$dt_ref <- NULL
  } else {
    mm$dt[, ref := NA_real_]
  }

  .assert_unique_keys(mm$dt, .met_keys)

  mm
}

#' @keywords internal
metamet_long_to_wide <- function(mm) {
  .assert_is_metamet_long(mm$dt)

  dt_long <- data.table::copy(mm$dt)

  mm$dt <- data.table::dcast(
    dt_long,
    site + TIMESTAMP ~ var_name,
    value.var = "value"
  )

  dt_qc_long <- dt_long[!is.na(qc), .(site, TIMESTAMP, var_name, validator, qc)]
  mm$dt_qc <- if (nrow(dt_qc_long)) {
    data.table::dcast(
      dt_qc_long,
      site + TIMESTAMP + validator ~ var_name,
      value.var = "qc"
    )
  } else {
    NULL
  }

  dt_ref_long <- dt_long[!is.na(ref), .(site, TIMESTAMP, var_name, ref)]
  mm$dt_ref <- if (nrow(dt_ref_long)) {
    data.table::dcast(
      dt_ref_long,
      site + TIMESTAMP ~ var_name,
      value.var = "ref"
    )
  } else {
    NULL
  }

  # dcast always produces a TIMESTAMP column; normalise dt_meta to match
  mm$dt_meta[type == "time", name_dt := "TIMESTAMP"]

  mm
}

# ---- Public dispatcher ------------------------------------------------

#' Reshape a metamet object between wide and long format
#'
#' Converts a `metamet` object to the target format. Wide format stores one
#' column per variable (the default on construction); long format stores one
#' row per variable with `qc`, `ref`, and metadata merged into a single table.
#'
#' @param mm A `metamet` object. Should have a `"format"` attribute of
#'   `"wide"` or `"long"` (set automatically on construction). Objects loaded
#'   from older `.rds` files without this attribute are assumed to be wide.
#' @param format Target format: `"wide"` or `"long"`.
#'
#' @return The reshaped `metamet` object with the `"format"` attribute updated.
#'
#' @examples
#' \dontrun{
#' mm_long <- metamet_reshape(mm, "long")
#' mm_wide <- metamet_reshape(mm_long, "wide")
#' }
#'
#' @export
metamet_reshape <- function(mm, format = c("wide", "long")) {
  format <- match.arg(format)

  current <- attr(mm, "format", exact = TRUE)
  if (is.null(current)) {
    warning(
      "metamet object has no format attribute; assuming 'wide'",
      call. = FALSE
    )
    current <- "wide"
  }

  if (current == format) {
    return(mm)
  }

  mm <- if (current == "wide") {
    metamet_wide_to_long(mm)
  } else {
    metamet_long_to_wide(mm)
  }

  attr(mm, "format") <- format
  mm
}

# ---- Combining multiple metamet objects --------------------------------

#' Combine lists of metamet data tables into a single metamet object
#'
#' Row-binds lists of data tables into an existing `metamet` object. Typically
#' used to merge data from multiple sites or time periods.
#'
#' @param mm A `metamet` object to receive the combined tables.
#' @param l_dt A list of data tables to row-bind into `mm$dt`.
#' @param l_dt_meta A list of metadata tables to row-bind into `mm$dt_meta`.
#' @param l_dt_site A list of site tables to row-bind into `mm$dt_site`.
#'
#' @return The modified `metamet` object.
#'
#' @examples
#' \dontrun{
#' mm <- rbind_metamet(mm, list(dt1, dt2), list(meta1, meta2), list(site1, site2))
#' }
#'
#' @export
rbind_metamet <- function(mm, l_dt, l_dt_meta, l_dt_site) {
  mm$dt <- data.table::rbindlist(l_dt, fill = TRUE)
  mm$dt_meta <- data.table::rbindlist(l_dt_meta, fill = TRUE)
  mm$dt_site <- data.table::rbindlist(l_dt_site, fill = TRUE)
  mm
}
