# ---- Constants --------------------------------------------------------

.met_keys <- c("site", "TIMESTAMP", "var_name")

.met_long_cols <- c(
  .met_keys,
  "value",
  "qc",
  "validator",
  "comment",
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
  required <- .met_long_cols
  missing <- setdiff(required, names(dt))
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

  # Drop rows where site is NA — these are padding/join artefacts with no
  # useful data; keeping them creates spurious NA facets in plots.
  mm$dt <- mm$dt[!is.na(site)]

  data.table::setkeyv(mm$dt, .met_keys)
  data.table::setkeyv(mm$dt_meta, c("site", "name_dt"))

  mm$dt[
    mm$dt_meta,
    `:=`(type = type, name_icos = name_icos),
    on = c(var_name = "name_dt")
  ]

  if (!is.null(mm$dt_qc)) {
    .assert_is_dt(mm$dt_qc)
    dt_qc <- if (!"var_name" %in% names(mm$dt_qc)) {
      # Backward compat: old wide format (one column per variable for qc codes)
      id_cols <- intersect(c("site", time_name, "validator"), names(mm$dt_qc))
      dt_qc_melt <- data.table::melt(
        mm$dt_qc,
        id.vars = id_cols,
        variable.name = "var_name",
        value.name = "qc"
      )
      if (!"validator" %in% names(dt_qc_melt)) {
        dt_qc_melt[, validator := NA_character_]
      }
      dt_qc_melt[, comment := NA_character_]
      dt_qc_melt
    } else {
      mm$dt_qc
    }
    if (time_name != "TIMESTAMP" && time_name %in% names(dt_qc)) {
      data.table::setnames(dt_qc, time_name, "TIMESTAMP")
    }
    if (!"comment" %in% names(dt_qc)) {
      dt_qc[, comment := NA_character_]
    }
    dt_qc[, var_name := as.character(var_name)]
    mm$dt[
      dt_qc,
      `:=`(qc = qc, validator = validator, comment = comment),
      on = .met_keys
    ]
    mm$dt_qc <- NULL
  } else {
    mm$dt[, `:=`(
      qc = NA_real_,
      validator = NA_character_,
      comment = NA_character_
    )]
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
  # Old long-format fixtures may lack the comment column; add it before asserting.
  if (!"comment" %in% names(mm$dt)) {
    mm$dt[, comment := NA_character_]
  }
  .assert_is_metamet_long(mm$dt)

  dt_long <- data.table::copy(mm$dt)

  mm$dt <- data.table::dcast(
    dt_long,
    site + TIMESTAMP ~ var_name,
    value.var = "value"
  )

  dt_qc_long <- dt_long[
    !is.na(qc),
    .(
      site,
      TIMESTAMP,
      var_name = as.character(var_name),
      qc,
      validator,
      comment
    )
  ]
  mm$dt_qc <- if (nrow(dt_qc_long)) dt_qc_long else NULL

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

  # dcast always produces a TIMESTAMP column; normalise dt_meta to match.
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

#' Combine metamet objects or data-table lists into a single metamet object
#'
#' Two calling conventions are supported:
#'
#' **List of `metamet` objects** (preferred):
#' `rbind_metamet(list(mm1, mm2, mm3))`
#' All objects must have the same `format` attribute (`"wide"` or `"long"`).
#' For **long-format** objects the combination is a plain `rbindlist` — rows
#' from different sites never share the same
#' `(site, TIMESTAMP, var_name)` key, so no join logic is needed.
#' For **wide-format** objects `Reduce(join, l_mm)` is used, which applies
#' `power_full_join` with `coalesce_yx` for correct handling of overlapping
#' timestamps within a site.
#'
#' **Legacy four-argument form** (kept for backward compatibility):
#' `rbind_metamet(mm, l_dt, l_dt_meta, l_dt_site)`
#' Row-binds lists of individual data tables into an existing `metamet` object.
#'
#' @param mm_or_list Either a list of `metamet` objects (new interface) or a
#'   single `metamet` object to receive combined tables (legacy interface).
#' @param l_dt (Legacy) A list of data tables to row-bind into `mm$dt`.
#' @param l_dt_meta (Legacy) A list of metadata tables to row-bind into
#'   `mm$dt_meta`.
#' @param l_dt_site (Legacy) A list of site tables to row-bind into
#'   `mm$dt_site`.
#'
#' @return A `metamet` object with combined data.
#'
#' @examples
#' \dontrun{
#' # New interface — list of metamet objects:
#' mm_all <- rbind_metamet(list(mm_amo_qc, mm_buc_qc, mm_ebu_qc, mm_whm_qc))
#'
#' # Legacy interface:
#' mm <- rbind_metamet(
#'   mm,
#'   list(dt1, dt2), list(meta1, meta2), list(site1, site2)
#' )
#' }
#'
#' @export
rbind_metamet <- function(mm_or_list, l_dt, l_dt_meta, l_dt_site) {
  # ---- New interface: list of metamet objects --------------------------------
  if (
    is.list(mm_or_list) &&
      length(mm_or_list) > 0L &&
      inherits(mm_or_list[[1L]], "metamet")
  ) {
    return(.rbind_metamet_list(mm_or_list))
  }
  # ---- Legacy interface: (mm, l_dt, l_dt_meta, l_dt_site) ------------------
  mm <- mm_or_list
  mm$dt <- data.table::rbindlist(l_dt, fill = TRUE)
  mm$dt_meta <- data.table::rbindlist(l_dt_meta, fill = TRUE)
  mm$dt_site <- data.table::rbindlist(l_dt_site, fill = TRUE)
  mm
}

# ---- Internal worker -------------------------------------------------------

.rbind_metamet_list <- function(l_mm) {
  l_mm <- Filter(Negate(is.null), l_mm)
  if (length(l_mm) == 0L) return(NULL)
  if (length(l_mm) == 1L) return(l_mm[[1L]])

  # Require consistent format across all objects
  v_fmts <- vapply(l_mm, function(mm) {
    fmt <- attr(mm, "format", exact = TRUE)
    if (is.null(fmt)) "wide" else fmt
  }, character(1L))

  if (length(unique(v_fmts)) != 1L) {
    stop(
      "All metamet objects must have the same format ('wide' or 'long'); ",
      "found: ", paste(sort(unique(v_fmts)), collapse = ", "),
      call. = FALSE
    )
  }
  fmt <- v_fmts[1L]

  .rbind_slot <- function(slot) {
    l <- Filter(Negate(is.null), lapply(l_mm, `[[`, slot))
    if (length(l) == 0L) return(NULL)
    data.table::rbindlist(l, fill = TRUE, use.names = TRUE)
  }

  if (fmt == "long") {
    # Long format: pure union via rbindlist — no key overlap possible across
    # sites, so power_full_join overhead is unnecessary.
    mm_out <- l_mm[[1L]]
    mm_out$dt <- .rbind_slot("dt")
    mm_out$dt_meta <- unique(.rbind_slot("dt_meta"), by = "name_dt")
    mm_out$dt_site <- .rbind_slot("dt_site")
    if (!is.null(l_mm[[1L]]$dt_qc)) {
      mm_out$dt_qc <- .rbind_slot("dt_qc")
    }
    attr(mm_out, "format") <- "long"
    return(mm_out)
  }

  # Wide format: use join() for coalesce semantics on overlapping timestamps
  Reduce(join, l_mm)
}

# ---- Format coercion helpers -------------------------------------------

# Reshape to long if not already long. Objects with no format attribute are
# treated as wide (same assumption as metamet_reshape).
.ensure_long <- function(mm) {
  if (!identical(attr(mm, "format"), "long")) {
    mm <- metamet_reshape(mm, "long")
  }

  if (!"comment" %in% names(mm$dt)) {
    mm$dt[, comment := NA_character_]
  }

  mm
}

# Reshape to wide if not already wide.
.ensure_wide <- function(mm) {
  if (!identical(attr(mm, "format"), "wide")) {
    mm <- metamet_reshape(mm, "wide")
  }
  mm
}
