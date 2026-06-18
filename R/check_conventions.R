#' Check dt_meta convention columns for consistency
#'
#' Validates that all sensors of the same variable share identical convention
#' units. For example, all rows with `name_icos == "TA"` must have the same
#' `units_icos`. Called automatically by [new_metamet()]; also available for
#' direct use.
#'
#' @param dt_meta A `data.table` of column-level metadata (the `dt_meta` slot
#'   of a metamet object).
#' @param check_against_standard Logical (default `FALSE`). If `TRUE`, also
#'   warns when a variable's convention units differ from the package registry
#'   (`l_conventions`).
#'
#' @return `dt_meta` invisibly.
#' @export
check_dt_meta <- function(dt_meta, check_against_standard = FALSE) {
  if (is.null(dt_meta)) {
    return(invisible(dt_meta))
  }
  dt <- data.table::as.data.table(dt_meta)

  # Detect which conventions are present (name_icos, name_era5, etc.),
  # excluding name_dt and name_local which are not convention keys.
  v_conventions <- sub(
    "^name_",
    "",
    grep("^name_(?!dt$|local$)", names(dt), perl = TRUE, value = TRUE)
  )

  for (conv in v_conventions) {
    name_col <- paste0("name_", conv)
    units_col <- paste0("units_", conv)
    if (!units_col %in% names(dt)) {
      next
    }
    # Use name_icos as the grouping key for all unit checks: it identifies the
    # physical variable regardless of which convention's units are being checked.
    # Multiple ICOS variables may legitimately share one ERA5 proxy (e.g. ssrd is
    # used for both SW_IN and PPFD_IN), so grouping by name_era5 gives false
    # positives. Fall back to the convention's own name column if name_icos is absent.
    group_col <- if ("name_icos" %in% names(dt)) "name_icos" else name_col
    dt_bad <- dt[
      !is.na(get(group_col)) & !is.na(get(units_col)),
      .(n_units = data.table::uniqueN(get(units_col))),
      by = c(group_col)
    ][n_units > 1L]
    if (nrow(dt_bad) > 0L) {
      warning(
        "dt_meta: inconsistent ",
        units_col,
        " for: ",
        paste(dt_bad[[group_col]], collapse = ", "),
        ". All sensors of the same variable should share the same ",
        "convention units.",
        call. = FALSE
      )
    }
  }

  if (check_against_standard) {
    .check_dt_meta_against_registry(dt, v_conventions)
  }

  invisible(dt_meta)
}

.check_dt_meta_against_registry <- function(dt, v_conventions) {
  # Element names are "df_icos", "df_era5" etc.; strip prefix to get conv key
  v_registry_convs <- sub("^df_", "", names(l_conventions))
  for (conv in intersect(v_registry_convs, v_conventions)) {
    name_col <- paste0("name_", conv)
    units_col <- paste0("units_", conv)
    if (!units_col %in% names(dt)) {
      next
    }
    dt_lkp <- data.table::as.data.table(
      l_conventions[[paste0("df_", conv)]]
    )[, .(name, units)]
    data.table::setnames(dt_lkp, c(name_col, ".std_units"))
    dt_unique <- unique(dt[
      !is.na(get(name_col)) & !is.na(get(units_col)),
      .SD,
      .SDcols = c(name_col, units_col)
    ])
    dt_merged <- dt_lkp[dt_unique, on = name_col]
    dt_wrong <- dt_merged[!is.na(.std_units) & get(units_col) != .std_units]
    if (nrow(dt_wrong) > 0L) {
      warning(
        "dt_meta: ",
        units_col,
        " for ",
        paste(dt_wrong[[name_col]], collapse = ", "),
        " differs from the convention standard in l_conventions.",
        call. = FALSE
      )
    }
  }
}

#' Populate convention columns in dt_meta from the package registry
#'
#' Fills missing `units_<conv>` and `long_name_<conv>` values in `dt_meta`
#' from the package registry (`l_conventions`), for any convention column
#' (`name_icos`, `name_era5`, etc.) that is present.
#'
#' @param dt_meta A `data.table` of column-level metadata.
#' @param overwrite Logical (default `FALSE`). If `FALSE`, only fills cells
#'   that are currently `NA`. If `TRUE`, overwrites all cells with registry
#'   values where a match exists.
#'
#' @return A `data.table` with convention columns populated.
#' @export
populate_convention_cols <- function(dt_meta, overwrite = FALSE) {
  dt <- data.table::copy(data.table::as.data.table(dt_meta))

  # Element names are "df_icos", "df_era5" etc.; strip prefix to get conv key
  for (elem_name in names(l_conventions)) {
    conv <- sub("^df_", "", elem_name)
    name_col <- paste0("name_", conv)
    units_col <- paste0("units_", conv)
    lnname_col <- paste0("long_name_", conv)
    if (!name_col %in% names(dt)) {
      next
    }

    dt_lkp <- data.table::as.data.table(l_conventions[[elem_name]])[, .(
      name,
      units,
      long_name
    )]

    if (!units_col %in% names(dt)) {
      dt[, (units_col) := NA_character_]
    }
    if (!lnname_col %in% names(dt)) {
      dt[, (lnname_col) := NA_character_]
    }

    v_rows_to_fill <- if (overwrite) {
      which(!is.na(dt[[name_col]]))
    } else {
      which(is.na(dt[[units_col]]) & !is.na(dt[[name_col]]))
    }
    if (length(v_rows_to_fill) == 0L) {
      next
    }

    dt_matches <- dt_lkp[
      dt[v_rows_to_fill, .SD, .SDcols = name_col],
      on = c(name = name_col)
    ]
    v_has_match <- !is.na(dt_matches$units)
    if (!any(v_has_match)) {
      next
    }
    dt[
      v_rows_to_fill[v_has_match],
      (units_col) := dt_matches$units[v_has_match]
    ]
    dt[
      v_rows_to_fill[v_has_match],
      (lnname_col) := dt_matches$long_name[v_has_match]
    ]
  }

  dt
}
