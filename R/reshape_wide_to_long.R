reshape_wide_to_long <- function(mm) {
  mm$dt <- melt(
    mm$dt,
    id.vars = c("site", "TIMESTAMP"),
    variable.name = "var_name"
  )
  dt_qc <- melt(
    mm$dt_qc,
    id.vars = c("site", "TIMESTAMP", "validator"),
    variable.name = "var_name",
    value.name = "qc"
  )
  dt_ref <- melt(
    mm$dt_ref,
    id.vars = c("site", "TIMESTAMP"),
    variable.name = "var_name",
    value.name = "ref"
  )

  key_cols <- c("site", "TIMESTAMP", "var_name")
  setkeyv(mm$dt, key_cols)
  setkeyv(dt_qc, key_cols)
  setkeyv(dt_ref, key_cols)
  setkeyv(mm$dt_meta, c("site", "name_dt"))

  mm$dt <- mm$dt[
    mm$dt_meta,
    `:=`(type = type, name_icos = name_icos),
    on = c(var_name = "name_dt")
  ]
  mm$dt <- mm$dt[dt_qc, `:=`(qc = qc, validator = validator)]
  mm$dt <- mm$dt[dt_ref, ref := ref]

  # return a reformatted metamet object with long format dt containing all info
  # from mm$dt_qc and mm$dt_ref, so these are no longer needed
  mm$dt_qc <- NULL
  mm$dt_ref <- NULL
  return(mm)
}

rbind_metamet <- function(mm, l_dt, l_dt_meta, l_dt_site) {
  mm$dt <- rbindlist(l_dt)
  mm$dt_meta <- rbindlist(l_dt_meta)
  mm$dt_site <- rbindlist(l_dt_site)
  return(mm)
}
