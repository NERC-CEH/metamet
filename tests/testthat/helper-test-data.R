#' Build a minimal metamet object for testing
#'
#' @param include_qc Logical. Include dt_qc slot (default TRUE).
#' @param include_ref Logical. Include dt_ref slot (default TRUE).
make_test_metamet <- function(include_qc = TRUE, include_ref = TRUE) {
  ts <- as.POSIXct("2024-01-01 00:00:00", tz = "GMT")

  dt <- data.table::data.table(
    site      = "A",
    TIMESTAMP = ts,
    temp      = 10.0,
    flux      = 3.0
  )

  dt_meta <- data.table::data.table(
    site      = "A",
    name_dt   = c("temp", "flux"),
    type      = c("climate", "flux"),
    name_icos = c("TA", "NEE")
  )

  dt_site <- data.table::data.table(
    site = "A",
    lat  = 55.8,
    lon  = -3.2
  )

  dt_qc <- if (include_qc) {
    data.table::data.table(
      site      = "A",
      TIMESTAMP = ts,
      validator = "auto",
      temp      = 0,
      flux      = 1
    )
  } else {
    NULL
  }

  dt_ref <- if (include_ref) {
    data.table::data.table(
      site      = "A",
      TIMESTAMP = ts,
      temp      = 9.8,
      flux      = 2.9
    )
  } else {
    NULL
  }

  mm <- list(
    dt      = dt,
    dt_meta = dt_meta,
    dt_site = dt_site,
    dt_qc   = dt_qc,
    dt_ref  = dt_ref
  )
  class(mm) <- c("metamet", "list")
  attr(mm, "format") <- "wide"
  mm
}
