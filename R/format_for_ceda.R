#' Format the data for submission to CEDA
#'
#' @param mm A metamet object.
#' @param v_names A character vector of column names
#'   that should be included in the new data frame.
#'
#' @return A new data frame with the selected columns from \code{dt} and
#'   \code{dt_qc}.
#'
##' @keywords internal
##' @noRd
format_for_ceda <- function(
  mm,
  v_names = c(
    "TA_4_1_1",
    "RH_4_1_1",
    "PA_4_1_1",
    "RG_4_1_0",
    "PPFD_IN_4_1_1",
    "RN_5_1_1",
    "P_12_1_1",
    "TS",
    "G",
    "SWC",
    "WS_6_1_1",
    "WD_6_1_1",
    "D_SNOW",
    "WTD"
  )
) {
  # Get the qc names by adding "_qc" to the v_names
  v_names_qc <- paste0(v_names, "_qc")

  # Select the columns from dt_qc with the names in v_names_qc
  mm$dt_qc <- setNames(mm$dt_qc[, ..v_names], v_names_qc)

  # Create a new data frame by binding the selected columns from dt and dt_qc
  dt <- cbind(mm$dt[, .(DATECT)], mm$dt[, ..v_names], mm$dt_qc)

  # Return the new data frame
  return(dt)
}
