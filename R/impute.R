##' Impute missing values in meteorological data
##'
##' Imputes missing or flagged values in one or more variables of a \code{metamet} object
##' using various methods. The function supports regression-based imputation, time-series
##' smoothing (GAM), substitution from reference data (ERA5), and physical constraints.
##' All imputed values are flagged in the quality control (QC) table.
##'
##' @param v_y Character vector of variable names (as quoted strings) to impute.
##'   If \code{NULL} (default), all variables in the data table except site and time
##'   are selected for imputation.
##' @param mm A \code{metamet} object containing observation data (\code{dt}),
##'   quality control codes (\code{dt_qc}), and optional reference data (\code{dt_ref}).
##' @param method Character string specifying the imputation method to use.
##'   If \code{NULL} (default), the method is read from the \code{imputation_method}
##'   column in \code{dt_meta}. Supported methods:
##'   \describe{
##'     \item{\code{"time"}}{Generalized additive model (GAM) with smoothing splines
##'       over time and hour of day. Suitable for variables with strong diurnal/seasonal patterns.}
##'     \item{\code{"regn"}}{Linear regression against covariate \code{x}. Fits a model
##'       excluding missing values, then predicts.}
##'     \item{\code{"era5"}}{Substitute ERA5 reanalysis data from \code{dt_ref}.
##'       If fewer than \code{n_min} observations, replaces directly without fitting.}
##'     \item{\code{"noneg"}}{Replace negative values with zero (physical constraint).}
##'     \item{\code{"nightzero"}}{Replace nighttime values with zero. Uses site coordinates
##'       (\code{lat}, \code{lon}) to identify day/night via \code{openair::cutData()}.}
##'     \item{\code{"zero"}}{Replace all missing/flagged values with zero.}
##'   }
##' @param qc_tokeep Integer QC code(s) indicating "good" or "raw" data to retain unchanged.
##'   Default \code{0}. Data with QC codes not in \code{qc_tokeep} are candidates for imputation.
##' @param selection Logical. If \code{TRUE} (default), applies selection filtering
##'   from metadata. If \code{FALSE}, imputes all values matching \code{qc_tokeep} criteria.
##' @param k Integer. Smoothing basis dimension for GAM in "time" method (default: 40).
##'   Automatically reduced if data is sparse. Controls temporal smoothness.
##' @param fit Logical. If \code{TRUE} (default), fits regression/GAM models for
##'   imputation. If \code{FALSE}, uses direct substitution (useful with "era5" method
##'   and minimal data).
##' @param n_min Integer. Minimum number of non-missing observations required to fit
##'   a model (default: 10). If fewer observations exist, "time" and "regn" methods skip
##'   imputation; "era5" method switches to direct substitution.
##' @param x Optional. Character string naming a covariate column in the data table
##'   for use in "regn" method. For example, \code{x = "PPFD_IN"} to regress against
##'   photosynthetic photon flux density.
##' @param lat Numeric. Latitude of the site in degrees (default: \code{55.792}).
##'   Used by "nightzero" method to calculate sunrise/sunset times.
##' @param lon Numeric. Longitude of the site in degrees (default: \code{-3.243}).
##'   Used by "nightzero" method to calculate sunrise/sunset times.
##' @param plot_graph Logical. If \code{TRUE} (default), generates diagnostic plots
##'   showing observations, reference data (if available), and QC flags. Saves PNG files
##'   to the \code{output/} directory with naming convention \code{plot_<variable>_<method>.png}.
##'
##' @return The input \code{metamet} object \code{mm}, invisibly returned with updated
##'   \code{dt} (imputed values) and \code{dt_qc} (new QC codes for imputed points).
##'
##' @details
##' **Imputation Process:**
##' The function iterates over each variable in \code{v_y}. For each variable:
##' 1. Determines the imputation method (from parameter or metadata).
##' 2. Identifies which rows to impute based on QC codes and \code{selection} flag.
##' 3. Applies the selected imputation method.
##' 4. Updates the QC table to flag imputed values.
##' 5. Optionally generates a diagnostic plot.
##'
##' **Minimum Data Handling:**
##' If fewer than \code{n_min} non-missing observations exist:
##' - "time" and "regn" methods skip the variable (no imputation).
##' - "era5" method switches to direct substitution (\code{fit = FALSE}).
##' - Other methods ("zero", "noneg", "nightzero") are unaffected.
##'
##' **Data Reference:**
##' The function requires a metadata table (\code{dt_meta}) describing variables,
##' and optionally a reference table (\code{dt_ref}) for ERA5 or other reanalysis data.
##' Ensure these are present in the \code{metamet} object.
##'
##' **Plotting:**
##' Diagnostic plots overlay observations (colored by QC code), reference data (black line),
##' and imputed points. Useful for validating imputation results and identifying issues.
##'
##' @seealso
##'   \code{\link{metamet}} for object structure
##'   \code{\link{add_era5}} for adding ERA5 reference data
##'   \code{\link{time_average}} for temporal aggregation
##'
##' @examples
##' \dontrun{
##' # Example 1: Impute from metadata method specification
##' mm <- impute(
##'   v_y = "SW_IN",
##'   mm = mm,
##'   qc_tokeep = 0,
##'   plot_graph = TRUE
##' )
##'
##' # Example 2: Impute using ERA5 data, multiple variables
##' mm <- impute(
##'   v_y = c("TA", "RH"),
##'   mm = mm,
##'   method = "era5",
##'   fit = FALSE,
##'   plot_graph = TRUE
##' )
##'
##' # Example 3: Regression imputation with covariate
##' mm <- impute(
##'   v_y = "SW_IN",
##'   mm = mm,
##'   method = "regn",
##'   x = "PPFD_IN",
##'   fit = TRUE,
##'   n_min = 15
##' )
##' }
##'
##' @keywords univar na
##' @rdname impute
##' @export
impute <- function(
  v_y = NULL,
  mm,
  method = NULL,
  qc_tokeep = 0,
  selection = TRUE,
  k = 40,
  fit = TRUE,
  n_min = 10,
  x = NULL,
  lat = 55.792,
  lon = -3.243,
  plot_graph = TRUE
) {
  # if not given as an argument, use that specified in dt_meta
  if (is.null(method)) {
    use_method_from_meta <- TRUE
  } else {
    use_method_from_meta <- FALSE
  }

  time_name <- mm$dt_meta[type == "time", name_dt]
  v_cols_to_exclude <- c("site", time_name)
  if (is.null(v_y)) {
    v_y <- names(mm$dt)[names(mm$dt) %!in% v_cols_to_exclude]
  }

  # these 3 lines not needed if we prefix with mm$ throughout
  dt <- mm$dt
  dt_qc <- mm$dt_qc
  dt_ref <- mm$dt_ref

  for (y in v_y) {
    print(paste("Getting ready to impute", y))
    if (use_method_from_meta) {
      method <- mm$dt_meta[name_dt == y, imputation_method]
    }
    # method <- match.arg(method, df_method$method)
    method <- df_method$method[match(method, df_method$method)]
    # get the qc code for the selected method
    qc <- df_method$qc[match(method, df_method$method)]
    print(paste("using method", qc, method))

    # how many non-missing data are there?
    n_data <- sum(!is.na(dt[, get(y)]))
    # with very few/no data, just replace with era5 data rather than trying to fit a regression
    if (n_data <= n_min && method == "era5") {
      print(paste("Too few data to fit regression; using ERA5 data directly"))
      fit <- FALSE
    }
    # these methods don't work with very few/no data
    if (n_data <= n_min && (method == "time" || method == "regn")) {
      print(paste("Not enough data to impute", y))
      next
    }

    # qc_tokeep typically set to 0, so this selects any other value (missing or imputed)
    # selection optionally adds those selected in the metdb app ggiraph plots
    # isel = TRUE  = missing (AND selected), to be imputed
    # isel = FALSE = raw
    i_sel <- dt_qc[, get(y)] %!in% qc_tokeep & selection
    if (method == "noneg") {
      i_sel <- i_sel & dt[, y] < 0
    }
    if (method == "nightzero") {
      dt$date <- dt[, ..date_field] # needs to be called "date" for openair functions
      dt <- openair::cutData(
        dt,
        type = "daylight",
        latitude = lat,
        longitude = lon
      )
      i_sel <- i_sel & dt$daylight == "nighttime"
      dt$daylight <- NULL
      # if date is not the original variable name, delete it - we don't want an extra column
      if (time_name != "date") dt$date <- NULL
    }

    # calculate replacement values depending on the method
    # if a constant zero
    if (method == "nightzero" | method == "noneg" | method == "zero") {
      dt[i_sel, eval(y) := 0]
    } else if (method == "time") {
      if (k > n_data / 4) {
        k <- as.integer(n_data / 4)
      }
      v_date <- dt[, get(time_name)]
      datect_num <- as.numeric(v_date) ## !dt_qry$
      hour <- as.POSIXlt(v_date)$hour

      # cannot include hour if daily avg, as no spread in hours
      m <- mgcv::gam(
        dt[, get(y)] ~
          s(datect_num, k = k, bs = "cr"),
        # s(yday, k = k_yday, bs = "cr") +
        # s(hour, k = -1, bs = "cc"),
        na.action = na.exclude #, data = dt
      )
      v_pred <- predict(m, newdata = data.frame(datect_num, hour))
      dt[i_sel, y] <- v_pred[i_sel]
    } else if (method == "regn" || method == "era5") {
      if (method == "era5") {
        v_x <- dt_ref[, get(y)] # use ERA5 data
      } else {
        v_x <- dt[, get(x)] # use x variable in the obs data
      }
      if (fit) {
        dtt <- data.frame(y = dt[, get(y)], x = v_x)
        # exclude indices i_sel i.e. do not fit to those we are replacing
        dtt$y[i_sel] <- NA
        m <- lm(y ~ x, data = dtt, na.action = na.exclude)
        v_pred <- predict(m, newdata = dtt)
      } else {
        # or just replace y with x
        v_pred <- v_x
      }
      dt[i_sel, eval(y) := v_pred[i_sel]]
    }

    # add code for each replaced value in the qc dt
    dt_qc[i_sel, eval(y) := qc]

    if (plot_graph) {
      dtt <- data.table(
        date = dt[, get(time_name)],
        y = dt[, get(y)],
        qc = dt_qc[, get(y)]
      )
      p <- ggplot(dtt, aes(date, y))
      p <- p +
        geom_line(
          data = dt_ref,
          aes(x = dt_ref[, get(time_name)], y = dt_ref[, get(y)]),
          colour = "black"
        )
      # }
      p <- p + geom_point(aes(y = y, colour = factor(qc)), size = 1) + ylab(y)
      fs::dir_create("output")
      fname <- paste0("plot_", y, "_", method, ".png")
      ggsave(p, filename = fname)
    }
  }
  return(mm)
}
