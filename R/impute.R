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
##' @param is_selected Logical. If \code{TRUE} (default), applies imputation to all missing values
##'   If called from the shiny app, this will be a vector showing which points were selected by the user.
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
##' 2. Identifies which rows to impute based on QC codes and \code{is_selected} flag.
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
  row_selected = TRUE,
  k = 40,
  fit = TRUE,
  n_min = 10,
  x = NULL,
  lat = 55.792,
  lon = -3.243,
  plot_graph = TRUE
) {
  if (!identical(attr(mm, "format"), "long")) {
    stop(
      "impute() requires a long-format metamet object. ",
      "Call metamet_reshape(mm, 'long') first.",
      call. = FALSE
    )
  }

  # if not given as an argument, use that specified in dt_meta
  if (is.null(method)) {
    use_method_from_meta <- TRUE
  } else {
    use_method_from_meta <- FALSE
  }

  time_name <- unique(mm$dt_meta[type == "time", name_dt])
  if (length(time_name) > 1) {
    stop("More than one time variable found in data.")
  }
  v_cols_to_exclude <- c("site", time_name)
  if (is.null(v_y)) {
    v_y <- unique(mm$dt$name_icos)
  }

  # this line not needed if we prefix with mm$ throughout
  dt <- mm$dt

  for (y in v_y) {
    print(paste("Getting ready to impute", y))
    if (use_method_from_meta) {
      method <- mm$dt_meta[name_icos == y, imputation_method][1]
    }
    method <- df_method$method[match(method, df_method$method)]
    if (is.na(method)) {
      warning(
        "No valid imputation method found for '",
        y,
        "'. ",
        "Check that '",
        y,
        "' appears in dt_meta$name_icos (not dt_meta$name_dt). ",
        "Skipping."
      )
      next
    }
    # get the qc code for the selected method
    qc <- df_method$qc[match(method, df_method$method)]
    print(paste("using method", qc, method))

    # qc_tokeep typically set to 0, so this selects any other value (missing or imputed)
    # is_selected optionally adds those selected in the metqc app ggiraph plots
    # by default, this selects all points marked as missing because the only
    # qc_tokeep is the raw data. Run interactively in the app, this applies only
    # to those selected with the additional condition "is_selected".
    if (isTRUE(row_selected)) {
      dt[name_icos == y, is_selected := TRUE]
    } else {
      dt[name_icos == y, is_selected := row_name %in% row_selected]
    }
    dt[name_icos == y, is_selected := qc %!in% qc_tokeep & is_selected]

    if (method == "noneg") {
      # only previously selected values which are negative stay selected
      dt[, is_selected := y == name_icos & is_selected & value < 0]
    }
    if (method == "nightzero") {
      # Pass only a date column to cutData so that dt is never reassigned
      # (reassignment breaks the by-reference link between dt and mm$dt).
      dt_daylight <- openair::cutData(
        data.frame(date = dt[[time_name]]),
        type = "daylight",
        latitude = lat,
        longitude = lon
      )
      dt[, daylight := dt_daylight$daylight]
      # only previously selected values which are in night-time stay selected
      dt[,
        is_selected := y == name_icos & is_selected & daylight == "nighttime"
      ]
      dt[, daylight := NULL]
    }

    # calculate replacement values depending on the method
    if (method == "nightzero" | method == "noneg" | method == "zero") {
      dt[is_selected & y == name_icos, value := 0]
      dt[y == name_icos & is_selected == TRUE, qc := ..qc]
    } else {
      # model-fitting methods: fit a separate model per replicate (var_name)
      v_var_names_selected <- unique(dt[
        name_icos == y & is_selected == TRUE,
        var_name
      ])

      for (vn in v_var_names_selected) {
        n_data_vn <- sum(!is.na(dt[name_icos == y & var_name == vn, value]))
        fit_vn <- fit
        k_vn <- k

        if (n_data_vn <= n_min && method == "era5") {
          print(paste(
            "Too few data to fit regression; using ERA5 data directly for",
            vn
          ))
          fit_vn <- FALSE
        }
        if (n_data_vn <= n_min && (method == "time" || method == "regn")) {
          print(paste("Not enough data to impute", vn))
          next
        }

        if (method == "time") {
          if (k_vn > n_data_vn / 4) {
            k_vn <- as.integer(n_data_vn / 4)
          }
          v_date <- dt[name_icos == y & var_name == vn, TIMESTAMP]
          datect_num <- as.numeric(v_date)
          hour <- as.POSIXlt(v_date)$hour
          m <- mgcv::gam(
            dt[name_icos == y & var_name == vn, value] ~
              s(datect_num, k = k_vn, bs = "cr"),
            na.action = na.exclude
          )
          dt[
            name_icos == y & var_name == vn,
            pred := predict(m, newdata = data.frame(datect_num, hour))
          ]
          dt[is_selected & name_icos == y & var_name == vn, value := pred]
          dt[, pred := NULL]
        } else if (method == "regn" || method == "era5") {
          if (method == "era5") {
            v_x <- dt[name_icos == y & var_name == vn, ref]
          } else {
            stop("regn method does not currently work with long-format data")
          }
          if (fit_vn) {
            dtt <- data.table(
              y = dt[name_icos == y & var_name == vn, value],
              x = v_x,
              is_selected = dt[name_icos == y & var_name == vn, is_selected]
            )
            dtt[is_selected == TRUE, y := NA]
            m <- lm(y ~ x, data = dtt, na.action = na.exclude)
            v_pred <- predict(m, newdata = dtt)
            dt[
              name_icos == y & var_name == vn & is_selected == TRUE,
              value := v_pred[dtt$is_selected]
            ]
          } else {
            dt[
              name_icos == y & var_name == vn & is_selected == TRUE,
              value := ref
            ]
          }
        }

        dt[name_icos == y & var_name == vn & is_selected == TRUE, qc := ..qc]
      }
    }

    if (plot_graph) {
      dt_plot <- merge(
        dt[name_icos == y],
        data.table::as.data.table(df_method)[, .(qc, method_longname)],
        by = "qc",
        all.x = TRUE
      )
      p <- ggplot(dt_plot, aes(TIMESTAMP, value))
      p <- p + geom_line(aes(y = ref), colour = "black")
      p <- p +
        geom_point(
          aes(colour = method_longname, shape = method_longname),
          size = 1
        ) +
        ylab(y)
      p <- p + facet_grid(~ site * var_name)
      fs::dir_create("output")
      fname <- paste0("plot_", y, "_", method, ".png")
      ggsave(p, filename = fname)
    }
  }
  drop_cols <- intersect(c("is_selected", "pred"), names(dt))
  if (length(drop_cols)) {
    dt[, (drop_cols) := NULL]
  }

  return(mm)
}
