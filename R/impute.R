#' @title impute
#' @description Impute missing values using various methods
#' @param y Response variable with missing values to be replaced
#'   (variable name as a "quoted string")
#' @param x Covariate to be used (name of a variable in the same data frame as
#'   a "quoted string")
#' @param mm metamet object containing data and qc codes.
#' @param qc qc code to denote values imputed by this function, Default: 3
#' @param fit Whether to fit a linear model or directly replace missing y with
#'   x values, Default: TRUE
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()) {
#'  #EXAMPLE1
#' mm <- list(df = df, df_qc = df_qc)
#' mm <- impute(y = "SW_IN", x = "PPFD_IN",  mm)
#'  }
#' }
#' @rdname impute
#' @export
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

      m <- gam(
        dt[, get(y)] ~
          s(datect_num, k = k, bs = "cr") +
          # s(yday, k = k_yday, bs = "cr") +
          s(hour, k = -1, bs = "cc"),
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
      fname <- paste0("plot_", y, "_", method, ".png")
      ggsave(p, filename = here("output", fname))
    }
  }
  return(mm)
}
