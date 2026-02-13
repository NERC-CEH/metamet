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
  y,
  mm,
  method = "era5",
  qc_tokeep = 0,
  selection = TRUE,
  date_field = "DATECT",
  k = 40,
  fit = TRUE,
  n_min = 10,
  x = NULL,
  lat = 55.792,
  lon = -3.243,
  plot_graph = TRUE
) {
  print(paste("Standing by to impute", y))

  method <- match.arg(method, df_method$method)
  # get the qc code for the selected method
  qc <- df_method$qc[match(method, df_method$method)]

  dt <- mm$dt
  dt_qc <- mm$dt_qc
  dt_era5 <- mm$dt_ref

  # how many non-missing data are there?
  n_data <- sum(!is.na(dt[, ..y][[1]]))
  # with very few/no data, just replace with era5 data rather than trying to fit a regression
  if (n_data <= n_min && method == "era5") {
    print(paste("Too few data to fit regression; using ERA5 data directly"))
    fit <- FALSE
  }
  # these methods don't work with very few/no data
  if (n_data <= n_min && (method == "time" || method == "regn")) {
    return(mm)
  }

  # dt_qc[, y][which(i_sel)]
  # table(i_sel)
  # table(dt[, y] < 0)
  # table(dt_qc[, y])
  # indices of values to change
  # default
  # qc_tokeep typically set to 0, so this selects any other value (missing or imputed)
  # selection optionally adds those selected in the metdb app ggiraph plots
  # isel = TRUE  = missing, imputed (AND selected)
  # isel = FALSE = raw
  i_sel <- dt_qc[, ..y][[1]] %!in% qc_tokeep & selection
  if (method == "noneg") {
    i_sel <- i_sel & dt[, y] < 0
  }
  if (method == "nightzero") {
    dt$date <- dt[, ..date_field] # needs to be called "date" for openair functions
    dt <- cutData(dt, type = "daylight", latitude = lat, longitude = lon)
    i_sel <- i_sel & dt$daylight == "nighttime"
    dt$daylight <- NULL
    # if date is not the original variable name, delete it - we don't want an extra column
    if (date_field != "date") dt$date <- NULL
  }

  # calculate replacement values depending on the method
  # if a constant zero
  if (method == "nightzero" | method == "noneg" | method == "zero") {
    dt[i_sel, eval(y) := 0]
  } else if (method == "time") {
    if (k > n_data / 4) {
      k <- as.integer(n_data / 4)
    }
    v_date <- dt[, ..date_field][[1]]
    datect_num <- as.numeric(v_date) ## !dt_qry$
    hour <- as.POSIXlt(v_date)$hour
    # yday <- as.POSIXlt(v_date)$yday
    # n_yday <- length(unique(yday))
    # k_yday <- as.integer(n_yday / 2)

    m <- gam(
      dt[, ..y][[1]] ~
        s(datect_num, k = k, bs = "cr") +
        # s(yday, k = k_yday, bs = "cr") +
        s(hour, k = -1, bs = "cc"),
      na.action = na.exclude #, data = dt
    )
    v_pred <- predict(m, newdata = data.frame(datect_num, hour))
    dt[i_sel, y] <- v_pred[i_sel]
  } else if (method == "regn" || method == "era5") {
    if (method == "era5") {
      v_x <- dt_era5[, ..y][[1]] # use ERA5 data
    } else {
      v_x <- dt[, ..x][[1]] # use x variable in the obs data
    }
    if (fit) {
      dtt <- data.frame(y = dt[, ..y][[1]], x = v_x)
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
      date = dt[, ..date_field][[1]],
      y = dt[, ..y][[1]],
      qc = dt_qc[, ..y][[1]]
    )
    p <- ggplot(dtt, aes(date, y))
    p <- p +
      geom_line(
        data = dt_era5,
        aes(x = dt_era5[, ..date_field][[1]], y = dt_era5[, ..y][[1]]),
        colour = "black"
      )
    # }
    p <- p + geom_point(aes(y = y, colour = factor(qc)), size = 1) + ylab(y)
    fname <- paste0("plot_", y, "_", method, ".png")
    ggsave(p, filename = here("output", fname))
  }
  return(mm)
}
