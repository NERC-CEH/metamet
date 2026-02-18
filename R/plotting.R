#' Custom plotting function for each variable
#'

#' @title plotting_function
#' @description Creates an interactive girafe plot, whereby the user can select
#'   points with dubious quality and impute new values.
#' @param input_variable The name of the variable within the query data frame to plot.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @keywords internal
#' @noRd
plotting_function <- function(input_variable) {
  df <- data.frame(
    DATECT = mm_qry$dt$DATECT,
    y = mm_qry$dt[, ..input_variable][[1]],
    qc = mm_qry$dt_qc[, ..input_variable][[1]],
    checked = mm_qry$dt$checked
  )

  df <- left_join(df, df_method, by = "qc")

  col_pal <- c(
    '#5b5b5b',
    '#377EB8',
    '#4DAF4A',
    '#984EA3',
    '#00ff7f',
    '#FFFF33',
    '#A65628',
    '#F781BF',
    '#FF7F00'
  )
  names(col_pal) <- levels(df_method$method_longname)

  p1_ggplot <- ggplot(df, aes(DATECT, y)) +
    geom_point_interactive(
      aes(
        data_id = checked,
        tooltip = glue("Timestamp: {DATECT}\nMeasure: {y}"),
        colour = factor(method_longname)
      ),
      size = 3
    ) +
    scale_color_manual(values = col_pal, limits = force) +
    xlab("Date") +
    ylab(paste("Your variable:", input_variable)) +
    ggtitle(paste(input_variable, "time series")) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank()
    )
  p1_girafe <- girafe(code = print(p1_ggplot), width_svg = 10, height_svg = 5)
  p1_girafe <- girafe_options(
    p1_girafe,
    opts_selection(
      type = "multiple",
      css = "fill:#FF3333;stroke:black;"
    ),
    opts_tooltip(zindex = 9999),
    opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"),
    opts_zoom(max = 5)
  )
}

#' Custom plotting function to construct a heatmap calendar
#'

#' @title plot_heatmap_calendar
#' @description Plots a calendar showing who has checked the data by date
#' @param df A data frame of met data
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @keywords internal
#' @noRd
plot_heatmap_calendar <- function(df) {
  # Transforming query dataframe with lubridate to fit the format needed for a heatmap calendar
  df <- df %>%
    mutate(
      year = lubridate::year(DATECT),
      day_of_the_week = lubridate::wday(DATECT, label = TRUE, week_start = 1),
      month = lubridate::month(DATECT, label = TRUE, abbr = FALSE),
      week = as.double(lubridate::isoweek(DATECT)),
      day = lubridate::day(DATECT)
    ) %>%
    dplyr::select(year, month, day, week, day_of_the_week, validator) %>%
    mutate(
      week = case_when(
        month == "December" & week == 1 ~ 53,
        month == "January" & week %in% 52:53 ~ 0,
        TRUE ~ week
      )
    ) %>%
    distinct()

  df$f_validator <- forcats::fct_relevel(
    as.factor(df$validator),
    'auto',
    after = Inf
  )

  heatmap_plot <- ggplot(df, aes(day_of_the_week, week, fill = f_validator)) +
    geom_tile(color = "white", linewidth = 0.1) +
    labs(x = 'Day of week', y = 'Week', fill = 'Validator') +
    facet_wrap(year ~ month, nrow = 4, ncol = 3, scales = "free") +
    scale_y_reverse() +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) +
    theme(legend.title = element_text(size = 8)) +
    theme(legend.text = element_text(size = 6)) +
    ggExtra::removeGrid()
  heatmap_plot
}
