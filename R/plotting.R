#' Custom plotting function for each variable
#'

#' @title ggiraph_plot
#' @description Creates an interactive girafe plot, whereby the user can select
#'   points with dubious quality and impute new values.
#' @param input_variable The name of the variable within the query data frame to plot.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export
ggiraph_plot <- function(input_variable) {
  time_name <- mm_qry$dt_meta[type == "time", name_dt]
  df <- data.frame(
    # DATECT = mm_qry$dt$DATECT,
    DATECT = mm_qry$dt[, get(time_name)],
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
        tooltip = glue::glue("Timestamp: {DATECT}\nMeasure: {y}"),
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
