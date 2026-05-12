#' Custom plotting function for each variable
#'

#' @importFrom ggplot2 scale_linetype_manual
#' @title ggiraph_plot
#' @description Creates an interactive girafe plot, whereby the user can select
#'   points with dubious quality and impute new values.
#' @param input_variable The name of the variable within the query data frame to plot.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export
ggiraph_plot <- function(input_variable) {
  dt_plot <- merge(
    mm_qry$dt,
    data.table::as.data.table(df_method)[, .(qc, method_longname)],
    by = "qc",
    all.x = TRUE
  )

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

  p1_ggplot <- ggplot(
    dt_plot[name_icos == input_variable],
    aes(TIMESTAMP, value)
  ) +
    geom_point_interactive(
      aes(
        data_id = row_name,
        tooltip = glue::glue(
          "Timestamp: {TIMESTAMP}\nRowname: {row_name}"
        ),
        shape = factor(method_longname),
        colour = factor(var_name)
      ),
      size = 3
    ) +

    facet_wrap_interactive(
      ncol = 2,
      interactive_on = "text",
      vars(site),
      labeller = labeller_interactive(aes(
        tooltip = paste("The site is", site),
        data_id = site
      ))
    ) +

    geom_line(
      aes(y = ref, linetype = paste0("Reference: ", input_variable)),
      colour = "black"
    ) +
    scale_linetype_manual(name = NULL, values = "solid") +
    # scale_color_manual(values = col_pal, limits = force) +
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
  return(p1_girafe)
}
