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
ggiraph_plot <- function(
  input_variable,
  scale_ref = FALSE,
  point_size = 3,
  vars_to_show = NULL
) {
  dt_plot <- merge(
    mm_qry$dt,
    data.table::as.data.table(df_method)[, .(qc, method_longname)],
    by = "qc",
    all.x = TRUE
  )

  if (scale_ref) {
    dt_plot[
      name_icos == input_variable,
      ref := {
        valid <- !is.na(value) & !is.na(ref)
        if (sum(valid) >= 2L && var(ref[valid]) > 0) {
          m <- lm(value[valid] ~ ref[valid])
          coef(m)[[1L]] + coef(m)[[2L]] * ref
        } else {
          ref
        }
      },
      by = site
    ]
  }

  if (!is.null(vars_to_show) && length(vars_to_show) > 0L) {
    dt_plot <- dt_plot[var_name %in% vars_to_show]
  }

  col_pal_base <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#999999"
  )
  n_vars <- data.table::uniqueN(dt_plot[name_icos == input_variable, var_name])
  col_pal <- colorRampPalette(col_pal_base)(max(n_vars, 1L))

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
      size = point_size
    ) +

    facet_wrap_interactive(
      ncol = 2,
      interactive_on = "text",
      ggplot2::vars(site),
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
  return(p1_girafe)
}
