##' Create output map plot
##'
##' @param geom_data Data frame containing indicators output and geometry
##'   filtered to a single quarter
##' @param indicator The indicator to generate plot for
##' @param age The age group to include in plots
##' @param sex_disag Sex diaggregation to use in plot,
##' @param start_colour_scale The colour value to start scale at, applied to
##'   lowest value in the data.
##' @param end_colour_scale The colour value to end scale at, applied to highest
##'   value in the supplied data,
##' @param fig_title Title for the figure, blank by default
##' @param legend_title Title for the legend, blank by default
##' @param legend_label Legend label
##' @param breaks Number of break points to create in scale
##'
##' @return
##' @export
map_outputs <- function(geom_data,
                        indicator,
                        age,
                        sex_disag,
                        start_colour_scale,
                        end_colour_scale,
                        fig_title = NULL,
                        legend_title = NULL,
                        legend_label = ggplot2::waiver(),
                        breaks = 6) {

  force(fig_title)
  force(legend_title)
  # filter data for desired indicator
  fig_data <- geom_data %>% dplyr::filter(indicator == !!indicator,
                                          age_group == age,
                                          sex == sex_disag)
  # generate figure
  ggplot2::ggplot(fig_data, ggplot2::aes(fill = mean)) +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_steps(low = start_colour_scale,
                              high = end_colour_scale,
                              name = legend_title,
                              labels = legend_label,
                              n.breaks = breaks) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = "vertical",
                   plot.title = ggplot2::element_text(size = 8, face = "bold"),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.title = ggplot2::element_text(size = 8, face = "bold",
                                                        hjust = 0.5),
                   legend.key.size = ggplot2::unit(0.7, "lines"),
                   legend.background = ggplot2::element_rect(
                     linetype = "dashed", colour = "black"),
                   legend.title.align = 0.5,
                   plot.margin = ggplot2::margin(0, 0.4, 0, 0.4, "cm")) +
    ggplot2::ggtitle(fig_title)
}
