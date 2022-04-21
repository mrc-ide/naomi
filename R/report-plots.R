##' Create output map plot
##'
##' @param naomi_geom Labelled naomi output object with geometry
##' @param indicator The indicator to generate plot for
##' @param calendar_quarter Naomi projection quarter to filter supplied data to.
##' @param age The age group to include in plots
##' @param sex_disag Sex diaggregation to use in plot,
##' @param level Integer, corresponding to desired area level to filter supplied data to. Default is lowest area level available in supplied data.
##' @param colour_palette Names or hexcode value for colour scale to be applied to to lowest and highest value in the supplied data. Can be specified as a preset colour palette using "red", "green" and "blue" or manually as a named list: cols = c( start_colour = "red", end_colour = "blue").
##' @param fig_title Title for the figure, blank by default
##' @param legend_title Title for the legend, blank by default
##' @param legend_label Legend label
##' @param breaks Number of break points to create in scale
##'
##' @return
##' @export
##'
##'
##'

map_outputs <- function(naomi_geom,
                        indicator,
                        calendar_quarter,
                        age,
                        sex_disag,
                        level = NULL,
                        colour_palette = "red",
                        fig_title = NULL,
                        legend_title = NULL,
                        legend_label = ggplot2::waiver(),
                        breaks = 6) {


  # set colour palette
  if(colour_palette == "blue") {cols = c(start_colour="#4A718C47",end_colour="skyblue4")}
  if(colour_palette == "red") {cols = c(start_colour="#8C000038",end_colour="red4")}
  if(colour_palette == "green") {cols = c(start_colour="#688C2155",end_colour="#4A690C")}

  # shape data for plot
  if(is.null(level)) {level <- max(levels(as.factor(naomi_geom$area_level)))}
  quarter <- calendar_quarter

  fig_data <- naomi_geom %>%
    dplyr::filter(indicator == !!indicator,
                  age_group == age,
                  sex == sex_disag,
                  calendar_quarter == quarter,
                  area_level == level)
  # generate plot
  ggplot2::ggplot(fig_data, ggplot2::aes(fill = mean)) +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_steps(low = cols[["start_colour"]],
                              high = cols[["end_colour"]],
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

#' Create population pyramid plot
#'
#' @param naomi_output  Naomi output object or path to naomi output zip folder
#' @param calendar_quarter Naomi projection quarter to filter data to, default is calendar_quarter_t2
#' @param colour_palette Names or hexcode value for right and left side of population pyramid. Can be specified as a preset colour palette using "red", "green" and "blue" or manually as a named list: cols <- c( left_clour = "red", right_colour = "blue").
#' @param x_title Title for the figure x axis, blank by default
#' @param y_title Title for the figure y axis, default is "Age Group"
#' @param fig_title Title for the figure, blank by default,
#' @param masc_label Label for male sex group, default is "Male"
#' @param fem_label Label for female sex group, default is "Female"
#'
##' @return
##' @export

pop_pyramid_outputs <- function(naomi_output,
                                calendar_quarter = 2,
                                indicator,
                                colour_palette = "blue",
                                x_title = NULL,
                                y_title = t_("AGE_GROUP"),
                                fig_title = NULL,
                                legend_label = abs,
                                masc_label = t_("SEX_MALE"),
                                fem_label = t_("SEX_FEMALE")) {



  # if not naomi output - read naomi output zip
  if(!inherits(naomi_output, "naomi_output")) {
    naomi_output <- read_output_package(naomi_output)
  }

  # set colour palette
  if(colour_palette == "blue") {cols <- c(left_colour="slategray3",right_colour="skyblue4")}
  if(colour_palette == "red") {cols <- c(left_colour="mistyrose2",right_colour="red4")}
  if(colour_palette == "green") {cols <- c(left_colour="honeydew3",right_colour="olivedrab4")}

  # assign translated labels to colours
  names(cols) <- c(masc_label, fem_label)

  # shape data for plot

  level_max <- min(levels(as.factor(naomi_output$meta_area$area_level)))
  quarter <- naomi_output$meta_period[calendar_quarter,]$calendar_quarter

  fig_data <- add_output_labels(naomi_output) %>%
    # get age labels for 5-year age bands
    dplyr::left_join(naomi_output$meta_age_group, by = c("age_group", "age_group_label")) %>%
    # filter for desired indicator and disaggregates
    dplyr::filter(area_level == level_max,
                  calendar_quarter == quarter,
                  sex != "both",
                  age_group_span == 5,
                  indicator == !!indicator) %>%
    dplyr::mutate(age_group = forcats::fct_reorder(age_group_label, age_group_sort_order)) %>%
    # recode sex varible to corresponding colour palette label
    dplyr::mutate(sex = dplyr::recode_factor(sex,
                                             "male" = masc_label,
                                             "female" = fem_label))


  # generate plot
  ggplot2::ggplot(fig_data, ggplot2::aes(x = ifelse(sex == masc_label, -mean, mean),
                                                 y = age_group,
                                                 ymin = lower,
                                                 ymax = upper,
                                                 fill = sex)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::scale_x_continuous(labels = legend_label,
                                limits = max(fig_data$mean) * c(-1,1)) +
    ggplot2::labs(x = x_title,
                  y = y_title) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(legend.position = "top",
                   plot.title = ggplot2::element_text(size = 8, face = "bold",
                                                      hjust = 0.5),
                   axis.title = ggplot2::element_text(size = 6),
                   axis.text.x = ggplot2::element_text(size = 5),
                   axis.text.y = ggplot2::element_text(size = 5),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 6),
                   legend.key.size = ggplot2::unit(0.5, "lines"),
                   plot.margin = ggplot2::margin(0.5, 0.3, 0.5, 0.3, "cm")) +
    ggplot2::ylab(y_title) +
    ggplot2::ggtitle(fig_title)
}



##' Create district bar plot
##'
##' @param naomi_output  Naomi output object or path to naomi output zip folder
##' @param indicator Indicator to plot
##' @param age Age group to plot
##' @param sex_disag Sexes to plot
##' @param level Area level to plot, default all
##' @param calendar_quarter Naomi projection quarter to filter data to, default is calendar_quarter_t2
##' @param label_format Label format for y labels
##' @param colour_palette Names or hexcode value for colours to differentiate districts with values above and below the national average. Can be specified as a preset colour palette using "red", "green" and "blue" or manually as a named list: cols <- c(above_colour = "red", below_colour = "blue").
##' @param x_title Title for the figure x axis, blank by default
##' @param fig_title Title for the figure, blank by default,
##' @param legend_title Title for the legend
##' @param above_label Label for districts with values higher than national average, default is "Above"
##' @param below_label Label for districts with values higher than national average, default is "Below"
##'
##' @return
##' @export
district_barplot <- function(naomi_output,
                             indicator,
                             age,
                             sex_disag,
                             level = NULL,
                             calendar_quarter = 2,
                             label_format,
                             x_title = NULL,
                             fig_title = NULL,
                             legend_title = NULL,
                             colour_palette = "blue",
                             above_label = t_("ABOVE"),
                             below_label = t_("BELOW")) {

  # if not naomi output - read naomi output zip
  if(!inherits(naomi_output, "naomi_output")) {
    naomi_output <- read_output_package(naomi_output)
  }

  # filter data for desired disaggregates
  if(is.null(level)) {level <- max(levels(as.factor(naomi_output$meta_area$area_level)))}
  quarter <- naomi_output$meta_period[calendar_quarter,]$calendar_quarter

  district_data <- add_output_labels(naomi_output) %>%
    dplyr::filter(indicator == !!indicator,
                  age_group == age,
                  sex == sex_disag,
                  calendar_quarter == quarter)

  # generate bar plot with national level threshold if national indicator present
  if ("0" %in% naomi_output$meta_area$area_level) {

    # set colour palette
    if(colour_palette == "blue") {cols <- c(below_colour="slategray3",above_colour="skyblue4")}
    if(colour_palette == "red") {cols <- c(below_colour="mistyrose2",above_colour="red4")}
    if(colour_palette == "green") {cols <- c(below_colour="honeydew3",above_colour="olivedrab4")}

    # assign translated labels to colours
    names(cols) <- c(above_label, below_label)

    # format national indicator
    national_data <- dplyr::filter(district_data, area_level == 0)

    gen_est <- function(df, ind_scale, deci, sign) {
      df <- df %>%
        dplyr::mutate_if(is.numeric, ~ . * ind_scale) %>%
        dplyr::mutate_if(is.numeric, round, deci)
      est <- paste0(df$mean, sign, " (", df$lower, " - ", df$upper, sign, ")") }

    national_ind <- national_data$mean
    if(indicator == "incidence"){national_est <- gen_est(national_data, 1000, 1, "")}
    if(indicator == "prevalence"){national_est <- gen_est(national_data, 100, 2, "%")}
    if(indicator == "art_coverage"){national_est <- gen_est(national_data, 100, 0, "%")}

    # shape data for plot
    fig_data <- district_data %>%
      dplyr::filter(area_level == level) %>%
      dplyr::mutate(threshold = ifelse(mean > national_ind, above_label , below_label),
                    threshold = as.factor(threshold))

    #Plot figure
   plot <- ggplot2::ggplot(fig_data, ggplot2::aes(reorder(area_name, mean),
                                           mean,
                                           ymin = lower,
                                           ymax = upper,
                                           fill = threshold)) +
      ggplot2::theme_classic() +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::geom_linerange(position = ggplot2::position_dodge(0.8)) +
      ggplot2::scale_y_continuous(labels = label_format) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 8),
        legend.background = ggplot2::element_rect(linetype = "dashed", colour = "black"),
        legend.text = ggplot2::element_text(size = 8),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = ggplot2::element_text(size = 8, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 8, face = "bold"),
        axis.title.y = ggplot2::element_blank()) +
      ggplot2::ylab(x_title) +
      ggplot2::geom_hline(yintercept= national_ind, linetype="dashed", color = "black")+
      ggplot2::ggtitle(fig_title) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(name = paste0(legend_title, " \n ", national_est),
                                 values = cols)
   plot

  } else {
    # generate district level plot if no national indicator is available

    # set colour palette
    if(colour_palette == "blue") {colour="skyblue4"}
    if(colour_palette == "red") {colour="red4"}
    if(colour_palette == "green") {colour="olivedrab4"}

    # shape data for plot
    fig_data <- district_data %>%
      dplyr::filter(area_level == level)

    #Plot figure
    plot <- ggplot2::ggplot(district_data, ggplot2::aes(reorder(area_name, mean),
                                                mean,
                                                ymin = lower,
                                                ymax = upper)) +
      ggplot2::theme_classic() +
      ggplot2::geom_col(position = "dodge", fill = colour) +
      ggplot2::geom_linerange(position = ggplot2::position_dodge(0.8)) +
      ggplot2::scale_y_continuous(labels = label_format) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_text(size = 8, face = "bold"),
        axis.title.y = ggplot2::element_blank()) +
      ggplot2::ylab(x_title) +
      ggplot2::ggtitle(fig_title) +
      ggplot2::coord_flip()

    plot

  }
}

