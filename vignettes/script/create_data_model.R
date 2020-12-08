#!/usr/bin/env Rscript

library(dplyr)
library(forcats)
library(ggplot2)
library(datamodelr)
library(sf)

area_levels <- naomi::demo_area_levels
area_hierarchy <- naomi::demo_area_hierarchy
area_boundaries <- naomi::demo_area_boundaries

#' population
population_agesex <- naomi::demo_population_agesex
age_group_meta <- naomi::get_age_groups()

fertility <- data.frame(area_id = character(0),
                        time = numeric(0),
                        age_group = character(0),
                        calendar_quarter = character(0),
                        asfr = numeric(0))

#' surveys
survey_meta <- naomi::demo_survey_meta
survey_regions <- naomi::demo_survey_regions
survey_clusters <- naomi::demo_survey_clusters
survey_individuals <- naomi::demo_survey_individuals
survey_biomarker <- naomi::demo_survey_biomarker

survey_hiv_indicators <- naomi::demo_survey_hiv_indicators

#' programme
art_number <- naomi::demo_art_number
anc_testing <- naomi::demo_anc_testing

dm_add_colors(
  dm_color_scheme(
    population = dm_palette(
      line_color = "#8064A2",
      header_bgcolor = "#B1A0C7",
      header_font = "#FFFFFF",
      bgcolor = "#E4DFEC"
    ),
    areas = dm_palette(
      line_color = "#C0504D",
      header_bgcolor = col2rgb("darkred") %>% {rgb(.[1], .[2], .[3], maxColorValue = 255)},
      header_font = "#FFFFFF",
      bgcolor = "#F2DCDB"
    )
  )
)

dm <-
  dm_from_data_frames(
    area_levels,
    area_hierarchy,
    area_boundaries,
    age_group_meta,
    population_agesex,
    fertility,
    survey_meta,
    survey_regions,
    survey_clusters,
    survey_individuals,
    survey_biomarker,
    survey_hiv_indicators,
    art_number,
    anc_testing
  ) %>%
  dm_set_display(
    list(
      areas = c("area_levels", "area_hierarchy", "area_boundaries"),
      population = c("age_group_meta", "population_agesex", "fertility"),
      accent1 = c("art_number", "anc_testing"),
      accent4 = c("survey_meta", "survey_hiv_indicators"),
      accent6 = c("age_group_meta")
    )
  ) %>%
  dm_set_key("area_levels", "area_level") %>%
  dm_set_key("area_hierarchy", "area_id") %>%
  dm_set_key("area_boundaries", "area_id") %>%
  dm_set_key("age_group_meta", "age_group") %>%
  dm_set_key("population_agesex", c("area_id", "calendar_quarter", "sex", "age_group", "source")) %>%
  dm_set_key("fertility", c("area_id", "time", "age_group")) %>%
  dm_set_key("survey_meta", "survey_id") %>%
  dm_set_key("survey_regions", c("survey_id", "survey_region_id")) %>%
  dm_set_key("survey_clusters", c("survey_id", "cluster_id")) %>%
  dm_set_key("survey_individuals", c("survey_id", "cluster_id", "household", "line")) %>%
  dm_set_key("survey_biomarker", c("survey_id", "cluster_id", "household", "line")) %>%
  dm_set_key("survey_hiv_indicators", c("indicator", "survey_id", "area_id", "sex", "age_group")) %>%
  dm_set_key("art_number", c("area_id", "sex", "age_group", "calendar_quarter")) %>%
  dm_set_key("anc_testing", c("area_id", "age_group", "calendar_quarter")) %>%
  dm_add_references(
    area_hierarchy$area_level == area_levels$area_level,
    area_hierarchy$parent_area_id == area_hierarchy$area_id,
    area_boundaries$area_id == area_hierarchy$area_id,
    population_agesex$area_id == area_hierarchy$area_id,
    population_agesex$age_group == age_group_meta$age_group,
    fertility$area_id == population_agesex$area_id,
    fertility$time == population_agesex$time,
    fertility$age_group == population_agesex$age_group,
    population_agesex$age_group == age_group_meta$age_group,
    survey_regions$survey_id == survey_meta$survey_id,
    survey_regions$survey_region_area_id == area_hierarchy$area_id,
    survey_clusters$survey_id == survey_meta$survey_id,
    survey_clusters$survey_id == survey_regions$survey_id,
    survey_clusters$survey_region_id == survey_regions$survey_region_id,
    survey_clusters$geoloc_area_id == area_hierarchy$area_id,
    survey_individuals$survey_id == survey_clusters$survey_id,
    survey_individuals$cluster_id == survey_clusters$cluster_id,
    survey_biomarker$survey_id == survey_individuals$survey_id,
    survey_biomarker$cluster_id == survey_individuals$cluster_id,
    survey_biomarker$household == survey_individuals$household,
    survey_biomarker$line == survey_individuals$line,
    survey_hiv_indicators$survey_id == survey_meta$survey_id,
    survey_hiv_indicators$area_id == area_hierarchy$area_id,
    survey_hiv_indicators$age_group == age_group_meta$age_group,
    art_number$area_id == area_hierarchy$area_id,
    art_number$age_group == age_group_meta$age_group,
    anc_testing$area_id == area_hierarchy$area_id,
    anc_testing$age_group == age_group_meta$age_group
  ) %>%
  dm_set_segment(
    list(
      "Areas" = c("area_levels", "area_hierarchy", "area_boundaries"),
      "Population" = c("age_group_meta", "population_agesex", "fertility"),
      "Survey" = c("survey_meta", "survey_regions", "survey_clusters",
                   "survey_individuals", "survey_biomarker",
                   "survey_hiv_indicators"),
      "Programme" = c("art_number", "anc_testing")
    )
  )

#' Render graph
dm <- dm_create_graph(dm, rankdir = "BT", col_attr = c("column", "type"))
dm_export_graph(dm, "vignettes/figure/data_model.png", "png")
