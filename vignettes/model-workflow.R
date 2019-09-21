
#' ---
#' title: "Naomi Model Workflow Example"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Naomi Model Workflow Example}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---

##+ include = FALSE
knitr::opts_chunk$set(
                    collapse = TRUE,
                    comment = "#>"
                  )

#'
#'

##+ setup, message = FALSE
## library(naomi)
devtools::load_all()
library(tidyverse)
library(sf)

#' # 0. Prepare webtool GeoJSON input
#'
#' The MVP version of Naomi web tool allows upload of a single GeoJSON file for
#' specifying the area hierarchy. This preprocessing step joins the area tables
#' into a single long format dataset and saves as a GeoJSON for upload to the
#' web tool.
##+ load_area_data, message = FALSE

area_levels <- read_csv(system.file("extdata/areas/area_levels.csv", package = "naomi"))
area_hierarchy  <- read_csv(system.file("extdata/areas/area_hierarchy.csv", package = "naomi"))
area_boundaries <- sf::read_sf(system.file("extdata/areas/area_boundaries.geojson", package = "naomi"))

area_long <- area_hierarchy %>%
  left_join(
    area_levels %>% select(area_level, area_level_label, display, naomi_level)
  ) %>%
  left_join(
    area_boundaries
  )

st_write(area_long, file.path(tempdir(), "area_long.geojson"), delete_dsn = TRUE)

#' # 1. (Up)Load data inputs
#'
#' Area hierarchy and boundaries

area_long <- read_sf(file.path(tempdir(), "area_long.geojson"))

areas <- create_areas(area_levels, area_hierarchy, area_boundaries)


#' Population data
##+ load_population_data, message = FALSE
pop_agesex <- read_csv(system.file("extdata/population/population_agesex.csv", package = "naomi"))

#' Survey data
##+ load_survey_data, message = FALSE
survey_hiv_indicators <- read_csv(system.file("extdata/survey/survey_hiv_indicators.csv", package = "naomi"))

#' Programme data
#'

art_number <- read_csv(system.file("extdata/programme/art_number.csv", package = "naomi"))
anc_testing <- read_csv(system.file("extdata/programme/anc_testing.csv", package = "naomi"))


#' Programme data
#'

#' Spectrum PJNZ

pjnz <- system.file("extdata/mwi2019.PJNZ", package = "naomi")
spec <- extract_pjnz_naomi(pjnz)



#' # 2. Choose and validate inputs

#'### Choose data to include

#' Vector of area IDs to restrict model to
scope <- "MWI"

#' Level in hierarchy to fit the model
level <- 4

#' Survey IDs to include in fitting
survey_ids  <- c("MWI2016PHIA", "MWI2015DHS")

#' First time point for model fitting (roughly midpoint of survey fieldwork)
artnum_quarter_id_t1 <- convert_quarter_id(1, 2016)

#' Range of quarters to use for ANC testing data at time 1
#' Roughly 2 quarters before nad after time 1
anc_quarter_id_t1 <- convert_quarter_id(c(4, 1, 2, 3), c(2015, 2016, 2016, 2016))

#' Second time point for model fitting (current quarter desired for estimates)
artnum_quarter_id_t2 <- convert_quarter_id(3, 2018)

#' Range of quarters to use for ANC testing data at time 1
anc_quarter_id_t2 <- convert_quarter_id(1:4, 2018)




## #' # 3. Review input data
## #'
## #' ### Survey prevalence chlorpleth
## #'
## #' PHIA prevalence by Zone (level 2)


## ar <- get_area_collection(areas, level = 2)

## dat <- mwi_survey_hiv_indicators %>%
##   filter(indicator == "prev",
##          area_id %in% ar$area_id,
##          sex == "both",
##          age_group_id == 18,
##          survey_id == "MWI2016PHIA")

## dat %>%
##   left_join(
##     area_geom %>% filter(type == "boundary")
##   ) %>%
##   sf::st_as_sf() %>%
##   ggplot(aes(fill = est)) +
##   geom_sf() +
##   viridis::scale_fill_viridis(labels = scales::percent_format()) +
##   th_map()


## #' DHS prevalence by district (level 4) within Northern Region (MWI.1)

## ar <- get_area_collection(areas, level = 4, area_scope = "MWI.1")

## dat <- mwi_survey_hiv_indicators %>%
##   filter(indicator == "prev",
##          area_id %in% ar$area_id,
##          sex == "both",
##          age_group_id == 18,
##          survey_id == "MWI2015DHS")

## dat %>%
##   left_join(
##     area_geom %>% filter(type == "boundary")
##   ) %>%
##   sf::st_as_sf() %>%
##   ggplot(aes(fill = est)) +
##   geom_sf() +
##   viridis::scale_fill_viridis(labels = scales::percent_format()) +
##   th_map()



## #' ### Survey prevalence and sample size
## #'


## ar <- get_area_collection(areas, level = 4) %>%
##   left_join(
##     mwi_area_geom %>% filter(type == "boundary")
##   ) %>%
##   sf::st_as_sf()

## dat <- mwi_survey_hiv_indicators %>%
##   filter(indicator == "prev",
##          area_id %in% ar$area_id,
##          sex == "both",
##          age_group_id == 18,
##          survey_id == "MWI2016PHIA")

## dat %>%
##   left_join(
##     area_geom %>% filter(type == "center_adj")
##   ) %>%
##   sf::st_as_sf() %>%
##   ggplot() +
##   geom_sf(data = ar) +
##   geom_sf(aes(color = est, size = n_obs), show.legend = "point") +
##   viridis::scale_color_viridis(labels = scales::percent_format()) +
##   scale_size_area() +
##   th_map()


#' # 4. Prepare model inputs

#' Setup the model 

naomi_mf <- naomi_model_frame(areas,
                              pop_agesex,
                              spec,
                              level = level,
                              artnum_quarter_id_t1,
                              artnum_quarter_id_t2)


#' Prepare data inputs

prev_dat <- survey_prevalence_mf(survey_ids, survey_hiv_indicators, naomi_mf)
artcov_dat <- survey_artcov_mf(survey_ids, survey_hiv_indicators, naomi_mf)
recent_dat <- survey_recent_mf(survey_ids, survey_hiv_indicators, naomi_mf)

anc_prev_t1_dat <- anc_testing_prev_mf(anc_quarter_id_t1, anc_testing, naomi_mf)
anc_artcov_t1_dat <- anc_testing_artcov_mf(anc_quarter_id_t1, anc_testing, naomi_mf)

anc_prev_t2_dat <- anc_testing_prev_mf(anc_quarter_id_t2, anc_testing, naomi_mf)
anc_artcov_t2_dat <- anc_testing_artcov_mf(anc_quarter_id_t2, anc_testing, naomi_mf)

artnum_t1_dat <- artnum_mf(artnum_quarter_id_t1, art_number, naomi_mf)
artnum_t2_dat <- artnum_mf(artnum_quarter_id_t2, art_number, naomi_mf)


#' Prepare model inputs and initial parameters

tmb_inputs <- prepare_tmb_inputs(naomi_mf, prev_dat, artcov_dat, recent_dat,
                                 anc_prev_t1_dat,
                                 anc_prev_t2_dat,
                                 anc_artcov_t1_dat,
                                 anc_artcov_t2_dat,
                                 artnum_t1_dat,
                                 artnum_t2_dat)


#' 5. Fit model
#'
#' Note: useful for how to include multiple TMB models: https://stackoverflow.com/questions/48627069/guidelines-for-including-tmb-c-code-in-an-r-package

#' Fit the TMB model
fit <- fit_tmb(tmb_inputs)

#' Calculate model outputs. We can calculate outputs based on posterior mode
#' estimates before running `report_tmb()` to calculate posterior intervals.

outputs <- output_package(fit, naomi_mf, areas)

#' The output package consists of a data frame of indicators and metadata
#' defining the labels for each indicator.
names(outputs)

#' If uncertainty has not been calcualted yet, the output object retures values
#' for `mode`, but not `mean` or `lower` and `upper` 95% uncertainty ranges.

outputs$indicators %>%
  dplyr::filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%
  head()

#' The function `add_output_labels()` returns the indicators table
#' with labels added as additional columns.
add_output_labels(outputs) %>%
  dplyr::filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%
  head()


#' Calculate uncertainty ranges and add to the output object
#' (This is time consuming and memory intensive.
system.time(fit <- sample_tmb(fit))

#' Regenerate outputs with uncertainty ranges.
system.time(outputs <- output_package(fit, naomi_mf, areas))

outputs$indicators %>%
  dplyr::filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%
  head()


#' Save model outputs to ZIP

save_output_package(outputs, "mwi_outputs", "~/Downloads", with_labels = FALSE)
save_output_package(outputs, "mwi_outputs_with_labels", "~/Downloads", with_labels = TRUE)
save_output_package(outputs, "mwi_outputs_single_csv", "~/Downloads", with_labels = TRUE, single_csv = TRUE)
save_output_package(outputs, "mwi_outputs_single_csv_unlabelled", "~/Downloads", with_labels = FALSE, single_csv = TRUE)


#' 6. Plot some model outputs

## summary(ftmb)

indicators <- add_output_labels(outputs) %>%
  left_join(outputs$meta_area %>% select(area_level, area_id, center_x, center_y)) %>%
  sf::st_as_sf()


#' 15-49 prevalence by district
indicators %>%
  filter(age_group_id == 18,
         ## sex == "both",
         indicator_id == 2L,
         area_level == 4) %>%
  ggplot(aes(fill = mode)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

#' 15-49 prevalence by 28 districts, Southern region
#'

indicators %>%
  filter(age_group_id == 18,
         ## sex == "both",
         indicator_id == 2L,
         area_level == 3) %>%
  ## semi_join(get_area_collection(areas, level = 3, area_scope = "MWI.3")) %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

indicators %>%
  dplyr::filter(area_level == 0,
         sex != "both",
         age_group_id %in% 1:17,
         indicator_id == 2L) %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_id)) %>%
  ggplot(aes(age_group, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  geom_point(aes(age_group, median), position = position_dodge(0.8)) +
  geom_point(aes(age_group, mode), position = position_dodge(0.8), shape = 2) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


indicators %>%
  dplyr::filter(area_level == 0,
         sex != "both",
         age_group_id %in% 1:17,
         indicator_id == 4L) %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_id)) %>%
  ggplot(aes(age_group, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  geom_point(aes(age_group, mean), position = position_dodge(0.8)) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


#' 15-64 ART coverage
indicators %>%
  filter(age_group_id == 19,
         area_level == 4,
         indicator_id == 4L) %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

#' ART coverage by age/sex and region
indicators %>%
  filter(area_level == 1,
         sex != "both",
         age_group_id %in% 1:17,
         indicator_id == 4L) %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_id)) %>%
  ggplot(aes(age_group, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


#' Bubble plot prevalence and PLHIV
indicators %>%
  filter(age_group_id == 19,
         area_level == 4,
         indicator_id %in% 2:3) %>%
  select(sex, center_x, center_y, indicator_label, mean) %>%
  spread(indicator_label, mean) %>%
  ggplot() +
  geom_sf() +
  geom_point(aes(center_x, center_y, colour = `HIV Prevalence`, size = PLHIV)) +
  viridis::scale_color_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)


adj_ij %>%
  left_join(sh %>% select(area_name_i = area_name, i = area_idx)) %>%
  left_join(sh %>% select(area_name_j = area_name, j = area_idx)) %>%
  mutate(gamma = rep$gamma_art) %>%
  filter(area_name_i == "Dedza")
