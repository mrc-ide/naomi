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
unlink("outputs", recursive = TRUE)
#'
#'

##+ setup, message = FALSE
library(naomi)
library(tidyverse)
library(sf)

#' # 0. Prepare webtool GeoJSON input
#'
#' The MVP version of Naomi web tool allows upload of a single GeoJSON file for
#' specifying the area hierarchy. This preprocessing step joins the area tables
#' into a single long format dataset and saves as a GeoJSON for upload to the
#' web tool.
##+ load_area_data, message = FALSE

#' # 1. (Up)Load data inputs
#'
#' Area hierarchy and boundaries

area_merged <- read_sf(system.file("extdata/demo_areas.geojson", package = "naomi"))

#' Population data
##+ load_population_data, message = FALSE
pop_agesex <- read_csv(system.file("extdata/demo_population_agesex.csv", package = "naomi"))

#' Survey data
##+ load_survey_data, message = FALSE
survey_hiv_indicators <- read_csv(system.file("extdata/demo_survey_hiv_indicators.csv", package = "naomi"))

#' Programme data
#'
##+ message = FALSE
art_number <- read_csv(system.file("extdata/demo_art_number.csv", package = "naomi"))
anc_testing <- read_csv(system.file("extdata/demo_anc_testing.csv", package = "naomi"))


#' Programme data
#'

#' Spectrum PJNZ

pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
spec <- extract_pjnz_naomi(pjnz)



#' # 2. Choose model areas and time points
#'
#' The following are required to be provided to define the model state space:
#'
#' * `scope`: A collection of `area_id`s defining the set of areas to be modelled.
#'    Usually this is simply national level, so the level 0 `area_id`.
#' * `level`: Area level at which to fit model.
#' * `quarter_id_t1`: The first time point for the model--approximately the midpoint
#'   of the household survey data used.
#' * `quarter_id_t2`: The second time point for the model--the current time for which
#'    estimates are needed.
#' * `quarter_id_t3`: The third time point for the model--the future projection for HIV
#'    estimates.
#' 

scope <- "MWI"
level <- 4
calendar_quarter_t1 <- "CY2016Q1"
calendar_quarter_t2 <- "CY2018Q3"
calendar_quarter_t3 <- "CY2019Q4"

#' The following select data inputs to model fitting from the uploaded datasets.
#' Providing `NULL` for any will exclude that data source from model fitting.
#'
#' * Multiple household survey may be used in fitting, but they must be rougly
#'   contemporaneous around `quarter_id_t1`.
#' * Only survey ART coverage or survey VLS should be included from a given survey,
#'   not both. ART coverage is preferred if both are available.
#' * `artnum_quarter_id_t1` and `artnum_quarter_id_t1` are the time point at
#'   which current on ART programme data will be used to estimte ART coverage.
#'   They are typically the same `quarter_id_t1` and `quarter_id_t2` if ART
#'   programme data are used.
#' * `anc_quarter_id_t1` and `anc_quarter_id_t2` are typically a range of 3-4 quarters.    Data will be aggregated over these quarters for a larger sample size. They
#'   will typically be consecutive quarters, though a quarter could be dropped for
#'   example if there were reporting problems known to affect a given quarter.

#' Survey IDs to include in fitting
prev_survey_ids  <- c("DEMO2016PHIA", "DEMO2015DHS")
artcov_survey_ids  <- "DEMO2016PHIA"
vls_survey_ids <- NULL
recent_survey_ids <- "DEMO2016PHIA"

artnum_calendar_quarter_t1 <- "CY2016Q1"
artnum_calendar_quarter_t2 <- "CY2018Q3"

anc_clients_year2 <- 2018
anc_clients_year2_num_months <- 9

anc_prevalence_year1 <- 2016
anc_prevalence_year2 <- 2018

anc_art_coverage_year1 <- 2016
anc_art_coverage_year2 <- 2018


#' # 3. Review input data
#'


#' # 4. Prepare model inputs

#' Setup the model

naomi_mf <- naomi_model_frame(area_merged,
                              pop_agesex,
                              spec,
                              scope = scope,
                              level = level,
                              calendar_quarter_t1,
                              calendar_quarter_t2,
                              calendar_quarter_t3)


#' Prepare data inputs

naomi_data <- select_naomi_data(naomi_mf,
                                survey_hiv_indicators,
                                anc_testing,
                                art_number,
                                prev_survey_ids,
                                artcov_survey_ids,
                                recent_survey_ids,
                                vls_survey_ids,
                                artnum_calendar_quarter_t1,
                                artnum_calendar_quarter_t2,
                                anc_prevalence_year1,
                                anc_prevalence_year2,
                                anc_art_coverage_year1,
                                anc_art_coverage_year2)


#' 5. Fit model

#' Prepare model inputs and initial parameters

tmb_inputs <- prepare_tmb_inputs(naomi_data)


#' Fit the TMB model

##+ fit_model, cache = TRUE
fit <- fit_tmb(tmb_inputs)

#' Calculate model outputs. We can calculate outputs based on posterior mode
#' estimates before running `report_tmb()` to calculate posterior intervals.

outputs <- output_package(fit, naomi_mf)

#' The output package consists of a data frame of indicators and metadata
#' defining the labels for each indicator.
names(outputs)

#' If uncertainty has not been calcualted yet, the output object retures values
#' for `mode`, but not `mean` or `lower` and `upper` 95% uncertainty ranges.

outputs$indicators %>%
  dplyr::filter(
    indicator == "prevalence",  # HIV prevalence
    age_group == "Y015_049"   # Age group 15-49
  ) %>%
  head()

#' The function `add_output_labels()` returns the indicators table
#' with labels added as additional columns.
add_output_labels(outputs) %>%
  dplyr::filter(
    indicator == "prevalence",  # HIV prevalence
    age_group == "Y015_049"   # Age group 15-49
  ) %>%
  head()


#' Calculate uncertainty ranges and add to the output object
#' (This is time consuming and memory intensive.

##+ sample_outputs, cache = TRUE
system.time(fit <- sample_tmb(fit))

#' Regenerate outputs with uncertainty ranges.

##+ make_output_package, cache = TRUE
system.time(outputs <- output_package(fit, naomi_mf))

outputs_calib <- calibrate_outputs(outputs, naomi_mf,
                                   spectrum_plhiv_calibration_level = "national",
                                   spectrum_plhiv_calibration_strat = "sex_age_coarse",
                                   spectrum_artnum_calibration_level = "national", 
                                   spectrum_artnum_calibration_strat = "sex_age_coarse",
                                   spectrum_aware_calibration_level = "national", 
                                   spectrum_aware_calibration_strat = "sex_age_coarse",
                                   spectrum_infections_calibration_level = "national", 
                                   spectrum_infections_calibration_strat = "sex_age_coarse")


outputs$indicators %>%
  dplyr::filter(
    indicator == "prevalence",  # HIV prevalence
    age_group == "Y015_049"   # Age group 15-49
  ) %>%
  head()


#' Save model outputs to ZIP

##+ save_outputs, message = FALSE, results = "hide"
dir.create("outputs", showWarnings = FALSE)
save_output_package(outputs, "demo_outputs", "outputs", with_labels = FALSE)
save_output_package(outputs, "demo_outputs_with_labels", "outputs", with_labels = TRUE)
save_output_package(outputs, "demo_outputs_single_csv", "outputs", with_labels = TRUE, single_csv = TRUE)
save_output_package(outputs, "demo_outputs_single_csv_unlabelled", "outputs", with_labels = FALSE, single_csv = TRUE)


## #' 6. Plot some model outputs

indicators <- add_output_labels(outputs) %>%
  left_join(outputs$meta_area %>% select(area_level, area_id, center_x, center_y)) %>%
  sf::st_as_sf()


#' 15-49 prevalence by district

##+ prev_by_district_15, fig.height = 4, fig.width = 7
indicators %>%
  filter(age_group == "Y015_049",
         indicator == "prevalence",
         area_level == 4) %>%
  ggplot(aes(fill = mode)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

#' 15-49 prevalence by Zone
#'

##+ prev_by_zone_15, fig.height = 4, fig.width = 7
indicators %>%
  filter(age_group == "Y015_049",
         ## sex == "both",
         indicator == "prevalence",
         area_level == 2) %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

#' Age-specific prevalence, national

##+ age_specific_prev, fig.height = 5, fig.width = 7
indicators %>%
  dplyr::filter(area_level == 0,
         sex != "both",
         age_group %in% get_five_year_age_groups(),
         calendar_quarter == "CY2018Q3",
         indicator == "prevalence") %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_sort_order)) %>%
  ggplot(aes(age_group, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(1)) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))



#' 15-64 ART coverage by district

##+ art_cov_district, fig.height = 4, fig.width = 7
indicators %>%
  filter(age_group == "Y015_064",
         area_level == 4,
         indicator == "art_coverage") %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

#' Age-specific ART coverage, national

##+ age_specific_art_cov, fig.height = 5, fig.width = 7
indicators %>%
  dplyr::filter(area_level == 0,
         sex != "both",
         age_group %in% get_five_year_age_groups(),
         indicator == "art_coverage",
         calendar_quarter == "CY2018Q3") %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_sort_order)) %>%
  ggplot(aes(age_group, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(1)) +
  facet_wrap(~calendar_quarter) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))

#' ART coverage by age/sex and region
#'

##+ art_cov_age_sex, fig.height = 4, fig.width = 7
indicators %>%
  filter(area_level == 1,
         sex != "both",
         age_group %in% get_five_year_age_groups(),
         indicator == "art_coverage",
         calendar_quarter == "CY2018Q3") %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_sort_order)) %>%
  ggplot(aes(age_group, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(1)) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


#' Bubble plot prevalence and PLHIV
#'
##+ bubble_plot, fig.height = 4, fig.width = 7
indicators %>%
  filter(age_group == "Y015_064",
         area_level == 4,
         indicator %in% c("prevalence", "plhiv"),
         calendar_quarter == "CY2018Q3") %>%
  select(sex, center_x, center_y, indicator_label, mean) %>%
  spread(indicator_label, mean) %>%
  ggplot() +
  geom_sf() +
  geom_point(aes(center_x, center_y, colour = `HIV prevalence`, size = PLHIV)) +
  viridis::scale_color_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)
