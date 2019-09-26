options(error = traceback)

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(
                    collapse = TRUE,
                    comment = "#>"
                  )


## ----setup, message = FALSE----------------------------------------------
library(naomi)
library(tidyverse)
library(sf)


## ----load_area_data, message = FALSE-------------------------------------

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


## ----message = FALSE-----------------------------------------------------
st_write(area_long, file.path(tempdir(), "area_long.geojson"), delete_dsn = TRUE)


## ------------------------------------------------------------------------
area_long <- read_sf(file.path(tempdir(), "area_long.geojson"))

areas <- create_areas(area_levels, area_hierarchy, area_boundaries)


## ----load_population_data, message = FALSE-------------------------------
pop_agesex <- read_csv(system.file("extdata/population/population_agesex.csv", package = "naomi"))


## ----load_survey_data, message = FALSE-----------------------------------
survey_hiv_indicators <- read_csv(system.file("extdata/survey/survey_hiv_indicators.csv", package = "naomi"))


## ----message = FALSE-----------------------------------------------------
art_number <- read_csv(system.file("extdata/programme/art_number.csv", package = "naomi"))
anc_testing <- read_csv(system.file("extdata/programme/anc_testing.csv", package = "naomi"))


## ------------------------------------------------------------------------
pjnz <- system.file("extdata/mwi2019.PJNZ", package = "naomi")
spec <- extract_pjnz_naomi(pjnz)


## ------------------------------------------------------------------------
scope <- "MWI"
level <- 4
quarter_id_t1 <- convert_quarter_id(1, 2016)
quarter_id_t2 <- convert_quarter_id(3, 2018)


## ------------------------------------------------------------------------
prev_survey_ids  <- c("MWI2016PHIA", "MWI2015DHS")
artcov_survey_ids  <- "MWI2016PHIA"
vls_survey_ids <- NULL
recent_survey_ids <- "MWI2016PHIA"

artnum_quarter_id_t1 <- convert_quarter_id(1, 2016)
artnum_quarter_id_t2 <- convert_quarter_id(3, 2018)

anc_quarter_id_t1 <- convert_quarter_id(c(4, 1, 2, 3), c(2015, 2016, 2016, 2016))
anc_quarter_id_t2 <- convert_quarter_id(1:4, 2018)


## ------------------------------------------------------------------------
naomi_mf <- naomi_model_frame(areas,
                              pop_agesex,
                              spec,
                              scope = scope,
                              level = level,
                              quarter_id_t1,
                              quarter_id_t2)


## ------------------------------------------------------------------------
prev_dat <- survey_prevalence_mf(prev_survey_ids, survey_hiv_indicators, naomi_mf)
artcov_dat <- survey_artcov_mf(artcov_survey_ids, survey_hiv_indicators, naomi_mf)
recent_dat <- survey_recent_mf(recent_survey_ids, survey_hiv_indicators, naomi_mf)

anc_prev_t1_dat <- anc_testing_prev_mf(anc_quarter_id_t1, anc_testing, naomi_mf)
anc_artcov_t1_dat <- anc_testing_artcov_mf(anc_quarter_id_t1, anc_testing, naomi_mf)

anc_prev_t2_dat <- anc_testing_prev_mf(anc_quarter_id_t2, anc_testing, naomi_mf)
anc_artcov_t2_dat <- anc_testing_artcov_mf(anc_quarter_id_t2, anc_testing, naomi_mf)

artnum_t1_dat <- artnum_mf(artnum_quarter_id_t1, art_number, naomi_mf)
artnum_t2_dat <- artnum_mf(artnum_quarter_id_t2, art_number, naomi_mf)


## ------------------------------------------------------------------------
tmb_inputs <- prepare_tmb_inputs(naomi_mf, prev_dat, artcov_dat, recent_dat,
                                 anc_prev_t1_dat,
                                 anc_prev_t2_dat,
                                 anc_artcov_t1_dat,
                                 anc_artcov_t2_dat,
                                 artnum_t1_dat,
                                 artnum_t2_dat)


## ----fit_model, cache = TRUE---------------------------------------------
fit <- fit_tmb(tmb_inputs)


## ------------------------------------------------------------------------
outputs <- output_package(fit, naomi_mf, areas)


## ------------------------------------------------------------------------
names(outputs)


## ------------------------------------------------------------------------
outputs$indicators %>%
  dplyr::filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%
  head()


## ------------------------------------------------------------------------
add_output_labels(outputs) %>%
  dplyr::filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%
  head()


## ----sample_outputs, cache = TRUE----------------------------------------
system.time(fit <- sample_tmb(fit))


## ----make_output_package, cache = TRUE-----------------------------------
system.time(outputs <- output_package(fit, naomi_mf, areas))

outputs$indicators %>%
  dplyr::filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%
  head()


## ----save_outputs, message = FALSE, results = "hide"---------------------
dir.create("outputs", showWarnings = FALSE)
save_output_package(outputs, "mwi_outputs", "outputs", with_labels = FALSE)
save_output_package(outputs, "mwi_outputs_with_labels", "outputs", with_labels = TRUE)
save_output_package(outputs, "mwi_outputs_single_csv", "outputs", with_labels = TRUE, single_csv = TRUE)
save_output_package(outputs, "mwi_outputs_single_csv_unlabelled", "outputs", with_labels = FALSE, single_csv = TRUE)


## #' 6. Plot some model outputs

indicators <- add_output_labels(outputs) %>%
  left_join(outputs$meta_area %>% select(area_level, area_id, center_x, center_y)) %>%
  sf::st_as_sf()


## ----fig.height = 4, fig.width = 7---------------------------------------
indicators %>%
  filter(age_group_id == 18,
         indicator_id == 2L,
         area_level == 4) %>%
  ggplot(aes(fill = mode)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)


## ----fig.height = 4, fig.width = 7---------------------------------------
indicators %>%
  filter(age_group_id == 18,
         ## sex == "both",
         indicator_id == 2L,
         area_level == 2) %>%
  ## semi_join(get_area_collection(areas, level = 3, area_scope = "MWI.3")) %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)


## ----fig.height = 5, fig.width = 7---------------------------------------
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
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(1)) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


## ----fig.height = 4, fig.width = 7---------------------------------------
indicators %>%
  filter(age_group_id == 19,
         area_level == 4,
         indicator_id == 4L) %>%
  ggplot(aes(fill = mean)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)


## ----fig.height = 5, fig.width = 7---------------------------------------
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
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format(1)) +
  facet_wrap(~area_name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


## ----fig.height = 4, fig.width = 7---------------------------------------
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


## ----fig.height = 4, fig.width = 7---------------------------------------
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

