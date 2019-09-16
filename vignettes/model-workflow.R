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

#'
#' # 1. (Up)Load data inputs
#'
#' Area hierarchy and boundaries
##+ load_area_data, message = FALSE
area_meta <- read_csv(system.file("extdata/areas/area_meta.csv", package = "naomi"))
areas <- read_csv(system.file("extdata/areas/areas.csv", package = "naomi"))
area_geom <- sf::st_read(system.file("extdata/areas/area_geom.geojson", package = "naomi"))

#' Population data
##+ load_population_data, message = FALSE
pop_agesex <- read_csv(system.file("extdata/population/population_agesex.csv", package = "naomi"))

#' Survey data
##+ load_survey_data, message = FALSE
survey_hiv_indicators <- read_csv(system.file("extdata/survey/survey_hiv_indicators.csv", package = "naomi"))

#' Programme data
#'

#' # 2. Choose inputs

#'### Choose data to include

##+
level <- 4
surveys <- c("MWI2016PHIA", "MWI2015DHS")


#' # 3. Review input data
#'
#' ### Survey prevalence chlorpleth
#'
#' PHIA prevalence by Zone (level 2)

areas <- get_area_collection(mwi_areas, level = 2)

dat <- mwi_survey_hiv_indicators %>%
  filter(indicator == "prev",
         area_id %in% areas$area_id,
         sex == "both",
         age_group_id == 18,
         survey_id == "MWI2016PHIA")

dat %>%
  left_join(
    mwi_area_geom %>% filter(type == "boundary")
  ) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = est)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map()


#' DHS prevalence by district (level 4) within Northern Region (MWI.1)

areas <- get_area_collection(mwi_areas, level = 4, area_scope = "MWI.1")

dat <- mwi_survey_hiv_indicators %>%
  filter(indicator == "prev",
         area_id %in% areas$area_id,
         sex == "both",
         age_group_id == 18,
         survey_id == "MWI2015DHS")

dat %>%
  left_join(
    mwi_area_geom %>% filter(type == "boundary")
  ) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = est)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map()



#' ### Survey prevalence and sample size
#'

areas <- get_area_collection(mwi_areas, level = 4) %>%
  left_join(
    mwi_area_geom %>% filter(type == "boundary")
  ) %>%
  sf::st_as_sf()

dat <- mwi_survey_hiv_indicators %>%
  filter(indicator == "prev",
         area_id %in% areas$area_id,
         sex == "both",
         age_group_id == 18,
         survey_id == "MWI2016PHIA")

dat %>%
  left_join(
    mwi_area_geom %>% filter(type == "center_adj")
  ) %>%
  sf::st_as_sf() %>%
  ggplot() +
  geom_sf(data = areas) +
  geom_sf(aes(color = est, size = n_obs), show.legend = "point") +
  viridis::scale_color_viridis(labels = scales::percent_format()) +
  scale_size_area() +
  th_map()

