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
iso3 <- "MWI"
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


#' # 4. Prepare model inputs
#'
#' ### Areas
#' 
#' Define areas
sh <- mwi_areas %>%
  filter(iso3 == !!iso3, area_level == level) %>%
  select(-parent_area_id) %>%
  arrange(area_sort_order) %>%
  mutate(area_idx = row_number())

boundaries <- area_geom %>%
  filter(type == "boundary")
  
#' Neighbor list
nb <- sh %>%
  left_join(boundaries) %>%
  st_as_sf() %>%
  as("Spatial") %>%
  spdep::poly2nb() %>%
  `names<-`(sh$area_idx)

#' Adjacency matrix

M <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
colnames(M) <- rownames(M)

#' Precision matrix for ICAR model
Q <- diag(rowSums(M)) - M

#' Scaled precision matrix for 'BYM2' model.
Q_scaled  <- as.matrix(INLA::inla.scale.model(Q, constr = list(A = matrix(1, 1, nrow(Q)), e = 0)))

#' ### State space
#' 
#' Data frame for full space stratification
df <- sh %>%
  crossing(sex = c("male", "female"),
           age_group_id = 1:17) %>%
  mutate(idx = row_number()) %>%
  mutate(area_idf = as_factor(area_id),
         age_grou_idf = as_factor(age_group_id))

#' ### Survey data
#'

prev_dat <- df %>%
  inner_join(
    mwi_survey_hiv_indicators %>%
    filter(survey_id %in% surveys,
           indicator == "prev")
  ) %>%
  mutate(n = n_obs,
         x = n * est) %>%
  select(idx, area_id, area_idx, age_group_id, sex, n, x, est, se)

#' 5. Fit model
#'
#' Note: useful for how to include multiple TMB models: https://stackoverflow.com/questions/48627069/guidelines-for-including-tmb-c-code-in-an-r-package

dtmb <- list(X = model.matrix(~0, df), ## model.matrix(~sex, df),
             Z_area = model.matrix(~0 + area_idf, df),
             Z_age = model.matrix(~0 + age_group_idf, df),
             Z_area_sex = model.matrix(~0 + area_idf, df) * (df$sex == "female"),
             Z_age_sex = model.matrix(~0 + age_group_idf, df) * (df$sex == "female"),
             Z_area_age = model.matrix(~0 + area_idf:agegr_idf, df),
             Q_area = Q_scaled,
             idx_prev = prev_dat$idx - 1L,
             x_prev = prev_dat$x,
             n_prev = prev_dat$n)

ptmb <- list(
  beta = numeric(ncol(dtmb$X)),
  us_area = numeric(ncol(dtmb$Z_area)),
  ui_area = numeric(ncol(dtmb$Z_area)),
  us_area_sex = numeric(ncol(dtmb$Z_area_sex)),
  ui_area_sex = numeric(ncol(dtmb$Z_area_sex)),
  u_age = numeric(ncol(dtmb$Z_age)),
  u_age_sex = numeric(ncol(dtmb$Z_age)),
  logit_phi_age = 0,
  log_sigma_age = 0,
  logit_phi_age_sex = 0,
  log_sigma_age_sex = 0,
  logit_phi_area = 0,
  log_sigma_area = 0,
  logit_phi_area_sex = 0,
  log_sigma_area_sex = 0
)

obj <- TMB::MakeADFun(data = dtmb, parameters = ptmb, DLL = "naomi", silent = TRUE,
                      random = c("us_area", "ui_area",
                                 "us_area_sex", "ui_area_sex",
                                 "u_age", "u_age_sex"))

                 
obj$control = list(trace = 4, maxit = 1000, REPORT = 1)

nlminb(obj$par, obj$fn, obj$gr, control = list(trace = 1))
ftmb <- sdreport(obj, bias.correct = TRUE, bias.correct.control = list(sd = TRUE))

#' 6. Plot some model outputs

summary(ftmb)

out <- df %>%
  mutate(prev = plogis(obj$report()$mu_rho))

out %>%
  filter(age_group_id == 6, sex == "female") %>%
  left_join(boundaries) %>%
  st_as_sf() %>%
  ggplot(aes(fill = prev)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map()
  
