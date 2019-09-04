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

#' Spectrum PJNZ

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

ar <- get_area_collection(areas, level = 2)

dat <- mwi_survey_hiv_indicators %>%
  filter(indicator == "prev",
         area_id %in% ar$area_id,
         sex == "both",
         age_group_id == 18,
         survey_id == "MWI2016PHIA")

dat %>%
  left_join(
    area_geom %>% filter(type == "boundary")
  ) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = est)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map()


#' DHS prevalence by district (level 4) within Northern Region (MWI.1)

ar <- get_area_collection(areas, level = 4, area_scope = "MWI.1")

dat <- mwi_survey_hiv_indicators %>%
  filter(indicator == "prev",
         area_id %in% ar$area_id,
         sex == "both",
         age_group_id == 18,
         survey_id == "MWI2015DHS")

dat %>%
  left_join(
    area_geom %>% filter(type == "boundary")
  ) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = est)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map()



#' ### Survey prevalence and sample size
#'

ar <- get_area_collection(areas, level = 4) %>%
  left_join(
    mwi_area_geom %>% filter(type == "boundary")
  ) %>%
  sf::st_as_sf()

dat <- mwi_survey_hiv_indicators %>%
  filter(indicator == "prev",
         area_id %in% ar$area_id,
         sex == "both",
         age_group_id == 18,
         survey_id == "MWI2016PHIA")

dat %>%
  left_join(
    area_geom %>% filter(type == "center_adj")
  ) %>%
  sf::st_as_sf() %>%
  ggplot() +
  geom_sf(data = ar) +
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

area_id_model <- sh$area_id
sex_model <- c("male", "female")
age_group_id_model <- 1:17

construct_df_model <- function(area_id_model, sex_model, age_group_id_model) {

  tidyr::crossing(
           area_id = area_id_model,
           sex = sex_model,
           age_group_id = age_group_id_model
         ) %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
      dplyr::mutate(area_idf = forcats::as_factor(area_id),
                    age_group_idf = forcats::as_factor(age_group_id))
}

df_model <- construct_df_model(sh$area_id, c("male", "female"), age_group_id_model)

df_model <- df_model %>%
  left_join(
    interpolate_population_agesex(pop_agesex, 2016.25) %>%
    select(-source, -iso3)
  )

#' Data frame for outputs
#' 

#' Get age group ids for output
#'
#' Ensures that age groups are fully spanned by modelled
#' ages.
#' 
get_age_group_id_out <- function(age_group_id_model) {

  agegr <- get_age_groups() %>%
    dplyr::filter(age_group_id %in% age_group_id_model)

  age_min <- min(agegr$age_group_start)
  age_max <- max(agegr$age_group_start + agegr$age_group_span)

  val <- dplyr::filter(get_age_groups(),
                       age_group_start >= age_min,
                       is.infinite(age_max) |
                       age_group_start + age_group_span <= age_max)

  val$age_group_id
}
    

area_id_out <- areas$area_id
sex_out <- c("male", "female", "both")
age_group_id_out <- get_age_group_id_out(age_group_id_model)

df_out <-
  crossing(
    area_id = area_id_out,
    sex = sex_out,
    age_group_id = age_group_id_out 
  ) %>%
  mutate(idx_out = row_number())


area_id_join <- get_area_collection(areas, level = level, area_scope = area_id_out) %>%
  rename(area_id_out = area_scope) %>%
  select(-area_level)

stopifnot(area_id_model %in% area_id_join$area_id)

sex_join <- tibble(sex_out = c("male", "female", "both", "both"),
                   sex = c("male", "female", "male", "female"))

age_group_join <- get_age_groups() %>%
  filter(age_group_id %in% age_group_id_out) %>%
  setNames(paste0(names(.), "_out")) %>%
  crossing(get_age_groups() %>%
           filter(age_group_id %in% age_group_id_model)) %>%
  filter(age_group_start_out <= age_group_start,
         age_group_span_out == Inf |
         (age_group_start + age_group_span) <= 
         (age_group_start_out + age_group_span_out)) %>%
  select(age_group_id_out, age_group_id)

stopifnot(age_group_id_model %in% age_group_join$age_group_id)

df_join <- crossing(area_id_join, sex_join, age_group_join) %>%
  full_join(df_model) %>%
  full_join(df_out, by = c("area_id_out" = "area_id", "sex_out" = "sex", "age_group_id_out" = "age_group_id")) %>%
  select(idx_out, idx) %>%
  mutate(x = 1)

A_out <- Matrix::spMatrix(nrow(df_out),
                          nrow(df_model),
                          df_join$idx_out,
                          df_join$idx,
                          df_join$x)


##   get_area_collection(areas, level = level, area_scope = areas$area_id) %>%
  
  
#' ### Survey data
#'

prev_dat <- df_model %>%
  inner_join(
    mwi_survey_hiv_indicators %>%
    filter(survey_id %in% surveys,
           indicator == "prev")
  ) %>%
  mutate(n = n_obs,
         x = n * est) %>%
  select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)

artcov_dat <- df_model %>%
  inner_join(
    mwi_survey_hiv_indicators %>%
    filter(survey_id %in% surveys,
           indicator == "artcov")
  ) %>%
  mutate(n = n_obs,
         x = n * est) %>%
  select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)

#' 5. Fit model
#'
#' Note: useful for how to include multiple TMB models: https://stackoverflow.com/questions/48627069/guidelines-for-including-tmb-c-code-in-an-r-package

df <- df_model

dtmb <- list(
  population = df$population,
  X_rho = model.matrix(~as.integer(sex == "female"), df),
  X_alpha = model.matrix(~as.integer(sex == "female"), df),
  Z_x = model.matrix(~0 + area_idf, df),
  Z_a = model.matrix(~0 + age_group_idf, df),
  Z_xs = model.matrix(~0 + area_idf, df) * (df$sex == "female"),
  Z_as = model.matrix(~0 + age_group_idf, df) * (df$sex == "female"),
  Z_xa = model.matrix(~0 + area_idf:age_group_idf, df),
  Q_x = Q_scaled,
  A_out = A_out,
  idx_prev = prev_dat$idx - 1L,
  x_prev = prev_dat$x,
  n_prev = prev_dat$n,
  idx_artcov = artcov_dat$idx - 1L,
  x_artcov = artcov_dat$x,
  n_artcov = artcov_dat$n)

ptmb <- list(
  beta_rho = numeric(ncol(dtmb$X_rho)),
  beta_alpha = numeric(ncol(dtmb$X_alpha)),
  us_rho_x = numeric(ncol(dtmb$Z_x)),
  ui_rho_x = numeric(ncol(dtmb$Z_x)),
  us_rho_xs = numeric(ncol(dtmb$Z_xs)),
  ui_rho_xs = numeric(ncol(dtmb$Z_xs)),
  u_rho_a = numeric(ncol(dtmb$Z_a)),
  u_rho_as = numeric(ncol(dtmb$Z_a)),
  logit_phi_rho_a = 0,
  log_sigma_rho_a = 0,
  logit_phi_rho_as = 0,
  log_sigma_rho_as = 0,
  logit_phi_rho_x = 0,
  log_sigma_rho_x = 0,
  logit_phi_rho_xs = 0,
  log_sigma_rho_xs = 0
)

obj <- TMB::MakeADFun(data = dtmb, parameters = ptmb, DLL = "naomi", silent = TRUE,
                      random = c("us_rho_x", "ui_rho_x",
                                 "us_rho_xs", "ui_rho_xs",
                                 "u_rho_a", "u_rho_as"))

                 
obj$control = list(trace = 4, maxit = 1000, REPORT = 1)

nlminb(obj$par, obj$fn, obj$gr, control = list(trace = 1))

system.time(
  ftmb <- TMB::sdreport(obj, bias.correct = FALSE,
                        bias.correct.control = list(sd = FALSE))
)




#' 6. Plot some model outputs

## summary(ftmb)

df_out <- df_out %>%
  left_join(areas %>% select(area_id, area_level), by = "area_id")

df_out$prev <- ftmb$value[names(ftmb$value) == "rho_out"]
df_out$artcov <- ftmb$value[names(ftmb$value) == "alpha_out"]

#' 15-49 prevalence by district
df_out %>%
  filter(age_group_id == 18,
         area_level == 4) %>%
  left_join(boundaries, by = "area_id") %>%
  st_as_sf() %>%
  ggplot(aes(fill = prev)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

#' 15-49 prevalence by 28 districts, Southern region
#'

df_out %>%
  filter(age_group_id == 18,
         area_level == 3) %>%
  semi_join(get_area_collection(areas, level = 3, area_scope = "MWI.3")) %>%
  left_join(boundaries, by = "area_id") %>%
  st_as_sf() %>%
  ggplot(aes(fill = prev)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

df_out %>%
  filter(area_level == 1,
         sex != "both",
         age_group_id %in% 1:17) %>%
  ggplot(aes(age_group_id, prev, fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~area_id)


#' 15-64 ART coveragte
df_out %>%
  filter(age_group_id == 19,
         area_level == 4) %>%
  left_join(boundaries, by = "area_id") %>%
  st_as_sf() %>%
  ggplot(aes(fill = artcov)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)



out %>%
  filter(sex == "female") %>%
  left_join(boundaries) %>%
  st_as_sf() %>%
  ggplot(aes(fill = prev)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~age_group_id, nrow = 1)

out %>%
  left_join(boundaries) %>%
  st_as_sf() %>%
  ggplot(aes(fill = prev)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_grid(sex~age_group_id)


out %>%
  filter(area_id == "MWI.1.1.1.1") %>%
  ggplot(aes(age_group_id, prev, fill = sex)) +
  geom_col(position = "dodge")


survey_hiv_indicators %>%
  filter(age_group_id %in% 1:17, sex == "male",
         survey_id %in% surveys,
         indicator == "prev") %>%
  filter(area_id == "MWI.3") %>%
  ggplot(aes(age_group_id, est, fill = survey_id)) +
  geom_col(position = "dodge")
