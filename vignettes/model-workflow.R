


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

area_meta <- read_csv(system.file("extdata/areas/area_meta.csv", package = "naomi"))
area_names  <- read_csv(system.file("extdata/areas/area_names.csv", package = "naomi"))
area_hierarchy  <- read_csv(system.file("extdata/areas/area_hierarchy.csv", package = "naomi"))
area_boundaries <- sf::read_sf(system.file("extdata/areas/area_boundaries.geojson", package = "naomi"))
area_centers <- sf::read_sf(system.file("extdata/areas/area_centers.geojson", package = "naomi"))

area_long <- area_hierarchy %>%
  left_join(
    area_meta %>% select(iso3, area_level, area_level_label, display, naomi_level)
  ) %>%
  left_join(
    area_names
  ) %>%
  left_join(
    area_boundaries
  ) %>%
  left_join(
    area_centers %>%
    as.data.frame() %>%
    rename(centers = geometry)
  ) %>%
  mutate(
    center_x = st_coordinates(centers)[,1],
    center_y = st_coordinates(centers)[,2],
    centers = NULL
  ) %>%
  select(-geometry, everything(), geometry)


st_write(area_long, file.path(tempdir(), "area_long.geojson"), delete_dsn = TRUE)

#' # 1. (Up)Load data inputs
#'
#' Area hierarchy and boundaries

area_long <- read_sf(file.path(tempdir(), "area_long.geojson"))

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

##+
iso3 <- "MWI"
level <- 4
survey_ids  <- c("MWI2016PHIA", "MWI2015DHS")

artnum_quarter_id_t1 <- convert_quarter_id(1, 2016)
anc_quarter_id_t1 <- convert_quarter_id(c(4, 1, 2, 3), c(2015, 2016, 2016, 2016))

artnum_quarter_id_t2 <- convert_quarter_id(3, 2018)
anc_quarter_id_t2 <- convert_quarter_id(1:4, 2018)

areas <- create_areas(area_meta, area_hierarchy, area_names, area_boundaries)



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
#'
#' ### Areas
#'
#' Define areas


#' ### State space
#'
#' Data frame for full space stratification


sexes <- c("male", "female")
age_group_ids <- 1:17

quarter_id1 <- naomi::convert_quarter_id(1, 2016)
quarter_id2 <- naomi::convert_quarter_id(3, 2018)

naomi_mf <- naomi_model_frame(areas, pop_agesex, spec,
                              level = 4, quarter_id1, quarter_id2,
                              age_group_ids = age_group_ids,
                              sexes = sexes)


#' # ART attendance model

adj_ij <- (naomi_mf$M + diag(nrow(naomi_mf$M))) %>%
  as("dgCMatrix") %>%
  Matrix::summary() %>%
  dplyr::mutate(x = NULL,
                istar = as.integer(i == j),
                jstar = as.integer(i == j)) %>%
  dplyr::arrange(i, istar, j, jstar) %>%
  dplyr::mutate(idx_ij = dplyr::row_number(),
                idf_ij = as_factor(idx_ij))

n_nb <- colSums(naomi_mf$M)

gamma_or_prior <- adj_ij %>%
  filter(istar == 0) %>%
  left_join(
    data.frame(i = seq_along(n_nb),
               n_nb_lim = pmin(n_nb, 9))
  ) %>%
  left_join(
    data.frame(
      n_nb_lim = 1:9,
      gamma_or_mu = c(-3.29855798975623, -4.0643930585428, -4.53271592818956, -4.86910480099925, -5.13133396982624,
                      -5.34605339546364, -5.52745113789738, -5.68479564118418, -5.8234349424758),
      gamma_or_sigma = c(0.950818503595947, 1.04135785601697, 1.12665887287997, 1.19273171464978, 1.24570962739274,
                         1.28959773294666, 1.32675564121864, 1.35902556091841, 1.3873644912272)
    )
  )


df_art_attend <- naomi_mf$mf_model %>%
  left_join(adj_ij, by = c("area_idx" = "i"))

Xart_gamma <- Matrix::sparse.model.matrix(~0 + idf_ij, df_art_attend)
Xart_idx <- Matrix::sparse.model.matrix(~0 + as_factor(idx), df_art_attend)


#' # Incidence model

omega <- 0.7

naomi_mf$mf_model <- naomi_mf$mf_model %>%
  mutate(age15to49 = as.integer(age_group_id %in% 4:10)) %>%
  group_by(area_id) %>%
  mutate(
    spec_prev15to49 = sum(population_t1 * spec_prev) / sum(population_t1),
    spec_artcov15to49 =
      sum(population_t1 * spec_prev * spec_artcov) /
      sum(population_t1 * spec_prev),
    log_lambda_offset =
      log(spec_incid) - log(spec_prev15to49) - log(1 - omega * spec_artcov15to49)
  ) %>%
  ungroup

X_15to49 <- Matrix::t(Matrix::sparse.model.matrix(~-1 + area_idf:age15to49, naomi_mf$mf_model))

## rho <- plogis(obj$report()$mu_rho)
## alpha <- plogis(obj$report()$mu_alpha)

## pop15to49 <- df$population_t1 %*% X_15to49
## plhiv15to49 <- (rho * df$population_t1) %*% X_15to49
## artnum15to49 <- (alpha * rho * df$population_t1) %*% X_15to49

## rho15to49 <- c(plhiv15to49 / pop15to49)
## alpha15to49 <- c(artnum15to49 / plhiv15to49)

## mf$mf_model$log_lambda_offset + dtmb$Z_x %*% (log(rho15to49) + log(1 - omega * alpha15to49))

#' ### Survey data
#'

prev_dat <- survey_prevalence_mf(survey_ids, survey_hiv_indicators, naomi_mf)
artcov_dat <- survey_artcov_mf(survey_ids, survey_hiv_indicators, naomi_mf)
recent_dat <- survey_recent_mf(survey_ids, survey_hiv_indicators, naomi_mf)

anc_dat <-
  anc_testing %>%
  filter(quarter_id %in% anc_quarter_id_t1) %>%
  group_by(area_id) %>%
  summarise_at(vars(ancrt_hiv_status, ancrt_known_pos, ancrt_test_pos, ancrt_already_art), sum, na.rm = TRUE) %>%
  mutate(ancrt_totpos = ancrt_known_pos + ancrt_test_pos) %>%
  transmute(
    area_id,
    anc_idx = row_number(),
    anc_prev_x = ancrt_totpos,
    anc_prev_n = ancrt_hiv_status,
    anc_artcov_x = ancrt_already_art,
    anc_artcov_n = ancrt_totpos
  ) %>%
  left_join(distinct(naomi_mf$mf_model, area_id, area_idf))


A_anc_prev <- anc_dat %>%
  inner_join(
    naomi_mf$mf_model %>%
    transmute(
      area_id,
      idx,
      births = asfr * population_t1
    ) %>%
    filter(births > 0)
  ) %>%
  {
    Matrix::spMatrix(nrow(anc_dat),
                     nrow(naomi_mf$mf_model),
                     .$anc_idx,
                     .$idx,
                     .$births)
  }

A_anc_artcov <- A_anc_prev


artnum_dat <- art_number %>%
  filter(quarter_id == artnum_quarter_id_t1,
         if(!1 %in% naomi_mf$mf_model$age_group_id) age_group_id == 20 else  TRUE) %>%
  mutate(artnum_idx = row_number())

A_artnum <- artnum_dat %>%
  left_join(
    naomi_mf$mf_areas %>% dplyr::select(area_id, area_idx)
  ) %>%
  inner_join(
    df_art_attend %>%
    transmute(
      j,
      age_group_id = if_else(age_group_id %in% 1:3, 24L, 20L),  ## HARD CODED
      sex = "both",                                             ## HARD CODED
      art_attend_idx = row_number(),
      value = 1
    ),
    by = c("area_idx" = "j", "sex" = "sex", "age_group_id" = "age_group_id")
  ) %>%
  {
    Matrix::spMatrix(nrow(artnum_dat),
                     nrow(df_art_attend),
                     .$artnum_idx,
                     .$art_attend_idx,
                     .$value)
  }



#' 5. Fit model
#'
#' Note: useful for how to include multiple TMB models: https://stackoverflow.com/questions/48627069/guidelines-for-including-tmb-c-code-in-an-r-package

fit_tmb <- function(mf) {

  df <- mf$mf_model

  dtmb <- list(
    population = df$population_t1,
    X_rho = model.matrix(~as.integer(sex == "female"), df),
    X_alpha = model.matrix(~as.integer(sex == "female"), df),
    X_ancrho = model.matrix(~1, anc_dat),
    X_ancalpha = model.matrix(~1, anc_dat),
    Z_x = Matrix::sparse.model.matrix(~0 + area_idf, df),
    Z_a = Matrix::sparse.model.matrix(~0 + age_group_idf, df),
    Z_xs = Matrix::sparse.model.matrix(~0 + area_idf, df) * (df$sex == "female"),
    Z_as = Matrix::sparse.model.matrix(~0 + age_group_idf, df) * (df$sex == "female"),
    ## Z_xa = Matrix::sparse.model.matrix(~0 + area_idf:age_group_idf, df),
    Z_ancrho_x = Matrix::sparse.model.matrix(~0 + area_idf, anc_dat),
    Z_ancalpha_x = Matrix::sparse.model.matrix(~0 + area_idf, anc_dat),
    ##
    Q_x = as(mf$Q, "dgCMatrix"),
    n_nb = n_nb,
    adj_i = adj_ij$i - 1L,
    adj_j = adj_ij$j - 1L,
    gamma_or_mu = gamma_or_prior$gamma_or_mu,
    gamma_or_sigma = 2.0 * gamma_or_prior$gamma_or_sigma,
    Xart_idx = Xart_idx,
    Xart_gamma = Xart_gamma,
    ##
    omega = omega,
    X_15to49 = X_15to49,
    log_lambda_offset = mf$mf_model$log_lambda_offset,
    ##
    A_out = mf$A_out,
    idx_prev = prev_dat$idx - 1L,
    x_prev = prev_dat$x,
    n_prev = prev_dat$n,
    idx_artcov = artcov_dat$idx - 1L,
    x_artcov = artcov_dat$x,
    n_artcov = artcov_dat$n,
    A_anc_prev = A_anc_prev,
    x_anc_prev = anc_dat$anc_prev_x,
    n_anc_prev = anc_dat$anc_prev_n,
    A_anc_artcov = A_anc_artcov,
    x_anc_artcov = anc_dat$anc_artcov_x,
    n_anc_artcov = anc_dat$anc_artcov_n,
    ##
    A_artnum = A_artnum,
    x_artnum = artnum_dat$current_art
  )


  ptmb <- list(
    beta_rho = numeric(ncol(dtmb$X_rho)),
    beta_alpha = numeric(ncol(dtmb$X_alpha)),
    beta_anc_rho = numeric(1),
    beta_anc_alpha = numeric(1),
    us_rho_x = numeric(ncol(dtmb$Z_x)),
    ui_rho_x = numeric(ncol(dtmb$Z_x)),
    us_rho_xs = numeric(ncol(dtmb$Z_xs)),
    ui_rho_xs = numeric(ncol(dtmb$Z_xs)),
    u_rho_a = numeric(ncol(dtmb$Z_a)),
    u_rho_as = numeric(ncol(dtmb$Z_a)),
    ui_anc_rho_x = numeric(ncol(dtmb$Z_x)),
    ui_anc_alpha_x = numeric(ncol(dtmb$Z_x)),
    ##
    us_alpha_x = numeric(ncol(dtmb$Z_x)),
    ui_alpha_x = numeric(ncol(dtmb$Z_x)),
    us_alpha_xs = numeric(ncol(dtmb$Z_xs)),
    ui_alpha_xs = numeric(ncol(dtmb$Z_xs)),
    u_alpha_a = numeric(ncol(dtmb$Z_a)),
    u_alpha_as = numeric(ncol(dtmb$Z_a)),
    ##
    logit_phi_rho_a = 0,
    log_sigma_rho_a = 0,
    logit_phi_rho_as = 0,
    log_sigma_rho_as = 0,
    logit_phi_rho_x = 0,
    log_sigma_rho_x = 0,
    logit_phi_rho_xs = 0,
    log_sigma_rho_xs = 0,
    ##
    logit_phi_alpha_a = 0,
    log_sigma_alpha_a = 0,
    logit_phi_alpha_as = 0,
    log_sigma_alpha_as = 0,
    logit_phi_alpha_x = 0,
    log_sigma_alpha_x = 0,
    logit_phi_alpha_xs = 0,
    log_sigma_alpha_xs = 0,
    ##
    log_sigma_ancrho_x = 0,
    log_sigma_ancalpha_x = 0,
    ##
    oddsratio_gamma_art_raw = numeric(sum(dtmb$n_nb))
  )

  obj <- TMB::MakeADFun(data = dtmb, parameters = ptmb, DLL = "naomi", silent = TRUE,
                        random = c("us_rho_x", "ui_rho_x",
                                   "us_rho_xs", "ui_rho_xs",
                                   "u_rho_a", "u_rho_as",
                                   ##
                                   "us_alpha_x", "ui_alpha_x",
                                   "us_alpha_xs", "ui_alpha_xs",
                                   "u_alpha_a", "u_alpha_as",
                                   ##
                                   "ui_anc_rho_x", "ui_anc_alpha_x",
                                   ##
                                   "oddsratio_gamma_art_raw"))

  f <- nlminb(obj$par, obj$fn, obj$gr, control = list(trace = 1))

  f$par.fixed <- f$par
  f$par.full <- obj$env$last.par

  val <- c(f, obj = list(obj))
  class(val) <- "naomi_fit"

  val
}

#' Fit the TMB model
fit <- fit_tmb(naomi_mf)

#' Calculate model outputs. We can calculate outputs based on posterior mode
#' estimates before running `report_tmb()` to calculate posterior intervals.

outputs <- output_package(fit, naomi_mf, areas)

#' The output package consists of a data frame of indicators and metadata
#' defining the labels for each indicator.
names(outputs)

#' If uncertainty has not been calcualted yet, the output object retures values
#' for `mode`, but not `mean` or `lower` and `upper` 95% uncertainty ranges.

outputs$indicators %>%
  filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%  
  head()

#' The function `add_output_labels()` returns the indicators table
#' with labels added as additional columns.
add_output_labels(outputs) %>%
  filter(
    indicator_id == 2L,  # HIV prevalence
    age_group_id == 18   # Age group 15-49
  ) %>%  
  head()


#' Calculate uncertainty ranges and add to the output object
#' (This is time consuming and memory intensive.
system.time(naomi_fit <- report_tmb(fit))

#' Regenerate outputs with uncertainty ranges.
outputs <- output_package(naomi_fit, naomi_mf, areas)

outputs$indicators %>%
  filter(
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
  ggplot(aes(fill = mode)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)

indicators %>%
  filter(area_level == 1,
         sex != "both",
         age_group_id %in% 1:17,
         indicator_id == 2L) %>%
  left_join(get_age_groups()) %>%
  mutate(age_group = fct_reorder(age_group_label, age_group_id)) %>%
  ggplot(aes(age_group, mode, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  facet_wrap(~area_id) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5))


#' 15-64 ART coverage
indicators %>%
  filter(age_group_id == 19,
         area_level == 4,
         indicator_id == 4L) %>%
  ggplot(aes(fill = mode)) +
  geom_sf() +
  viridis::scale_fill_viridis(labels = scales::percent_format()) +
  th_map() +
  facet_wrap(~sex)


#' Bubble plot prevalence and PLHIV
indicators %>%
  filter(age_group_id == 19,
         area_level == 4,
         indicator_id %in% 2:3) %>%
  select(sex, center_x, center_y, indicator_label, mode) %>%
  spread(indicator_label, mode) %>%
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
