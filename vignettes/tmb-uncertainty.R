#' ---
#' title: "TMB Uncertainty Validation"
#' output: rmarkdown::html_vignette
#' ---

#' ## Libraries
##+ message = FALSE, warnings = FALSE
library(tidyverse)
library(sf)
library(INLA)
library(TMB)
library(tmbstan)
library(here)
devtools::load_all()  # https://github.com/mrc-ide/naomi@tmb-uncertainty

#' ## Data inputs

##+ message = FALSE
area_levels <- read_csv(system.file("extdata/areas/area_levels.csv", package = "naomi"))
area_hierarchy  <- read_csv(system.file("extdata/areas/area_hierarchy.csv", package = "naomi"))
area_boundaries <- sf::read_sf(system.file("extdata/areas/area_boundaries.geojson", package = "naomi"))

pop_agesex <- read_csv(system.file("extdata/population/population_agesex.csv", package = "naomi"))
survey_hiv_indicators <- read_csv(system.file("extdata/survey/survey_hiv_indicators.csv", package = "naomi"))


areas <- create_areas(area_levels, area_hierarchy, area_boundaries)
pop2016 <- interpolate_population_agesex(pop_agesex, convert_quarter_id(1, 2016))

#' # 1. BYM2 area model for 15-49 prevalence
#'
#' 
#' $$y_x \sim Binomial(n_x, logit^{-1}(\mu_x))$$
#' $$\mu_x = \beta_0 + u_x$$
#' $$ u_x \sim BYM2(\sigma, \phi)$$
#'
#' Non-centered parameterisation: Sample the parameters $u^s_x$ and $u^i_x$ as
#' 
#' $$p(u^s_x) \propto exp(-0.5 \cdot {u^s_x}^\prime\ Q\ u^s_x)$$
#' $$u^i_x \sim Normal(0, 1)$$
#'
#' and calculate spatial effect $u_x = \sigma \cdot (\sqrt{\phi}\cdot u^s_x + \sqrt{1-\phi} \cdot u^i_x)$.
#'
#' INLA parameterisation: see `inla.doc("bym2")`.
#'
#' ## Prepare inputs
#' 
#' ### Area data frame
#'
#' Remove Likoma island to avoid differences in island constraints distoring
#' things.

area_id <- areas$tree$Get("area_id", filterFun = data.tree::isLeaf) %>%
  setdiff("MWI.1.1.6.7")

mf_areas <- data.frame(area_id,
                       area_idx = seq_along(area_id),
                       stringsAsFactors = FALSE) %>%
  mutate(area_idf = factor(area_id, area_id))

#' Join age 15-49 prevlance and population size to model frame.

##+ message = FALSE
mfa <- mf_areas %>%
  mutate(sex = "both",
         age_group_id = 18) %>%
  left_join(
    survey_hiv_indicators %>%
    filter(indicator == "prev", survey_id == "MWI2016PHIA")
  ) %>%
  mutate(x = est * n_obs) %>%
  left_join(
    pop2016 %>%
    filter(age_group_id %in% 4:10) %>%
    count(area_id, wt = population, name = "population")
  )


#' Construct adjacency matrix from shapefile.

M <- mf_areas %>%
  dplyr::mutate(geometry = areas$boundaries[area_id]) %>%
  sf::st_as_sf() %>%
  methods::as("Spatial") %>%
  spdep::poly2nb(.$area_id) %>%
  spdep::nb2mat(style = "B", zero.policy = TRUE)

colnames(M) <- rownames(M)

#' Scaled precision matrix for BYM2
Q  <- inla.scale.model(diag(rowSums(M)) - M,
                       constr = list(A = matrix(1, 1, nrow(M)), e = 0))


#' Outputs model frame and linear transformation matrix.

out_mfa <- mfa %>%
  mutate(idx = area_idx) %>%
  naomi_output_frame(areas, drop_partial_areas = FALSE)


#' ## Fit models
#'
#' ### INLA
##+ fit_inla, cache = TRUE
f_inla <- inla(x ~ f(area_idx, model = "bym2", graph = M),
          family = "binomial", data = mfa, Ntrials = n_obs,
          control.predictor=list(link=NA),
          control.compute=list(config = TRUE))

#' Sample from posterior distribution
smp_inla <- inla.posterior.sample(1000, f_inla, intern = TRUE)

#' Simulate logit prevalence and output prevalence
mu_rho_inla <- lapply(smp_inla, "[[", "latent") %>%
  vapply("[", numeric(nrow(mfa)), seq_len(nrow(mfa)))

plhiv_inla <- plogis(mu_rho_inla) * mfa$population
rho_out_inla <- (out_mfa$A %*% plhiv_inla) / (out_mfa$A %*% mfa$population)

sim_inla <- list(mu_rho = mu_rho_inla,
                 rho_out = rho_out_inla)


#' ### TMB, non-centered parameterization
#'
#' Compile and load TMB model.

##+ message = FALSE, results = "hide"
compile(here("vignettes/tmb-uncertainty_src/area_noncentered.cpp"))
dyn.load(dynlib(here("vignettes/tmb-uncertainty_src/area_noncentered")))

#' Prepare TMB input data and parameter initial values 

tmb_prev_dat <- mfa %>%
  filter(!is.na(x)) %>%
  {list(idx_prev = .$area_idx - 1L,
        n_prev = .$n_obs,
        x_prev = .$x)}

tmb_data <- c(list(population = mfa$population,
                   X_rho = model.matrix(~1, mfa),
                   Z_x = sparse.model.matrix(~0 + area_idf, mfa),
                   Q_x = Q,
                   A_out = out_mfa$A),
              tmb_prev_dat)

tmb_init <- list(beta_rho = numeric(ncol(tmb_data$X_rho)),
                 logit_phi_rho_x = 0,
                 log_sigma_rho_x = 0,
                 us_rho_x = numeric(ncol(tmb_data$Z_x)),
                 ui_rho_x = numeric(ncol(tmb_data$Z_x)))

#' Fit TMB model.
##+ message = FALSE
obj_nc <- TMB::MakeADFun(data = tmb_data,
                         parameters = tmb_init,
                         DLL = "area_noncentered",
                         silent = TRUE,
                         random = c("beta_rho", "us_rho_x", "ui_rho_x"))

fit_nc <- with(obj_nc, nlminb(par, fn, gr, control = list(trace = 1)))

#' Sample from TMB posterior distribution
sim_nc_random <- sample_tmb(obj_nc, random_only = TRUE)
sim_nc_full <- sample_tmb(obj_nc, random_only = FALSE)

#' ADREPORT outputs

sdr_nc <- sdreport(obj_nc, fit_nc$par,
                   bias.correct = TRUE,
                   bias.correct.control = list(sd = TRUE))

#' ### tmbstan

#' Fit model with Stan HMC

##+ fit_tmbstan, cache = TRUE
fit_nc_tmbstan <- tmbstan(obj_nc, refresh = 0, cores = 4)

#' Simulate outputs from HMC samples
sim_nc_tmbstan <- as.matrix(fit_nc_tmbstan)[ , seq_along(obj_nc$env$par)] %>%
  apply(1, obj_nc$report) %>%
  {Map(sapply, list(.), "[[", names(obj_nc$report()))} %>%
  setNames(names(obj_nc$report()))

#' ## Compare results

estci <- function(x) {
  qtl <- apply(x, 1, quantile, c(0.5, 0.025, 0.975))
  data.frame(
    mean = rowMeans(x),
    sd = apply(x, 1, sd),
    median = qtl[1,],
    lower = qtl[2,],
    upper = qtl[3,]
  )
}
  
mu_rho <- mfa %>%
  select(area_id, area_idx) %>%
  {bind_rows(
     bind_cols(mutate(., model = "INLA"),
               f_inla$summary.linear.predictor %>%
               transmute(mean,
                         sd,
                         median = `0.5quant`,
                         lower = `0.025quant`,
                         upper = `0.975quant`)
               ),
     bind_cols(mutate(., model = "TMB, joint precision"), estci(sim_nc_full$mu_rho)),
     bind_cols(mutate(., model = "TMB, random only"), estci(sim_nc_random$mu_rho)),
     bind_cols(mutate(., model = "tmbstan"), estci(sim_nc_tmbstan$mu_rho)),
     mutate(.,
            model = "TMB, sdreport",
            mean = sdr_nc$unbiased$value[names(sdr_nc$unbiased$value) == "mu_rho"],
            sd = sdr_nc$unbiased$sd[names(sdr_nc$unbiased$value) == "mu_rho"])
   )}

rho_out <- out_mfa$mf %>%
  {bind_rows(
     bind_cols(mutate(., model = "INLA"), estci(sim_inla$rho_out)),
     bind_cols(mutate(., model = "TMB, joint precision"), estci(sim_nc_full$rho_out)),
     bind_cols(mutate(., model = "TMB, random only"), estci(sim_nc_random$rho_out)),
     bind_cols(mutate(., model = "tmbstan"), estci(sim_nc_tmbstan$rho_out)),
     mutate(.,
            model = "TMB, sdreport",
            mean = sdr_nc$unbiased$value[names(sdr_nc$unbiased$value) == "rho_out"],
            sd = sdr_nc$unbiased$sd[names(sdr_nc$unbiased$value) == "rho_out"])
   )}

#' #### National prevalence estimate and uncertainty

##+ fig.height = 3, fig.width = 6
rho_out %>%
  filter(area_id == "MWI") %>%
  ggplot(aes(model, mean, ymin = lower, ymax = upper, fill = model)) +
  geom_col() +
  geom_linerange() +
  ylim(0, 0.12) +
  theme(axis.text.x = element_text(hjust = 1, angle = 15)) +
  ggtitle("National prevalence (95% range)")

rho_out %>%
  filter(area_id == "MWI") %>%
  ggplot(aes(model, sd, fill = model)) +
  geom_col() +
  theme(axis.text.x = element_text(hjust = 1, angle = 15)) +
  ggtitle("SE of national prevalence")


#' #### Linear predictor (logit scale)

df <- mu_rho %>%
  select(area_id, model, mean, sd) %>%
  gather(stat, value, mean, sd) %>%
  spread(model, value) %>%
  gather(model, value,
         `INLA`,
         `TMB, joint precision`,
         `TMB, random only`,
         `TMB, sdreport`)

##+ fig.width = 7, fig.height = 3, fig.align = "center"
df %>%
  filter(stat == "mean") %>%
  ggplot(aes(tmbstan, value)) +
  geom_abline(intercept = 0, slope = 1, color = "darkred") +
  geom_point() +
  facet_wrap(~model, nrow = 1) +
  coord_fixed() +
  ggtitle("Linear predictor: mean",
          "Each model (y-axis) compared to `tmbstan` HMC (x-axis)")

df %>%
  filter(stat == "sd") %>%
  ggplot(aes(tmbstan, value)) +
  geom_abline(intercept = 0, slope = 1, color = "darkred") +
  geom_point() +
  facet_wrap(~model, nrow = 1) +
  coord_fixed() +
  ggtitle("Linear predictor: SD",
          "Each model (y-axis) compared to `tmbstan` HMC (x-axis)")


    

#'
#' 
#' # 2. Model for area / age / sex prevalence
#' 
#' Construct model frame
mf_model <- tidyr::crossing(
                     mf_areas,
                     sex = c("male", "female"),
                     age_group_id = 1:17
                   ) %>%
  dplyr::mutate(idx = dplyr::row_number()) %>%
  dplyr::mutate(area_idf = forcats::as_factor(area_id),
                age_group_idf = forcats::as_factor(age_group_id))

#' Add population size

mf_model <- mf_model %>%
  left_join(pop2016 %>% select(area_id, sex, age_group_id, population))

#' Add PHIA survey HIV prevalence
mf_model <- mf_model %>%
  left_join(
    survey_hiv_indicators %>%
    filter(indicator == "prev", survey_id == "MWI2016PHIA")
  ) %>%
  mutate(x = est * n_obs)

#' Output frame
out_mf <- naomi_output_frame(mf_model, areas, drop_partial_areas = FALSE)

#' ### INLA

##+ fit_inla2, cache = TRUE
mf_model <- mf_model %>%
  mutate(area_idx.sex = if_else(sex == "male", area_idx, NA_integer_),
         age_group_id.sex = if_else(sex == "male", age_group_id, NA_real_))

f_inla <- inla(x ~ sex +
                 f(area_idx, model = "bym2", graph = M) +
                 f(area_idx.sex, model = "bym2", graph = M) +
                 f(age_group_id, model = "ar1") + 
                 f(age_group_id.sex, model = "ar1"),
               family = "binomial", data = mf_model, Ntrials = n_obs,
               control.predictor=list(link=1),
               control.compute=list(config = TRUE))

summary(f_inla)

#' Sample from posterior distribution
##+ sample_inla2, cache = TRUE
smp_inla <- inla.posterior.sample(1000, f_inla, intern = TRUE)

#' Simulate logit prevalence and output prevalence
mu_rho_inla <- lapply(smp_inla, "[[", "latent") %>%
  vapply("[", numeric(nrow(mf_model)), seq_len(nrow(mf_model)))

plhiv_inla <- plogis(mu_rho_inla) * mf_model$population
rho_out_inla <- (out_mf$A %*% plhiv_inla) / (out_mf$A %*% mf_model$population)


out <- out_mf$mf %>%
  mutate(mean = rowMeans(rho_out_inla),
         sd = apply(rho_out_inla, 1, sd),
         median = apply(rho_out_inla, 1, median),
         lower = apply(rho_out_inla, 1, quantile, 0.025),
         upper = apply(rho_out_inla, 1, quantile, 0.975))
         
##+ fig.height = 4, fig.width = 7
out %>%
  filter(area_id == "MWI", age_group_id %in% 1:17, sex %in% c("male", "female")) %>%
  ggplot(aes(age_group_id, mean, ymin = lower, ymax = upper, fill = sex)) +
  geom_col(position = "dodge") +
  geom_linerange(position = position_dodge(0.8)) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Age-specific HIV prevalence",
          "Data coverage ages 0-64 (age_group_id 1:13)")

#' _Comparisons with TMB model to be completed...but based on full Naomi model,
#' conclusions will be similar simpler area-model results._
#' 
#' # Conclusions
#'
#' * Sampling from the joint precision of TMB and gives poor uncertainty ranges
#'   (too wide)l.
#'   * Makes sense, probably non-linear correlation of hyper-parameters and
#'     random effects.
#'   * Speculate that this might be less-bad if using INLA parameterizations
#'     for
#' * For now: for uncertainty from TMB, sample random effects only conditional
#'   on point estimates for hyper-parameters.
#' * Further work:
#'   * Implemnent INLA parameterizations in TMB and compare.
#'   * Explore two-stage sampling: sample fixed effects (parameters), then sample
#'     latent field conditionally. I think this is similar to `inla.posterior.sample()` approach.
#' 
