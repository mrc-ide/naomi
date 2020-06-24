#' Prepare inputs for INLA
#'
#' @param naomi_data  Naomi data object
#'
#' @return Inputs ready for INLA
#'
#' @seealso [select_naomi_data]
#' @export
prepare_inla_inputs <- function(naomi_data) {

  stopifnot(is(naomi_data, "naomi_data"))
  stopifnot(is(naomi_data, "naomi_mf"))

  df <- naomi_data$mf_model %>%
    dplyr::select(area_id, area_idx, sex, age_group, idx,
                  female_15plus, population_t1,
                  logit_rho_offset, rho_a_fct,
                  logit_alpha_offset, alpha_a_fct)

  df_anc <- df %>%
    dplyr::filter(sex == "female",
                  age_group == "25-29") %>%
    dplyr::mutate(anc = 1)

  df_prev <- df %>%
    dplyr::mutate(anc = 0) %>%
    dplyr::full_join(
             dplyr::select(naomi_data$prev_dat, area_id, sex, age_group, n = n_eff, x = x_eff),
             by = c("area_id", "sex", "age_group")
           ) %>%
    dplyr::bind_rows(
             df_anc %>%
             dplyr::full_join(
                      dplyr::select(naomi_data$anc_prev_t1_dat,
                                    area_id, n = anc_prev_n, x = anc_prev_x),
                      by = "area_id"
                    )
           )

  data_prev <- df_prev %>%
    dplyr::arrange(anc, idx) %>%
    dplyr::group_by(idx) %>%
    dplyr::mutate(population_t1 = dplyr::if_else(dplyr::row_number() == 1, population_t1, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-!is.na(population_t1), idx) %>%
    dplyr::mutate(area_idx.sex = dplyr::if_else(female_15plus == 1, area_idx, NA_integer_),
                  rho_a_fct.sex = droplevels(dplyr::if_else(female_15plus == 1, rho_a_fct, NA_integer_)),
                  area_idx.anc = dplyr::if_else(anc == 1, area_idx, NA_integer_),
                  area_idx.age_group = area_idx,
                  area_idx.sex.age_group = area_idx.sex)

  df_artcov <- df %>%
    dplyr::mutate(anc = 0) %>%
    dplyr::full_join(
             dplyr::select(naomi_data$artcov_dat, area_id, sex, age_group, n = n_eff, x = x_eff),
             by = c("area_id", "sex", "age_group")
           ) %>%
    dplyr::bind_rows(
             df_anc %>%
             dplyr::full_join(
                      dplyr::select(naomi_data$anc_artcov_t1_dat,
                                    area_id, n = anc_artcov_n, x = anc_artcov_x),
                      by = "area_id"
                    )
           )

  data_artcov <- df_artcov %>%
    dplyr::arrange(anc, idx) %>%
    dplyr::group_by(idx) %>%
    dplyr::mutate(population_t1 = dplyr::if_else(dplyr::row_number() == 1, population_t1, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-!is.na(population_t1), idx) %>%
    dplyr::mutate(area_idx.sex = dplyr::if_else(female_15plus == 1, area_idx, NA_integer_),
                  alpha_a_fct.sex = droplevels(dplyr::if_else(female_15plus == 1, alpha_a_fct, NA_integer_)),
                  area_idx.anc = dplyr::if_else(anc == 1, area_idx, NA_integer_),
                  area_idx.age_group = area_idx,
                  area_idx.sex.age_group = area_idx.sex,
                  x = dplyr::if_else(x < n+1e-5, pmin(x, n), x))

  formula_prev <- x ~ female_15plus + anc +
    f(area_idx, model = "bym2", graph = inla_input$M) +
    f(area_idx.sex, model = "bym2", graph = inla_input$M) +
    f(rho_a_fct, model = "ar1") +
    f(rho_a_fct.sex, model = "ar1") +
    f(area_idx.anc, model = "iid")

  formula_artcov <- x ~ female_15plus + anc +
    f(area_idx, model = "bym2", graph = inla_input$M) +
    f(area_idx.sex, model = "bym2", graph = inla_input$M) +
    f(alpha_a_fct, model = "ar1") +
    f(alpha_a_fct.sex, model = "ar1") +
    f(area_idx.anc, model = "iid")

  inla_input <- list(data_prev = data_prev,
                     formula_prev = formula_prev,
                     data_artcov = data_artcov,
                     formula_artcov = formula_artcov,
                     M = naomi_data$M,
                     A_out = naomi_data$A_out)

  class(inla_input) <- "naomi_inla_input"

  inla_input
}


fit_inla <- function(inla_input,
                     integration_strategy = c("auto", "eb"),
                     verbose = FALSE,
                     inla_verbose = FALSE) {

  if ( !requireNamespace("INLA", quietly = TRUE) ) {
    stop("Please install the package INLA is required for fit_inla().")
  }

  if(verbose) print("INLA fitting prevalence model")
  fit_prev <- INLA::inla(inla_input$formula_prev,
                         data = inla_input$data_prev,
                         family = "binomial", Ntrials = n,
                         offset = logit_rho_offset,
                         control.predictor = list(link = 1),
                         control.compute = list(config = TRUE),
                         control.inla = list(int.strategy = integration_strategy[1]),
                         verbose = inla_verbose)
  if(verbose) print("INLA fitting ART coverage model")
  fit_artcov <- INLA::inla(inla_input$formula_artcov,
                           data = inla_input$data_artcov,
                           family = "binomial", Ntrials = n,
                           offset = logit_alpha_offset,
                           control.predictor = list(link = 1),
                           control.compute = list(config = TRUE),
                           control.inla = list(int.strategy = integration_strategy[1]),
                           verbose = inla_verbose)

  if(verbose) print("Extracting posterior mode from INLA fit")

  pred_idx <- which(!is.na(inla_input$data_prev$population_t1))
  population_t1 <- inla_input$data_prev$population_t1[pred_idx]
  rho_t1 <- fit_prev$summary.fitted.values$mode[pred_idx]
  alpha_t1 <- fit_artcov$summary.fitted.values$mode[pred_idx]

  A_out <- inla_input$A_out

  mode <- list()
  mode$population_t1_out <- as.vector(A_out %*% population_t1)
  mode$plhiv_t1_out <- as.vector(A_out %*% (population_t1 * rho_t1))
  mode$artnum_t1_out <- as.vector(A_out %*% (population_t1 * rho_t1 * alpha_t1))
  mode$rho_t1_out <- mode$plhiv_t1_out / mode$population_t1_out
  mode$alpha_t1_out <- mode$artnum_t1_out / mode$plhiv_t1_out

  ## TODO: Remove unneeded components of INLA object to reduce object size.
  ##       Should do this if ever want to save these objects on their own.
  v <- list(inlafit_prev = fit_prev,
            inlafit_artcov = fit_artcov,
            A_out = A_out,
            pred_idx = pred_idx,
            population_t1 = population_t1,
            mode = mode)
  class(v) <- "naomi_inlafit"

  v
}

sample_inla <- function(inlafit, nsample = 1000, rng_seed = NULL, verbose = FALSE) {

  if ( !requireNamespace("INLA", quietly = TRUE) ) {
    stop("Please install the package INLA is required for sample_inla().")
  }
  
  if(is.null(rng_seed))
    rng_seed <- 0L

  pred_idx <- inlafit$pred_idx

  if(verbose) print("INLA sampling prevalence model")
  sample_prev <- INLA::inla.posterior.sample(nsample, inlafit$inlafit_prev,
                                             selection = list("Predictor" = pred_idx),
                                             seed = rng_seed)
  rho_t1 <- plogis(do.call(cbind, lapply(sample_prev, "[[", "latent")))

  if(verbose) print("INLA sampling ART coverage model")
  sample_artcov <- INLA::inla.posterior.sample(nsample, inlafit$inlafit_artcov,
                                               selection = list("Predictor" = pred_idx),
                                               seed = rng_seed)
  alpha_t1 <- plogis(do.call(cbind, lapply(sample_artcov, "[[", "latent")))


  if(verbose) print("Saving sample")
  A_out <- inlafit$A_out
  population_t1 <- matrix(inlafit$population_t1,
                          nrow = length(inlafit$population_t1),
                          ncol = nsample)

  sample <- list()
  sample$population_t1_out <- as.matrix(A_out %*% population_t1)
  sample$plhiv_t1_out <- as.matrix(A_out %*% (population_t1 * rho_t1))
  sample$artnum_t1_out <- as.matrix(A_out %*% (population_t1 * rho_t1 * alpha_t1))
  sample$rho_t1_out <- sample$plhiv_t1_out / sample$population_t1_out
  sample$alpha_t1_out <- sample$artnum_t1_out / sample$plhiv_t1_out

  inlafit$sample <- sample

  inlafit
}
