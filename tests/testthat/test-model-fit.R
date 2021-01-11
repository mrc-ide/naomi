context("test-model-fit")

test_that("setting rng_seed returns same outputs", {

  a_fit_sample2 <- sample_tmb(a_fit, nsample = 30, rng_seed = 28)
  a_output2 <- output_package(a_fit_sample2, a_naomi_mf)

  expect_equal(a_fit_sample$sample, a_fit_sample2$sample)
  expect_equal(a_output$indicators$mean, a_output2$indicators$mean)
})

test_that("setting different rng_seed returns different output", {

  a_fit_sample3 <- sample_tmb(a_fit, nsample = 30, rng_seed = 1)
  expect_true(
    a_fit_sample$sample$artnum_t1_out[1] !=
    a_fit_sample3$sample$artnum_t1_out[1]
  )

  a_fit_sample_null <- sample_tmb(a_fit, nsample = 30)
  expect_true(
    a_fit_sample$sample$artnum_t1_out[1] !=
    a_fit_sample_null$sample$artnum_t1_out[1]
  )
})

test_that("exceeding maximum iterations throws a warning", {
  expect_warning(fit_tmb(a_tmb_inputs, outer_verbose = FALSE, max_iter = 5),
                 "iteration limit reached")
})

test_that("model fits with differing number of ANC observations T1 and T2", {

  ancdat <- demo_anc_testing %>%
    dplyr::filter(area_id %in% a_naomi_mf$mf_areas$area_id) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(year == 2016 |
                  year == 2018 & dplyr::row_number() == 3) %>%
    dplyr::ungroup()

  naomi_data <- select_naomi_data(a_naomi_mf,
                                  demo_survey_hiv_indicators,
                                  anc_testing = ancdat,
                                  demo_art_number,
                                  prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                  artcov_survey_ids = "DEMO2016PHIA",
                                  recent_survey_ids = "DEMO2016PHIA",
                                  anc_prev_year_t1 = 2016,
                                  anc_prev_year_t2 = 2018,
                                  anc_artcov_year_t1 = 2016,
                                  anc_artcov_year_t2 = 2018)

  tmb_inputs <- prepare_tmb_inputs(naomi_data)
  fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

  expect_equal(fit$convergence, 0)

})

test_that("model fit with no ART data at T2", {

    naomi_data <- select_naomi_data(a_naomi_mf,
                                    demo_survey_hiv_indicators,
                                    anc_testing = demo_anc_testing,
                                    demo_art_number,
                                    prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                    artcov_survey_ids = "DEMO2016PHIA",
                                    recent_survey_ids = "DEMO2016PHIA",
                                    anc_prev_year_t1 = 2016,
                                    anc_prev_year_t2 = 2018,
                                    anc_artcov_year_t1 = 2016,
                                    anc_artcov_year_t2 = 2018,
                                    artnum_calendar_quarter_t2 = NULL)

    tmb_inputs <- prepare_tmb_inputs(naomi_data)
    fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

    expect_equal(nrow(naomi_data$artnum_t2_dat), 0)
    expect_equal(tmb_inputs$par_init$beta_alpha_t2, numeric(0))
    expect_equal(tmb_inputs$par_init$u_alpha_xt, numeric(0))
    expect_equal(ncol(tmb_inputs$data$Z_alpha_xt), 0)
    expect_equal(ncol(tmb_inputs$data$X_alpha_t2), 0)
    expect_equal(tmb_inputs$data$x_artnum_t2, integer(0))

    expect_true(!"beta_alpha_t2" %in% names(fit$par.full))
    expect_true(!"u_alpha_xt" %in% names(fit$par.full))
})

test_that("extract_indicators returns expected names and types", {

  ind_colnames <- c("area_id", "sex", "age_group", "calendar_quarter", "indicator",
                    "mean", "se", "median", "mode", "lower", "upper")

  ind1 <- extract_indicators(a_fit, a_naomi_mf)
  expect_setequal(names(ind1), ind_colnames)
  expect_true(!any(is.na(ind1$mode)))
  expect_true(all(is.na(ind1[c("mean", "se", "median", "lower", "upper")])))
  expect_true(all(vapply(ind1[c("mean", "se", "median", "mode", "lower", "upper")],
                         typeof, character(1)) == "double"))

  ind_sample <- extract_indicators(a_fit_sample, a_naomi_mf)
  expect_setequal(names(ind_sample), ind_colnames)
  expect_true(!any(is.na(ind_sample)))
  expect_true(all(vapply(ind_sample[c("mean", "se", "median", "mode", "lower", "upper")],
                         typeof, character(1)) == "double"))

})


test_that("add_stats returns expected names and types", {

  prefix_colnames <- c("id", "x_mode", "x_mean", "x_se",
                       "x_median", "x_lower", "x_upper")

  m <- 1:3 / 10
  s <- matrix(1:12 / 10, length(m))

  ## sample = NULL
  v1 <- add_stats(data.frame(id = 1:3), m, prefix = "x_")
  expect_setequal(names(v1), prefix_colnames)
  expect_true(all(vapply(v1[prefix_colnames[-1]], typeof, character(1)) == "double"))
  expect_true(!any(is.na(v1$mode)))
  expect_true(all(is.na(v1[c("x_mean", "x_se", "x_median", "x_lower", "x_upper")])))

  ## with sample
  v2 <- add_stats(data.frame(id = 1:3), m, s, prefix = "x_")
  expect_setequal(names(v2), prefix_colnames)
  expect_true(all(vapply(v2[prefix_colnames[-1]], typeof, character(1)) == "double"))
  expect_true(!any(is.na(v2)))

})

test_that("output_package() works with mode, sample, or both", {

  output_mode <- output_package(a_fit, a_naomi_mf)

  fit_sample_only <- a_fit_sample
  fit_sample_only$mode <- NULL
  output_sample <- output_package(fit_sample_only, a_naomi_mf)

  expect_true(all(!is.na(a_output$indicators[c("mean", "se", "median", "mode", "lower", "upper")])))

  expect_true(all(is.na(output_mode$indicators[c("mean", "se", "median", "lower", "upper")])))
  expect_equal(output_mode$indicators$mode, a_output$indicators$mode)

  expect_true(all(is.na(output_mode$mode)))
  expect_equal(output_sample$indicators[c("mean", "se", "median", "lower", "upper")],
               a_output$indicators[c("mean", "se", "median", "lower", "upper")])
})


test_that("model fit with aggregate survey data", {

  ## Test cases:
  ## - Aggregate areas, age/sex stratified data
  ## - Aggregate areas and age, sex stratified dataii
  ## - Aggregate area/age/sex

  # 1. Aggregate areas, age/sex stratified data
  aggregate_survey_5yr <- dplyr::filter(demo_survey_hiv_indicators,
                                        age_group %in% get_five_year_age_groups(),
                                        sex %in% c("male", "female"),
                                        area_id == "MWI_1_1_demo",
                                        indicator %in% c("prevalence", "art_coverage"))

  naomi_data <- select_naomi_data(a_naomi_mf,
                                  aggregate_survey_5yr,
                                  demo_anc_testing,
                                  demo_art_number,
                                  prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                  artcov_survey_ids = "DEMO2016PHIA",
                                  recent_survey_ids = "DEMO2016PHIA",
                                  anc_prev_year_t1 = 2016,
                                  anc_prev_year_t2 = 2018,
                                  anc_artcov_year_t1 = 2016,
                                  anc_artcov_year_t2 = 2018,
                                  artnum_calendar_quarter_t1 = NULL,
                                  artnum_calendar_quarter_t2 = NULL,
                                  use_survey_aggregate = TRUE)

    tmb_inputs <- prepare_tmb_inputs(naomi_data)
    fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

    expect_true(ncol(tmb_inputs$data$Z_rho_xs) > 0)
    expect_true(ncol(tmb_inputs$data$Z_rho_a) > 0)
    expect_true(ncol(tmb_inputs$data$Z_rho_as) > 0)
    expect_true(length(tmb_inputs$par_init$u_rho_xs) > 0)
    expect_true(length(tmb_inputs$par_init$us_rho_xs) > 0)
    expect_true(length(tmb_inputs$par_init$u_rho_a) > 0)
    expect_true(length(tmb_inputs$par_init$u_rho_as) > 0)

    expect_true(ncol(tmb_inputs$data$Z_alpha_xs) > 0)
    expect_true(ncol(tmb_inputs$data$Z_alpha_a) > 0)
    expect_true(ncol(tmb_inputs$data$Z_alpha_as) > 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_xs) > 0)
    expect_true(length(tmb_inputs$par_init$us_alpha_xs) > 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_a) > 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_as) > 0)

    expect_equal(fit$convergence, 0)

    ## 2. Aggregate areas and age, sex stratified dataii
    aggregate_survey_sex <- dplyr::filter(demo_survey_hiv_indicators,
                                          age_group == "Y015_049",
                                          sex %in% c("male", "female"),
                                          area_id == "MWI_1_1_demo",
                                          indicator %in% c("prevalence", "art_coverage"))

    naomi_data <- select_naomi_data(a_naomi_mf,
                                    aggregate_survey_sex,
                                    demo_anc_testing,
                                    demo_art_number,
                                    prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                    artcov_survey_ids = "DEMO2016PHIA",
                                    recent_survey_ids = "DEMO2016PHIA",
                                    anc_prev_year_t1 = 2016,
                                    anc_prev_year_t2 = 2018,
                                    anc_artcov_year_t1 = 2016,
                                    anc_artcov_year_t2 = 2018,
                                    artnum_calendar_quarter_t1 = NULL,
                                    artnum_calendar_quarter_t2 = NULL,
                                    use_survey_aggregate = TRUE)
    
    tmb_inputs <- prepare_tmb_inputs(naomi_data)
    fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

    expect_true(ncol(tmb_inputs$data$Z_rho_xs) > 0)
    expect_true(ncol(tmb_inputs$data$Z_rho_a) == 0)
    expect_true(ncol(tmb_inputs$data$Z_rho_as) == 0)
    expect_true(length(tmb_inputs$par_init$u_rho_xs) > 0)
    expect_true(length(tmb_inputs$par_init$us_rho_xs) > 0)
    expect_true(length(tmb_inputs$par_init$u_rho_a) == 0)
    expect_true(length(tmb_inputs$par_init$u_rho_as) == 0)

    expect_true(ncol(tmb_inputs$data$Z_alpha_xs) > 0)
    expect_true(ncol(tmb_inputs$data$Z_alpha_a) == 0)
    expect_true(ncol(tmb_inputs$data$Z_alpha_as) == 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_xs) > 0)
    expect_true(length(tmb_inputs$par_init$us_alpha_xs) > 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_a) == 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_as) == 0)

    expect_equal(fit$convergence, 0)

    ## 3. Aggregate areas and age, sex stratified dataii
    aggregate_survey_all <- dplyr::filter(demo_survey_hiv_indicators,
                                          age_group == "Y015_049",
                                          sex == "both",
                                          area_id == "MWI_1_1_demo",
                                          indicator %in% c("prevalence", "art_coverage"))

    naomi_data <- select_naomi_data(a_naomi_mf,
                                    aggregate_survey_all,
                                    demo_anc_testing,
                                    demo_art_number,
                                    prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                    artcov_survey_ids = "DEMO2016PHIA",
                                    recent_survey_ids = "DEMO2016PHIA",
                                    anc_prev_year_t1 = 2016,
                                    anc_prev_year_t2 = 2018,
                                    anc_artcov_year_t1 = 2016,
                                    anc_artcov_year_t2 = 2018,
                                    artnum_calendar_quarter_t1 = NULL,
                                    artnum_calendar_quarter_t2 = NULL,
                                    use_survey_aggregate = TRUE)
    
    tmb_inputs <- prepare_tmb_inputs(naomi_data)
    fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

    expect_true(ncol(tmb_inputs$data$Z_rho_xs) == 0)
    expect_true(ncol(tmb_inputs$data$Z_rho_a) == 0)
    expect_true(ncol(tmb_inputs$data$Z_rho_as) == 0)
    expect_true(length(tmb_inputs$par_init$u_rho_xs) == 0)
    expect_true(length(tmb_inputs$par_init$us_rho_xs) == 0)
    expect_true(length(tmb_inputs$par_init$u_rho_a) == 0)
    expect_true(length(tmb_inputs$par_init$u_rho_as) == 0)

    expect_true(ncol(tmb_inputs$data$Z_alpha_xs) == 0)
    expect_true(ncol(tmb_inputs$data$Z_alpha_a) == 0)
    expect_true(ncol(tmb_inputs$data$Z_alpha_as) == 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_xs) == 0)
    expect_true(length(tmb_inputs$par_init$us_alpha_xs) == 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_a) == 0)
    expect_true(length(tmb_inputs$par_init$u_alpha_as) == 0)

    expect_equal(fit$convergence, 0)
})
