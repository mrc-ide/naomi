#' Model Frame and Linear Transform for Aggregated Model Outputs
#'
#' @param mf_model Model frame
#' @param areas naomi_areas object.
#' @param drop_partial_areas Drop areas from output if some children are
#'   missing (default TRUE).
#'
#' @export
naomi_output_frame <- function(mf_model, areas, drop_partial_areas = TRUE) {

  stopifnot(methods::is(areas, "naomi_areas"))


  
  age_group_ids <- unique(mf_model$age_group_id)


  model_area_ids <- unique(mf_model$area_id)
  
  area_id_out <- areas$tree$Get("area_id",
                                filterFun = function(x) x$display_level,
                                traversal = "level")
  area_id_out_leaves <- areas$tree$Get("leaves",
                                       filterFun = function(x) x$display_level,
                                       traversal = "level") %>%
    lapply(data.tree::Get, "area_id")

  area_id_out_leaves <- area_id_out_leaves[!duplicated(area_id_out)]
  area_id_out <- area_id_out[!duplicated(area_id_out)]

  leaf_in_model <- lapply(area_id_out_leaves, `%in%`, model_area_ids)
  if(drop_partial_areas) {
    all_leaves_in_model <- vapply(leaf_in_model, all, logical(1))
    area_id_out <- area_id_out[all_leaves_in_model]
    area_id_out_leaves <- area_id_out_leaves[all_leaves_in_model]
  } else {
    area_id_out_leaves <- Map("[", area_id_out_leaves, leaf_in_model)
    area_id_out <- area_id_out[lengths(area_id_out_leaves) > 0]
    area_id_out_leaves <- area_id_out_leaves[lengths(area_id_out_leaves) > 0]
  }

  area_id_join <- Map(data.frame,
                      area_id_out = area_id_out,
                      area_id = area_id_out_leaves,
                      stringsAsFactors = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  sexes <- unique(mf_model$sex)
  sex_out <- get_sex_out(sexes)

  sex_join <- data.frame(sex_out = c("male", "female", "both", "both", "both"),
                         sex = c("male", "female", "male", "female", "both"),
                         stringsAsFactors = FALSE) %>%
    dplyr::filter(sex %in% sexes, sex_out %in% !!sex_out)
  
  age_group_id_out <- get_age_group_id_out(age_group_ids)
  
  age_group_join <- get_age_groups() %>%
    dplyr::filter(age_group_id %in% age_group_id_out) %>%
    stats::setNames(paste0(names(.), "_out")) %>%
    tidyr::crossing(get_age_groups() %>%
                    dplyr::filter(age_group_id %in% age_group_ids)) %>%
    dplyr::filter(age_group_start_out <= age_group_start,
                  age_group_span_out == Inf |
                  (age_group_start + age_group_span) <=
                  (age_group_start_out + age_group_span_out)) %>%
    dplyr::select(age_group_id_out, age_group_id)

  stopifnot(age_group_ids %in% age_group_join$age_group_id)

  mf_out <- tidyr::crossing(
                     area_id = area_id_out,
                     sex = sex_out,
                     age_group_id = age_group_id_out
                   )

  df_join <- tidyr::crossing(area_id_join, sex_join, age_group_join) %>%
    dplyr::full_join(
             mf_model %>%
             dplyr::select("area_id", "sex", "age_group_id", "idx"),
             by = c("area_id", "sex", "age_group_id")) %>%
    dplyr::full_join(
             mf_out %>%
             dplyr::mutate(out_idx = dplyr::row_number())
            ,
             by = c("area_id_out" = "area_id",
                    "sex_out" = "sex",
                    "age_group_id_out" = "age_group_id")
           ) %>%
    dplyr::mutate(x = 1)

  A <- Matrix::spMatrix(nrow(mf_out),
                        nrow(mf_model),
                        df_join$out_idx,
                        df_join$idx,
                        df_join$x)

  list(mf = mf_out, A = A)
}


#' Construct Model Frames and Adjacency Structures
#'
#' @param area_merged Merged version of area hierarchy
#' @param population_agesex Population by age group and sex
#' @param spec Spec
#' @param scope The collection of area IDs to be modelled. Defaults to all area
#' ids.
#' @param level Admin level
#' @param calendar_quarter1 Calendar quarter at time 1 ("CYyyyyQq")
#' @param calendar_quarter2 Calendar quarter at time 2 ("CYyyyyQq")
#' @param age_group_ids Age group ids
#' @param sexes Sexes
#' @param omega Omega
#' @param rita_param rita_param
#' @param sigma_u_sd sigma_u_sd
#' @param artattend artattend
#' @param artattend_prior_sigma_scale artattend_propr_sigma_scale
#' @param logit_nu_mean mean of logit viral load suppression.
#' @param logit_nu_sd standard deviation of logit viral load suppression.
#' @return Naomi model frame
#'
#' @export
naomi_model_frame <- function(area_merged,
                              population_agesex,
                              spec,
                              scope = areas$tree$area_id,
                              level = max(areas$tree$Get("area_level")),
                              calendar_quarter1,
                              calendar_quarter2,
                              age_group_ids = 1:17,
                              sexes = c("male", "female"),
                              omega = 0.7,
                              rita_param = list(OmegaT0      = 130 / 365,
                                                sigma_OmegaT = ((142-118) / 365) / (2*qnorm(0.975)),
                                                betaT0       = 0.0,
                                                sigma_betaT  = 0.00001,
                                                ritaT        = 1.0),
                              sigma_u_sd   = 1.0,
                              artattend = FALSE,
                              artattend_prior_sigma_scale = 3.0,
                              logit_nu_mean = 2.0,
                              logit_nu_sd = 0.3) {

  ## Create area tree
  ## TODO: Get rid of reliance on data.tree
  areas <- create_areas(area_merged = area_merged)
  
  ## Prune areas below model level
  data.tree::Prune(areas$tree, function(x) x$area_level <= level)

  ## Get leaves that are children of scope
  area_id_leaves <- areas$tree$Get("leaves", traversal = "level")

  if(length(setdiff(scope, names(area_id_leaves))))
    stop(paste("Scope areas", setdiff(scope, names(area_id_leaves)), "not found in hierarchy."))

  area_id <- area_id_leaves[scope] %>%
    lapply(data.tree::Get, "area_id") %>%
    unlist() %>%
    unique()

  spectrum_region_code <- area_id_leaves[scope] %>%
    lapply(data.tree::Get, "spectrum_region_code") %>%
    unlist()

  ## Keep

  mf_areas <- data.frame(area_id,
                         area_idx = seq_along(area_id),
                         spectrum_region_code = spectrum_region_code,
                         stringsAsFactors = FALSE) %>%
    dplyr::mutate(area_idf = factor(area_id, area_id))

  ## Model frame
  mf_model <- tidyr::crossing(
                       mf_areas,
                       sex = sexes,
                       age_group_id = age_group_ids
                     ) %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::mutate(area_idf = forcats::as_factor(area_id),
                  age_group_idf = forcats::as_factor(age_group_id))

  ## Add population estimates
  
  mf_model <- mf_model %>%
    dplyr::left_join(
             population_agesex %>%
             dplyr::filter(area_id %in% mf_areas$area_id) %>%
             interpolate_population_agesex(calendar_quarter1) %>%
             dplyr::left_join(
                      get_age_groups() %>% dplyr::select(age_group, age_group_id),
                      by = "age_group"
                    ) %>%
             dplyr::select(area_id, sex, age_group_id, population_t1 = population),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::left_join(
             population_agesex %>%
             dplyr::filter(area_id %in% mf_areas$area_id) %>%
             interpolate_population_agesex(calendar_quarter2) %>%
             dplyr::left_join(
                      get_age_groups() %>% dplyr::select(age_group, age_group_id),
                      by = "age_group"
                    ) %>%
             dplyr::select(area_id, sex, age_group_id, population_t2 = population),
             by = c("area_id", "sex", "age_group_id")
           )

  stopifnot(!is.na(mf_model$population_t1))
  stopifnot(!is.na(mf_model$population_t2))

  zeropop1 <- mf_model$population_t1 == 0
  zeropop2 <- mf_model$population_t2 == 0

  if(any(zeropop1) | any(zeropop2)) {
    warning(paste("Zero population input for", sum(zeropop1) + sum(zeropop2),
                  "area/age/sex groups.",
                  "Replaced with population 0.1."))
    mf_model$population_t1[zeropop1] <- 0.1
    mf_model$population_t2[zeropop2] <- 0.1
  }


  ## Add Spectrum inputs

  mf_model <- mf_model %>%
    dplyr::left_join(
             calc_spec_age_group_aggregate(spec) %>%
             ## !!! NEEDS UPDATE
             dplyr::filter(year == 2016) %>%
             dplyr::select(
                      spectrum_region_code,
                      sex,
                      age_group_id,
                      spec_prev = prevalence,
                      spec_incid = incidence,
                      spec_artcov = art_coverage,
                      asfr
                    ),
             by = c("spectrum_region_code", "sex", "age_group_id")
           )

  ## Projection matrix

  quarter_id1 <- calendar_quarter_to_quarter_id(calendar_quarter1)
  quarter_id2 <- calendar_quarter_to_quarter_id(calendar_quarter2)
  Lproj <- create_Lproj(spec, mf_model, quarter_id1, quarter_id2)

  ## Adjacency matrix
  M <- mf_areas %>%
    dplyr::mutate(geometry = areas$boundaries[area_id]) %>%
    sf::st_as_sf() %>%
    methods::as("Spatial") %>%
    spdep::poly2nb(.$area_id) %>%
    spdep::nb2mat(style = "B", zero.policy = TRUE)

  colnames(M) <- rownames(M)

  ## Scaled  precision matrix for 'BYM2' model.
  Q  <- INLA::inla.scale.model(diag(rowSums(M)) - M,
                               constr = list(A = matrix(1, 1, nrow(M)), e = 0))



  ## Model output

  outf <- naomi_output_frame(mf_model, areas)


  ## ART attendance model

  artattendM <- if(artattend) M else matrix(0, nrow(M), ncol(M))

  mf_areas <- mf_areas %>%
    dplyr::left_join(
             data.frame(area_idx = seq_len(nrow(artattendM)),
                        n_neighbors = colSums(artattendM)),
             by = "area_idx"
           )

  mf_artattend <- (artattendM + diag(nrow(artattendM))) %>%
    methods::as("dgCMatrix") %>%
    Matrix::summary() %>%
    dplyr::rename(reside_area_idx = i,
                  artattend_area_idx = j) %>%
    dplyr::mutate(x = NULL,
                  istar = as.integer(reside_area_idx == artattend_area_idx),
                  jstar = as.integer(reside_area_idx == artattend_area_idx)) %>%
    dplyr::arrange(reside_area_idx, istar, artattend_area_idx, jstar) %>%
    dplyr::mutate(artattend_idx = dplyr::row_number())

  gamma_or_prior <-   mf_areas %>%
    dplyr::mutate(n_nb_lim = pmin(n_neighbors, 9)) %>%
    dplyr::left_join(
             data.frame(
               n_nb_lim = 1:9,
               gamma_or_mu = c(-3.29855798975623, -4.0643930585428, -4.53271592818956, -4.86910480099925, -5.13133396982624,
                               -5.34605339546364, -5.52745113789738, -5.68479564118418, -5.8234349424758),
               gamma_or_sigma = artattend_prior_sigma_scale *
                 c(0.950818503595947, 1.04135785601697, 1.12665887287997, 1.19273171464978, 1.24570962739274,
                   1.28959773294666, 1.32675564121864, 1.35902556091841, 1.3873644912272)
             ),
             by = "n_nb_lim"
           )

  mf_artattend <- mf_artattend %>%
    dplyr::left_join(
             gamma_or_prior %>%
             dplyr::select(area_idx, gamma_or_mu, gamma_or_sigma),
             by = c("reside_area_idx" = "area_idx")
           ) %>%
    dplyr::mutate(gamma_or_mu = dplyr::if_else(istar == 1, NA_real_, gamma_or_mu),
                  gamma_or_sigma = dplyr::if_else(istar == 1, NA_real_, gamma_or_sigma))

  ## Incidence model

  mf_model <- mf_model %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::filter(age_group_id %in% age_group_ids) %>%
             dplyr::mutate(
               age15to49 = as.integer(age_group_start >= 15 &
                                      (age_group_start + age_group_span) <= 50)
             ) %>%
             dplyr::select(age_group_id, age15to49),
             by = "age_group_id"
           ) %>%
    dplyr::group_by(area_id) %>%
    dplyr::mutate(
             spec_prev15to49 = sum(population_t1 * spec_prev * age15to49) / sum(population_t1 * age15to49),
             spec_artcov15to49 =
               sum(population_t1 * spec_prev * spec_artcov * age15to49) /
               sum(population_t1 * spec_prev * age15to49),
             log_lambda_offset =
               log(spec_incid) - log(spec_prev15to49) - log(1 - omega * spec_artcov15to49),
             logit_rho_offset = 0,
             logit_alpha_offset = 0,

           ) %>%
  dplyr::ungroup()

  v <- list(mf_model = mf_model,
            mf_out = outf$mf,
            mf_areas = mf_areas,
            mf_artattend = mf_artattend,
            A_out = outf$A,
            Lproj = Lproj,
            age_group_ids = age_group_ids,
            sexes = sexes,
            calendar_quarter1 = calendar_quarter1,
            calendar_quarter2 = calendar_quarter2,
            omega = omega,
            rita_param = rita_param,
            logit_nu_mean = logit_nu_mean,
            logit_nu_sd = logit_nu_sd,
            M = M,
            Q = Q)

  class(v) <- "naomi_mf"
  v
}

#' Select data for model fitting
#'
#' @param naomi_mf A Naomi model frame object.
#' @param survey_hiv_indicators Data frame of survey estimates, or NULL to exclude any survey data.
#' @param anc_testing Data frame of ANC routine testing outcomes, or NULL to exclude any ANC data.
#' @param art_number Data frame of number currently receiving ART, or NULL to exclude any ART data.
#' @param prev_survey_ids A character vector of `survey_id`s for prevalence data.
#' @param artcov_survey_ids A character vector of `survey_id`s for ART coverage data.
#' @param recent_survey_ids A character vector of `survey_id`s for recent HIV infection status.
#' @param vls_survey_ids A character vector of `survey_id`s for survey VLS among all HIV+ persons.
#' @param artnum_calendar_quarter_t1 Calendar quarter for first time point for number on ART.
#' @param artnum_calendar_quarter_t2 Calendar quarter for second time point for number on ART.
#' @param anc_prev_year_t1 Calendar year (possibly multiple) for first time point for ANC prevalence.
#' @param anc_prev_year_t2 Calendar year (possibly multiple) for second time point for ANC prevalence.
#' @param anc_artcov_year_t1 Calendar year (possibly multiple) for first time point for ANC ART coverage.
#' @param anc_artcov_year_t2 Calendar year (possibly multiple) for second time point for ANC ART coverage.
#'
#' @details
#' See example datasets for examples of required template for data sets. *`_survey_ids` must be reflected
#' in `survey_hiv_indicators`.
#'
#' ART coverage and VLS survey data should not be included from the same survey. This is checked
#' by the function call and will throw an error.
#'
#' @seealso [mwi_survey_hiv_indicators], [mwi_anc_testing], [mwi_art_number], [convert_quarter_id]
#'
#' @export
select_naomi_data <- function(naomi_mf,
                              survey_hiv_indicators,
                              anc_testing,
                              art_number,
                              prev_survey_ids,
                              artcov_survey_ids,
                              recent_survey_ids,
                              vls_survey_ids = NULL,
                              artnum_calendar_quarter_t1 = naomi_mf$calendar_quarter1,
                              artnum_calendar_quarter_t2 = naomi_mf$calendar_quarter2,
                              anc_prev_year_t1 = year_labels(calendar_quarter_to_quarter_id(naomi_mf$calendar_quarter1)),
                              anc_prev_year_t2 = year_labels(calendar_quarter_to_quarter_id(naomi_mf$calendar_quarter2)),
                              anc_artcov_year_t1 = anc_prev_year_t1,
                              anc_artcov_year_t2 = anc_prev_year_t2) {

  stopifnot(is(naomi_mf, "naomi_mf"))

  if(length(intersect(artcov_survey_ids, vls_survey_ids)))
    stop(paste("Do not use ART coverage and VLS data from the same survey:",
               intersect(artcov_survey_ids, vls_survey_ids)))

  naomi_mf$prev_dat <- survey_prevalence_mf(prev_survey_ids, survey_hiv_indicators, naomi_mf)
  naomi_mf$artcov_dat <- survey_artcov_mf(artcov_survey_ids, survey_hiv_indicators, naomi_mf)
  naomi_mf$recent_dat <- survey_recent_mf(recent_survey_ids, survey_hiv_indicators, naomi_mf)
  naomi_mf$vls_dat <- survey_vls_mf(vls_survey_ids, survey_hiv_indicators, naomi_mf)

  naomi_mf$anc_prev_t1_dat <- anc_testing_prev_mf(anc_prev_year_t1, anc_testing, naomi_mf)
  naomi_mf$anc_artcov_t1_dat <- anc_testing_artcov_mf(anc_artcov_year_t1, anc_testing, naomi_mf)

  naomi_mf$anc_prev_t2_dat <- anc_testing_prev_mf(anc_prev_year_t2, anc_testing, naomi_mf)
  naomi_mf$anc_artcov_t2_dat <- anc_testing_artcov_mf(anc_artcov_year_t2, anc_testing, naomi_mf)

  naomi_mf$artnum_t1_dat <- artnum_mf(artnum_calendar_quarter_t1, art_number, naomi_mf)
  naomi_mf$artnum_t2_dat <- artnum_mf(artnum_calendar_quarter_t2, art_number, naomi_mf)

  naomi_mf <- update_mf_offsets(naomi_mf,
                                naomi_mf$prev_dat,
                                naomi_mf$artcov_dat,
                                naomi_mf$vls_dat)

  class(naomi_mf) <- c("naomi_data", class(naomi_mf))

  naomi_mf
}

update_mf_offsets <- function(naomi_mf,
                              prev_dat = NULL,
                              artcov_dat = NULL,
                              vls_dat = NULL) {

  stopifnot(is(naomi_mf, "naomi_mf"))

  ## TODO: This should handle different age_max by sex...
  get_idx <- function(mf, df) {

    age_max <- df %>%
      dplyr::left_join(get_age_groups(), by = "age_group_id") %>%
      dplyr::summarise(age_max = max(age_group_start + age_group_span)) %>%
      .$age_max

    mf %>%
      dplyr::left_join(get_age_groups(), by = "age_group_id") %>%
      dplyr::transmute(idx,
                       data_range = as.integer((age_group_start + age_group_span) <= age_max),
                       offset_range = as.integer((age_group_start + age_group_span) >= age_max),
                       age_fct = factor(pmin(age_group_start,
                                             max(age_group_start * data_range))))
  }


  if(is.null(prev_dat) || nrow(prev_dat) == 0) {
    ## Offset vs. age 15-49 prevalence
    ## No rho_a_fct
    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::mutate(
               rho_a_fct = NA,
               logit_rho_offset = qlogis(spec_prev) - qlogis(spec_prev15to49),
             )
  } else {
    d <- get_idx(naomi_mf$mf_model, prev_dat)

    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::left_join(d, by = "idx") %>%
      dplyr::group_by(area_id, sex) %>%
      dplyr::mutate(
               rho_a_fct = age_fct,
               age_fct = NULL,
               logit_rho_offset = dplyr::if_else(offset_range == 1, qlogis(spec_prev) - qlogis(max(spec_prev * data_range * offset_range)), 0),
               data_range = NULL,
               offset_range = NULL
             ) %>%
      dplyr::ungroup()
  }


  if((is.null(artcov_dat) || nrow(artcov_dat) == 0) && (is.null(vls_dat) || nrow(vls_dat) == 0)) {
    ## Offset vs. age 15-49 ART coverage
    ## No alpha_a_fct
    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::mutate(
               alpha_a_fct = NA,
               logit_alpha_offset = qlogis(spec_artcov) - qlogis(spec_artcov15to49),
             )

  } else {

    artcov_vls_dat <- dplyr::bind_rows(artcov_dat, vls_dat)
    d <- get_idx(naomi_mf$mf_model, artcov_vls_dat)

    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::left_join(d, by = "idx") %>%
      dplyr::group_by(area_id, sex) %>%
      dplyr::mutate(
               alpha_a_fct = age_fct,
               age_fct = NULL,
               logit_alpha_offset = dplyr::if_else(offset_range == 1, qlogis(spec_artcov) - qlogis(max(spec_artcov * data_range * offset_range)), 0),
               data_range = NULL,
               offset_range = NULL
             ) %>%
      dplyr::ungroup()
  }

  naomi_mf
}

#' Get age group ids for output
#'
#' Ensures that age groups are fully spanned by modelled
#' ages.
#'
#' @param age_group_ids Modelled age group ids. Assumed to
#'   be non-overlapping.
#'
#' @keywords internal
get_age_group_id_out <- function(age_group_ids) {

  agegr <- get_age_groups() %>%
    dplyr::filter(age_group_id %in% age_group_ids) %>%
    dplyr::transmute(mod_agegr_start = age_group_start,
                     mod_agegr_span = age_group_span)


  ## TODO: Check agegr are non overlapping

  v <- get_age_groups() %>%
    tidyr::crossing(agegr) %>%
    dplyr::filter(age_group_start <= mod_agegr_start,
                  age_group_start + age_group_span >= mod_agegr_start + mod_agegr_span) %>%
    dplyr::group_by(age_group_id) %>%
    dplyr::filter(age_group_start == min(mod_agegr_start)) %>%
    dplyr::ungroup() %>%
    dplyr::count(age_group_id, age_group_span, wt = mod_agegr_span) %>%
    dplyr::filter(age_group_span == n)

  v$age_group_id
}


get_sex_out <- function(sexes) {

  if(!all(c("male", "female") %in% sexes))
    sex_out <- sexes
  else
    sex_out <- c("both", "male", "female")

  sex_out
}


#' Prepare model frames for survey datasets
#'
#' @param survey_ids Survey IDs
#' @param survey_hiv_indicators Survey HIV indicators
#' @param naomi_mf Naomi model frame
#' @param min_age Min age for calculating recent infection
#' @param max_age Max age for calculating recent infection
#'
#' @export
survey_prevalence_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf) {

  prev_dat <- naomi_mf$mf_model %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == "prev") %>%
             dplyr::left_join(
                      get_age_groups() %>%
                      dplyr::select(age_group, age_group_id),
                      by = "age_group"
                    ),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)

  prev_dat
}

#' @rdname survey_prevalence_mf
#' @export
survey_artcov_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf) {

  artcov_dat <- naomi_mf$mf_model %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == "artcov") %>%
             dplyr::left_join(
                      get_age_groups() %>%
                      dplyr::select(age_group, age_group_id),
                      by = "age_group"
                    ),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)

  artcov_dat
}

#' @rdname survey_prevalence_mf
#' @export
survey_vls_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf) {

  vls_dat <- naomi_mf$mf_model %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == "vls") %>%
             dplyr::left_join(
                      get_age_groups() %>%
                      dplyr::select(age_group, age_group_id),
                      by = "age_group"
                    ),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)

  vls_dat
}

#' @rdname survey_prevalence_mf
#' @export
survey_recent_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf,
                             min_age = 15, max_age = 80) {

  recent_dat <- naomi_mf$mf_model %>%
    dplyr::left_join(get_age_groups(), by = "age_group_id") %>%
    dplyr::filter(age_group_start >= min_age,
                  age_group_start + age_group_span <= max_age) %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == "recent") %>%
             dplyr::left_join(
                      get_age_groups() %>%
                      dplyr::select(age_group, age_group_id),
                      by = "age_group"
                    ),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)

  recent_dat
}


#' Prepare Model Frames for Programme Datasets
#'
#' @param year Calendar year
#' @param anc_testing ART data frame
#' @param art_number Number on ART
#' @param naomi_mf Naomi model frame
#' @return Calculated prevalence
#'
#' @export
anc_testing_prev_mf <- function(year, anc_testing, naomi_mf) {

  if(is.null(anc_testing)) {
    ## No ANC prevalence data used
    anc_prev_dat <- data.frame(
      area_id = character(0),
      anc_idx = integer(0),
      anc_prev_x = integer(0),
      anc_prev_n = integer(0)
    )
  } else {
    anc_prev_dat <-
      anc_testing %>%
      dplyr::filter(
               year == !!year,
               area_id %in% naomi_mf$mf_model$area_id
             ) %>%
      dplyr::group_by(area_id) %>%
      dplyr::summarise_at(dplyr::vars(ancrt_known_pos, ancrt_test_pos, ancrt_tested), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
               area_id,
               anc_idx = dplyr::row_number(),
               anc_prev_x = ancrt_known_pos + ancrt_test_pos,
               anc_prev_n = ancrt_known_pos + ancrt_tested
             )
  }

  anc_prev_dat
}


#' @rdname anc_testing_prev_mf
#' @export
anc_testing_artcov_mf <- function(year, anc_testing, naomi_mf) {

  if(is.null(anc_testing)) {
    ## No ANC ART coverage data used
    anc_artcov_dat <- data.frame(
      area_id = character(0),
      anc_idx = integer(0),
      anc_artcov_x = integer(0),
      anc_artcov_n = integer(0)
    )
  } else {
    anc_artcov_dat <-
      anc_testing %>%
      dplyr::filter(
               year == !!year,
               area_id %in% naomi_mf$mf_model$area_id,
               !is.na(ancrt_known_pos),
               !is.na(ancrt_test_pos),
               !is.na(ancrt_already_art)
             ) %>%
      dplyr::group_by(area_id) %>%
      dplyr::summarise_at(dplyr::vars(ancrt_known_pos, ancrt_test_pos, ancrt_already_art), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ancrt_totpos = ancrt_known_pos + ancrt_test_pos) %>%
      dplyr::transmute(
               area_id,
               anc_idx = dplyr::row_number(),
               anc_artcov_x = ancrt_already_art,
               anc_artcov_n = ancrt_totpos
             )
  }

  anc_artcov_dat
}


#' @rdname anc_testing_prev_mf
#'
#' @param calendar_quarter Calendar quarter
#' @export
artnum_mf <- function(calendar_quarter, art_number, naomi_mf) {

  stopifnot(length(calendar_quarter) <= 1)
  stopifnot(is(naomi_mf, "naomi_mf"))

  if(!is.null(calendar_quarter)) {
    year <- year_labels(calendar_quarter_to_quarter_id(calendar_quarter))
  } else {
    year <- NULL
  }

  if(!is.null(art_number) &&

     length(year) &&
     !year %in% art_number$year)
    stop(paste0("No ART data found for year ", year, ".\n",
                "Set year = NULL if you intend to include no ART data."))

  if(is.null(year) || is.null(art_number)) {
    ## No number on ART data or no year specified

    artnum_dat <- data.frame(
      area_id = character(0),
      sex = character(0),
      age_group_id = integer(0),
      artnum_idx = integer(0),
      current_art = integer(0)
    )
  } else {
    ## !!! Note: should add some subsetting for sex and age group.
    artnum_dat <- art_number %>%
      dplyr::filter(year == !!year,
                    area_id %in% naomi_mf$mf_areas$area_id) %>%
      dplyr::left_join(
               get_age_groups() %>%
               dplyr::select(age_group, age_group_id),
               by = "age_group"
             ) %>%
      dplyr::transmute(
               area_id,
               sex,
               age_group_id,
               artnum_idx = dplyr::row_number(),
               current_art
             )
  }

  artnum_dat
}
