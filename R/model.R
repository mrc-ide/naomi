#' Model Frame and Linear Transform for Aggregated Model Outputs
#'
#' @param drop_partial_areas Drop areas from output if some children are
#'   missing (default TRUE).
#' 
#' @export 
naomi_output_frame <- function(mf_model, areas, drop_partial_areas = TRUE) {

  stopifnot(methods::is(areas, "naomi_areas"))
  
  model_area_ids <- unique(mf_model$area_id)
  sexes <- unique(mf_model$sex)
  age_group_ids <- unique(mf_model$age_group_id)

  
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
  
  sex_out <- get_sex_out(sexes)
  age_group_id_out <- get_age_group_id_out(age_group_ids)

  mf_out <- tidyr::crossing(
                     area_id = area_id_out,
                     sex = sex_out,
                     age_group_id = age_group_id_out
                   )

  area_id_join <- Map(data.frame,
                      area_id_out = area_id_out,
                      area_id = area_id_out_leaves,
                      stringsAsFactors = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  sex_join <- data.frame (sex_out = c("male", "female", "both", "both", "both"),
                          sex = c("male", "female", "male", "female", "both"),
                          stringsAsFactors = FALSE) %>%
    dplyr::filter(sex %in% sexes, sex_out %in% !!sex_out)

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
    mutate(x = 1)

  A <- Matrix::spMatrix(nrow(mf_out),
                        nrow(mf_model),
                        df_join$out_idx,
                        df_join$idx,
                        df_join$x)

  list(mf = mf_out, A = A)
}


#' Construct Model Frames and Adjacency Structures
#'
#' @param areas Areas
#' @param population_agesex Population by age group and sex
#' @param spec Spec
#' @param level Admin level
#' @param quarter_id1 Quarter id1
#' @param quarter_id2 Quarter id2
#' @param age_group_ids Age group ids
#' @param sexes Sexes
#' @return Naomi model frame
#'
#' @export
naomi_model_frame <- function(areas,
                              population_agesex,
                              spec,
                              scope = areas$tree$area_id,
                              level = max(areas$tree$Get("area_level")),
                              quarter_id1,
                              quarter_id2,
                              age_group_ids = 1:17,
                              sexes = c("male", "female"),
                              omega = 0.7,
                              rita_param = list(OmegaT0      = 130 / 365,
                                                sigma_OmegaT = ((142-118) / 365) / (2*qnorm(0.975)),
                                                betaT0       = 0.0,
                                                sigma_betaT  = 0.00001,
                                                ritaT        = 1.0),
                              sigma_u_sd   = 1.0,
                              artattend_prior_sigma_scale = 3.0) {

  #' Prune areas below model level
  data.tree::Prune(areas$tree, function(x) x$area_level <= level)
  area_id <- areas$tree$Get("area_id", filterFun = data.tree::isLeaf)

  mf_areas <- data.frame(area_id,
                         area_idx = seq_along(area_id),
                         stringsAsFactors = FALSE) %>%
    dplyr::mutate(area_idf = factor(area_id, area_id))

  #' Model frame
  mf_model <- tidyr::crossing(
                       mf_areas,
                       sex = sexes,
                       age_group_id = age_group_ids
                     ) %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::mutate(area_idf = forcats::as_factor(area_id),
                  age_group_idf = forcats::as_factor(age_group_id))

  #' Add population estimates

  mf_model <- mf_model %>%
    dplyr::left_join(
             interpolate_population_agesex(population_agesex, quarter_id1) %>%
             dplyr::select(area_id, sex, age_group_id, population_t1 = population),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::left_join(
             interpolate_population_agesex(population_agesex, quarter_id2) %>%
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
  

  #' Add Spectrum inputs

  mf_model <- mf_model %>%
    dplyr::left_join(
             spec %>%
             ## !!! NEEDS UPDATE
             dplyr::filter(year == 2016) %>%
             dplyr::select(
               sex,
               age_group_id,
               spec_prev = prevalence,
               spec_incid = incidence,
               spec_artcov = art_coverage,
               asfr
             ),
             by = c("sex", "age_group_id")
           )

  #' Adjacency matrix
  M <- mf_areas %>%
    dplyr::mutate(geometry = areas$boundaries[area_id]) %>%
    sf::st_as_sf() %>%
    methods::as("Spatial") %>%
    spdep::poly2nb(.$area_id) %>%
    spdep::nb2mat(style = "B", zero.policy = TRUE)

  colnames(M) <- rownames(M)

  #' Scaled  precision matrix for 'BYM2' model.
  Q  <- INLA::inla.scale.model(diag(rowSums(M)) - M,
                               constr = list(A = matrix(1, 1, nrow(M)), e = 0))



  #' Model output

  outf <- naomi_output_frame(mf_model, areas)
  
  
  #' ART attendance model

  mf_areas <- mf_areas %>%
    dplyr::left_join(
             data.frame(area_idx = seq_len(nrow(M)),
                        n_neighbors = colSums(M)),
             by = "area_idx"
           )
  
  mf_artattend <- (M + diag(nrow(M))) %>%
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
    mutate(n_nb_lim = pmin(n_neighbors, 9)) %>%
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
    dplyr::mutate(gamma_or_mu = if_else(istar == 1, NA_real_, gamma_or_mu),
                  gamma_or_sigma = if_else(istar == 1, NA_real_, gamma_or_sigma))

  #' Incidence model
  
  mf_model <- mf_model %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::filter(age_group_id %in% age_group_ids) %>%
             mutate(
               age15to49 = as.integer(age_group_start >= 15 &
                                      (age_group_start + age_group_span) <= 50)
             ) %>%
             dplyr::select(age_group_id, age15to49),
             by = "age_group_id"
           ) %>%
    dplyr::group_by(area_id) %>%
    dplyr::mutate(
             spec_prev15to49 = sum(population_t1 * spec_prev) / sum(population_t1),
             spec_artcov15to49 =
               sum(population_t1 * spec_prev * spec_artcov) /
               sum(population_t1 * spec_prev),
             log_lambda_offset =
               log(spec_incid) - log(spec_prev15to49) - log(1 - omega * spec_artcov15to49)
           ) %>%
  dplyr::ungroup()
  
  v <- list(mf_model = mf_model,
            mf_out = outf$mf,
            mf_areas = mf_areas,
            mf_artattend = mf_artattend,
            A_out = outf$A,
            age_group_ids = age_group_ids,
            sexes = sexes,
            quarter_id1 = quarter_id1,
            quarter_id2 = quarter_id2,
            omega = omega,
            rita_param = rita_param,
            M = M,
            Q = Q)
  
  class(v) <- "naomi_mf"
  v
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
                  

  #' TODO: Check agegr are non overlapping

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
#'
#' @export
survey_prevalence_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf) {

  prev_dat <- naomi_mf$mf_model %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == "prev"),
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
                           indicator == "artcov"),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)
  
  artcov_dat
}

#' @rdname survey_prevalence_mf
#' @export
survey_recent_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf,
                             min_age = 15, max_age = 80) {

  recent_dat <- naomi_mf$mf_model %>%
    dplyr::left_join(get_age_groups()) %>%
    dplyr::filter(age_group_start >= min_age,
                  age_group_start + age_group_span <= max_age) %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == "recent"),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)
  
  recent_dat
}


#' Prepare Model Frames for Programme Datasets
#'
#' @export
anc_testing_prev_mf <- function(quarter_ids, anc_testing, naomi_mf) {

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
               quarter_id %in% quarter_ids,
               area_id %in% naomi_mf$mf_model$area_id
             ) %>%
      dplyr::group_by(area_id) %>%
      dplyr::summarise_at(vars(ancrt_hiv_status, ancrt_known_pos, ancrt_test_pos), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ancrt_totpos = ancrt_known_pos + ancrt_test_pos) %>%
      dplyr::transmute(
               area_id,
               anc_idx = dplyr::row_number(),
               anc_prev_x = ancrt_totpos,
               anc_prev_n = ancrt_hiv_status
             ) 
  }

  anc_prev_dat 
}


#' @rdname anc_testing_prev_mf
#' @export
anc_testing_artcov_mf <- function(quarter_ids, anc_testing, naomi_mf) {

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
               quarter_id %in% quarter_ids,
               area_id %in% naomi_mf$mf_model$area_id
             ) %>%
      dplyr::group_by(area_id) %>%
      dplyr::summarise_at(vars(ancrt_known_pos, ancrt_test_pos, ancrt_already_art), sum, na.rm = TRUE) %>%
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
#' @export
artnum_mf <- function(quarter_id, art_number, naomi_mf) {

  if(is.null(art_number)) {
    ## No number on ART data
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
      dplyr::filter(quarter_id == !!quarter_id,
                    area_id %in% naomi_mf$mf_areas$area_id) %>%
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
