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
                              level,
                              quarter_id1,
                              quarter_id2,
                              age_group_ids = 4:17,
                              sexes = c("male", "female")) {

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

  area_id_out <- areas$tree$Get("area_id",
                                filterFun = function(x) x$display_level,
                                traversal = "level")
  area_id_out_leaves <- areas$tree$Get("leaves",
                                       filterFun = function(x) x$display_level,
                                       traversal = "level") %>%
    lapply(data.tree::Get, "area_id")

  area_id_out_leaves <- area_id_out_leaves[!duplicated(area_id_out)]
  area_id_out <- area_id_out[!duplicated(area_id_out)]

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
    setNames(paste0(names(.), "_out")) %>%
    tidyr::crossing(get_age_groups() %>%
                    dplyr::filter(age_group_id %in% age_group_ids)) %>%
    dplyr::filter(age_group_start_out <= age_group_start,
                  age_group_span_out == Inf |
                  (age_group_start + age_group_span) <=
                  (age_group_start_out + age_group_span_out)) %>%
    dplyr::select(age_group_id_out, age_group_id)

  stopifnot(age_group_ids %in% age_group_join$age_group_id)

  df_join <- tidyr::crossing(area_id_join, sex_join, age_group_join) %>%
    dplyr::full_join(mf_model, by = c("area_id", "sex", "age_group_id")) %>%
    dplyr::full_join(
             mf_out %>%
             dplyr::mutate(out_idx = dplyr::row_number())
            ,
             by = c("area_id_out" = "area_id",
                    "sex_out" = "sex",
                    "age_group_id_out" = "age_group_id")
           ) %>%
    mutate(x = 1)

  A_out <- Matrix::spMatrix(nrow(mf_out),
                            nrow(mf_model),
                            df_join$out_idx,
                            df_join$idx,
                            df_join$x)


  v <- list(mf_model = mf_model,
            mf_out = mf_out,
            A_out = A_out,
            quarter_id1 = quarter_id1,
            quarter_id2 = quarter_id2,
            mf_areas = mf_areas,
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
#' @param age_group_ids Age group ids.
#'
#' @keywords internal
get_age_group_id_out <- function(age_group_ids) {

  agegr <- get_age_groups() %>%
    dplyr::filter(age_group_id %in% age_group_ids)

  age_min <- min(agegr$age_group_start)
  age_max <- max(agegr$age_group_start + agegr$age_group_span)

  val <- dplyr::filter(get_age_groups(),
                       age_group_start >= age_min,
                       is.infinite(age_max) |
                       age_group_start + age_group_span <= age_max)

  val$age_group_id
}


get_sex_out <- function(sexes) {

  if(!all(c("male", "female") %in% sexes))
    sex_out <- sexes
  else
    sex_out <- c("both", "male", "female")

  sex_out
}

#' Calculate Posterior Mean and Uncertainty Via TMB
#'
#' @param naomi_fit Fitted TMB model.
#'
#' @export
report_tmb <- function(naomi_fit) {
  naomi_fit$sdreport <- TMB::sdreport(naomi_fit$obj, naomi_fit$par,
                                      bias.correct = TRUE)
  naomi_fit
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
             filter(survey_id %in% survey_ids,
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
survey_recent_mf <- function(survey_ids, survey_hiv_indicators, naomi_mf) {

  recent_dat <- naomi_mf$mf_model %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             filter(survey_id %in% survey_ids,
                    indicator == "recent"),
             by = c("area_id", "sex", "age_group_id")
           ) %>%
    dplyr::mutate(n = n_obs,
                  x = n * est) %>%
    dplyr::select(idx, area_id, age_group_id, sex, survey_id, n, x, est, se)
  
  recent_dat
}
