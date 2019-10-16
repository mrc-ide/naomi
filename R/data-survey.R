#' Convert Date to Century Month Code (CMC)
#'
#' Converts a date to DHS Century Month Code (CMC).
#'
#' @param date a Date vector
#'
#' @return an integer vector of CMC dates
#'
#' @details
#' CMC date is defined as the number of months since 1900:
#' \deqn{cmc = (year - 1900) * 12 + momth}
#'
#' @references
#' https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm?rhtocid=_4_2_0#Structure_of_DHS_Databc-1
#'
#' @examples
#' cmc_date(Sys.Date())
#' cmc_date(as.Date("1987-02-11", format = "%Y-%m-%d"))
#'
#' @export
cmc_date <- function(date) {
  stopifnot(inherits(date, "Date"))
  12 * (as.integer(format(date, "%Y")) - 1900) + as.integer(format(date, "%m"))
}

#' Expand list of clusters at each area level
#'
#' This function recursively expands the list of clusters to produce a list
#' of survey clusters within areas at each level.
#'
#' TODO: These should be examples - where is areas_long.rds now?
#' areas_long <- readRDS(here::here("data/areas/areas_long.rds"))
#' survey_clusters <- readRDS(here::here("data/survey/survey_clusters.rds"))
#' survey_regions <- readRDS(here::here("data/survey/survey_regions.rds"))
#'
#' expand_survey_clusters(survey_clusters, areas_long)
#'
#' Get clusters at level 1 areas only
#' expand_survey_clusters(survey_clusters, areas_long, top_level = 1, bottom_level = 1)
#'
#' @keywords internal
expand_survey_clusters <- function(survey_clusters,
                                   survey_regions,
                                   areas,
                                   top_level = min(areas$area_level),
                                   bottom_level = max(areas$area_level)) {

  clusters <- survey_clusters %>%
    dplyr::select(survey_id, cluster_id, restype, survey_region_id, geoloc_area_id) %>%
    dplyr::left_join(
             survey_regions %>%
             dplyr::select(survey_id, survey_region_id, survey_region_area_id),
             by = c("survey_id", "survey_region_id")
           ) %>%
    dplyr::mutate(
      ## Get lowest known area for each cluster (geoloc or syvreg if not geocoded)
             area_id = if_else(is.na(geoloc_area_id), survey_region_area_id, geoloc_area_id),
             geoloc_area_id = NULL,
             survey_region_area_id = NULL
           ) %>%
    dplyr::left_join(
             areas %>%
             dplyr::select(area_id, area_level, parent_area_id) %>%
             dplyr::arrange(area_id, -area_level) %>%
             dplyr::group_by(area_id) %>%
             dplyr::filter(row_number() == 1),
      by = "area_id"
    )

  val <- clusters %>%
    dplyr::filter(between(area_level, top_level, bottom_level))

  #' Recursion
  while(any(clusters$area_level >= top_level)) {

    clusters <- clusters %>%
      dplyr::mutate(area_id = parent_area_id,
                    area_level = area_level - 1L,
                    parent_area_id = NULL) %>%
      dplyr::inner_join(areas %>% select(area_id, area_level, parent_area_id),
                        by = c("area_id", "area_level"))

    val <- dplyr::bind_rows(
                    val,
                    clusters %>%
                    dplyr::filter(between(area_level, top_level, bottom_level))
                  )
  }

  val$parent_area_id <- NULL

  return(val)
}


#' Calculate age/sex/area stratified survey estimates for biomarker outcomes
#'
#' @details
#' All other data will be subsetted based on the `survey_id` values appearing in
#' survey_meta, so if only want to calculate for a subset of surveys it is
#' sufficient to pass subset for survey_meta and full data frames for the others.
#'
#' Much of this function needs to be parsed out into more generic functions and
#' rewritten to be more efficient.
#'   * Age group would be more efficient if traversing a tree structure.
#'   * Need generic function to calculate
#'   * Flexibility about age/sex stratifications to calculate.
#'
#' @param survey_meta Survey metadata.
#' @param survey_regions Survey regions.
#' @param survey_clusters Survey clusters.
#' @param survey_individuals Survey individuals.
#' @param survey_biomarker Survey biomarkers.
#' @param areas Areas.
#' @param sex Sex.
#' @param age_group_id Age group id.
#' @param area_top_level Area top level.
#' @param area_bottom_level Area bottom level.
#'
calc_survey_hiv_indicators <- function(survey_meta,
                                       survey_regions,
                                       survey_clusters,
                                       survey_individuals,
                                       survey_biomarker,
                                       areas,
                                       sex = c("male", "female", "both"),
                                       age_group_id = NULL,
                                       area_top_level = min(areas$area_level),
                                       area_bottom_level = max(areas$area_level)) {

  ## 1. Identify age groups to calculate for each survey_id
  age_group <- get_age_groups()

  if(!is.null(age_group_id))
    age_group <- dplyr::filter(age_group, age_group_id %in% !!age_group_id)

  sex_age_group <- tidyr::crossing(sex, age_group)

  ## Only keep age groups that are fully contained within survey age range.
  ## For example, if survey sampled age 18-64, don't want to calculate
  ## aggregates for age 15-49.

  sex_age_group <- survey_meta %>%
    dplyr::select(survey_id, female_age_min, female_age_max,
                  male_age_min, male_age_max) %>%
    tidyr::crossing(sex_age_group) %>%
    dplyr::filter(age_group_start >= case_when(sex == "male" ~ male_age_min,
                                               sex == "female" ~ female_age_min,
                                               sex == "both" ~ pmin(male_age_min,
                                                                    female_age_min)),
                  age_group_start + age_group_span <= case_when(sex == "male" ~ male_age_max,
                                                                sex == "female" ~ female_age_max,
                                                                sex == "both" ~ pmin(male_age_max,
                                                                                     female_age_max)) + 1) %>%
    dplyr::select(survey_id, sex, age_group, age_group_label, age_group_start, age_group_span)


  ## 2. Expand clusters to identify all clusters within each area

  clust <- dplyr::filter(survey_clusters, survey_id %in% survey_meta$survey_id)
  clust_area <- expand_survey_clusters(clust, survey_regions, areas,
                                       area_top_level, area_bottom_level)

  clust_area <- clust_area %>%
    dplyr::arrange(survey_id, cluster_id, area_id, -area_level) %>%
    dplyr::group_by(survey_id, cluster_id, area_id) %>%
    dplyr::filter(row_number() == 1)

  ## 3. Expand individuals dataset to repeat for all individiuals within each
  ##    age/sex group for a given survey

  ind <- survey_individuals %>%
    dplyr::inner_join(survey_biomarker,
                      by = c("survey_id", "cluster_id", "household", "line")) %>%
    dplyr::filter(survey_id %in% survey_meta$survey_id,
                  !is.na(hivstatus)) %>%
    dplyr::select(survey_id, cluster_id, sex, age, hivweight, hivstatus, artself, arv, vls, recent) %>%
    dplyr::bind_rows({.} %>% mutate(sex = "both")) %>%
    dplyr::inner_join(sex_age_group, by = c("survey_id", "sex")) %>%
    dplyr::filter(age >= age_group_start,
                  age < age_group_start + age_group_span)

  ## 4. Join expanded age/sex and expanded cluster area map

  ind <- dplyr::inner_join(ind, clust_area, by = c("survey_id", "cluster_id"))


  ## 5. Construct ART coverage indicator as either self-report or ART biomarker
  ##    and gather to long dataset for each biomarker

  ind <- ind %>%
    dplyr::group_by(survey_id) %>%
    dplyr::mutate(has_artcov = any(!is.na(arv) | !is.na(artself)),
                  artcov = dplyr::case_when(!has_artcov ~ NA_integer_,
                                            hivstatus == 0 ~ NA_integer_,
                                            arv == 1 | artself == 1 ~ 1L,
                                            TRUE ~ 0L)) %>%
    dplyr::select(-has_artcov, -artself, -arv) %>%
    dplyr::rename(prev = hivstatus) %>%
    tidyr::gather(indicator, est, prev, artcov, vls, recent) %>%
    dplyr::filter(!is.na(est))

  ## 6. Calculate outcomes
  ## Note: using survey region as strata right now. Most DHS use region + restype

  dat <- ind %>%
    dplyr::filter(!is.na(hivweight), hivweight > 0)

  cnt <- dat %>%
    dplyr::group_by(indicator, survey_id, area_id, sex, age_group) %>%
    dplyr::summarise(n_cluster = dplyr::n_distinct(cluster_id),
                     n_obs = dplyr::n()) %>%
    dplyr::ungroup()

  datspl <- dat %>%
    dplyr::mutate(spl = paste(indicator, survey_id, area_level, sex, age_group)) %>%
    split(.$spl)

  do_svymean <- function(df) {

    des <- survey::svydesign(~cluster_id,
                             data = df,
                             strata = ~survey_id + survey_region_id,
                             nest = TRUE,
                             weights = ~hivweight)

    survey::svyby(~est,
                  ~ indicator + survey_id + area_id + sex + age_group,
                  des, survey::svymean)
  }

  options(survey.lonely.psu="adjust")
  est_spl <- parallel::mclapply(datspl, do_svymean,
                                mc.cores = parallel::detectCores())

  val <- cnt %>%
    dplyr::full_join(
             dplyr::bind_rows(est_spl),
             by = c("indicator", "survey_id", "area_id", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             survey_meta %>% select(iso3, survey_id, survey_year),
             by = c("survey_id")
    ) %>%
    dplyr::left_join(
             areas %>%
             dplyr::select(area_id, area_level, area_sort_order),
             by = c("area_id")
           ) %>%
    dplyr::arrange(
             iso3,
             fct_relevel(indicator, "prev", "artcov", "vls", "recent"),
             survey_id,
             survey_year,
             area_level,
             area_sort_order,
             area_id,
             fct_relevel(sex, "both", "male", "female"),
             age_group
           ) %>%
    dplyr::select(
             indicator, iso3, survey_id, survey_year, area_id, sex, age_group,
             n_cluster, n_obs, est, se) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
             ci_l = if_else(!est %in% 0:1, stats::plogis(stats::qlogis(est) - stats::qnorm(0.975) * se / (est * (1-est))), NA_real_),
             ci_u = if_else(!est %in% 0:1, stats::plogis(stats::qlogis(est) + stats::qnorm(0.975) * se / (est * (1-est))), NA_real_)
           )

  val
}
