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
#' @param artcov_definition Definition to use for calculate ART coverage.
#' @param by_restype Whether to stratify estimates by urban/rural restype; logical.
#'
#'
#' @details
#'
#' The argument `artcov_definition` controls whether to use both ARV biomarker and
#' self-report (`artcov_definition = "both"`; default), ARV biomarker only
#' (`artcov_definition = "arv"`), or self-report ART use only
#' (`artcov_definition = "artself"`).  If option is `"both"`, then all HIV positive
#' are used as the denomiator and no missing data on either indicator are
#' incorporated. If the option is `"arv"` or `"artself"` then missing values in those
#' variables, respectively, are treated as missing.
#'
#' @export
calc_survey_hiv_indicators <- function(survey_meta,
                                       survey_regions,
                                       survey_clusters,
                                       survey_individuals,
                                       survey_biomarker,
                                       areas,
                                       sex = c("male", "female", "both"),
                                       age_group_id = NULL,
                                       area_top_level = min(areas$area_level),
                                       area_bottom_level = max(areas$area_level),
                                       artcov_definition = c("both", "arv", "artself"),
                                       by_restype = FALSE) {

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

  if(by_restype)
    ind <- dplyr::bind_rows(ind, dplyr::mutate(ind, restype = "all"))
  else
    ind <- dplyr::mutate(ind, restype = "all")


  ## 5. Construct ART coverage indicator as either self-report or ART biomarker
  ##    and gather to long dataset for each biomarker

  if(artcov_definition[1] == "both") {
    ind <- ind %>%
      dplyr::group_by(survey_id) %>%
      dplyr::mutate(has_artcov = any(!is.na(arv) | !is.na(artself)),
                    artcov = dplyr::case_when(!has_artcov ~ NA_integer_,
                                              hivstatus == 0 ~ NA_integer_,
                                              arv == 1 | artself == 1 ~ 1L,
                                              TRUE ~ 0L)) %>%
      dplyr::select(-has_artcov, -artself, -arv) %>%
      dplyr::ungroup()
  } else if(artcov_definition[1] %in% c("arv", "artself")) {
    ind <- dplyr::rename(ind, artcov = artcov_definition[1]) %>%
      dplyr::mutate(artcov = dplyr::if_else(hivstatus == 0,  NA_integer_, as.integer(artcov)))
  } else {
    stop(paste("Invalid artcov_definition value:", artcov_definition[1]))
  }


  ind <- ind %>%
    dplyr::rename(prev = hivstatus) %>%
    tidyr::gather(indicator, est, prev, artcov, vls, recent) %>%
    dplyr::filter(!is.na(est))

  ## 6. Calculate outcomes
  ## Note: using survey region as strata right now. Most DHS use region + restype

  dat <- ind %>%
    dplyr::filter(!is.na(hivweight), hivweight > 0)

  cnt <- dat %>%
    dplyr::group_by(indicator, survey_id, area_id, restype, sex, age_group) %>%
    dplyr::summarise(n_cluster = dplyr::n_distinct(cluster_id),
                     n_obs = dplyr::n()) %>%
    dplyr::ungroup()

  datspl <- dat %>%
    dplyr::mutate(spl = paste(indicator, survey_id, area_level, restype, sex, age_group)) %>%
    split(.$spl)

  do_svymean <- function(df) {

    des <- survey::svydesign(~cluster_id,
                             data = df,
                             strata = ~survey_id + survey_region_id,
                             nest = TRUE,
                             weights = ~hivweight)

    survey::svyby(~est,
                  ~ indicator + survey_id + area_id + restype + sex + age_group,
                  des, survey::svymean)
  }

  options(survey.lonely.psu="adjust")
  mc.cores <- if(.Platform$OS.type == "windows") 1 else parallel::detectCores()
  est_spl <- parallel::mclapply(datspl, do_svymean, mc.cores = mc.cores)

  val <- cnt %>%
    dplyr::full_join(
             dplyr::bind_rows(est_spl),
             by = c("indicator", "survey_id", "area_id", "restype", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             survey_meta %>% select(survey_id, survey_mid_calendar_quarter),
             by = c("survey_id")
    ) %>%
    dplyr::left_join(
             areas %>%
             dplyr::select(area_id, area_level, area_sort_order),
             by = c("area_id")
           ) %>%
    dplyr::arrange(
             factor(indicator, c("prev", "artcov", "vls", "recent")),
             survey_id,
             survey_mid_calendar_quarter,
             area_level,
             area_sort_order,
             area_id,
             factor(restype, c("all", "urban", "rural")),
             factor(sex, c("both", "male", "female")),
             age_group
           ) %>%
    dplyr::select(
             indicator, survey_id, survey_mid_calendar_quarter, area_id, restype, sex, age_group,
             n_cluster, n_obs, est, se) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
             ci_l = if_else(!est %in% 0:1, stats::plogis(stats::qlogis(est) - stats::qnorm(0.975) * se / (est * (1-est))), NA_real_),
             ci_u = if_else(!est %in% 0:1, stats::plogis(stats::qlogis(est) + stats::qnorm(0.975) * se / (est * (1-est))), NA_real_)
           )

  val
}


#' Find Calendar Quarter Midpoint of Two Dates
#'
#' @param start_date vector coercibel to Date
#' @param end_date vector coercibel to Date
#'
#' @return A vector of calendar quarters
#'
#' @examples
#' start <- c("2005-04-01", "2010-12-01", "2016-01-01")
#' end <-c("2005-08-01", "2011-05-01", "2016-06-01")
#'
#' mid_calendar_quarter <- get_mid_calendar_quarter(start, end)
#'
#' @export
get_mid_calendar_quarter <- function(start_date, end_date) {

  start_date <- lubridate::decimal_date(as.Date(start_date))
  end_date <- lubridate::decimal_date(as.Date(end_date))

  stopifnot(!is.na(start_date))
  stopifnot(!is.na(end_date))
  stopifnot(start_date <= end_date)

  date4 <- (start_date + end_date) / 2
  year <- floor(date4)
  quarter <- floor((date4 %% 1) * 4) + 1

  paste0("CY", year, "Q", quarter)
}
