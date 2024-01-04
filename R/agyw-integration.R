#' Format naomi outputs for PSE tool
#'
#' @param outputs Naomi output
#' @param options Naomi model options.
#'
#'
#' @return Naomi indicators formatted for the AGYW workbook.
#'
#' @export

agyw_format_naomi <- function(outputs, options){

  naomi_ind <- outputs$indicators %>%
    dplyr::filter(indicator %in% c("population", "plhiv", "infections","incidence",
                                   "prevalence"),
                  calendar_quarter == options$calendar_quarter_t2)

  area_labels <- outputs$meta_area %>%
    dplyr::select(area_id, area_name,area_level, spectrum_region_code)
  area_label_cols <- intersect(names(naomi_ind), names(area_labels))

  age_labels <- outputs$meta_age_group
  age_label_cols <- intersect(names(naomi_ind), names(age_labels))

  naomi_ind_labelled <- naomi_ind %>%
    dplyr::left_join(area_labels, by = area_label_cols) %>%
    dplyr::left_join(age_labels, by = age_label_cols)


  summarise_naomi_ind <- function(dat, age_cat) {

    if(age_cat == "Y015_024"){age_groups <- c("Y015_019", "Y020_024")}
    if(age_cat == "Y025_049"){age_groups <- c("Y025_029","Y030_034","Y035_039",
                                              "Y040_044", "Y045_049")}

    dat %>%
      dplyr::select(area_id, area_name, area_level, spectrum_region_code, calendar_quarter,
                    age_group, sex, indicator, mean) %>%
      tidyr::pivot_wider(names_from = indicator, values_from = mean) %>%
      dplyr::group_by(area_id, area_name, area_level,spectrum_region_code,
                      calendar_quarter, sex) %>%
      dplyr::summarise(
        "population" = sum(population * as.integer(age_group %in% age_groups)),
        "plhiv" = sum(plhiv * as.integer(age_group %in% age_groups)),
        "infections" = sum(infections * as.integer(age_group %in% age_groups)),
        .groups = "drop") %>%
      dplyr::mutate(age_group = age_cat,
                    incidence = (infections/(population - plhiv)),
                    prevalence = plhiv/population) %>%
      tidyr::pivot_longer(cols = c(population, plhiv, infections, incidence, prevalence),
                          names_to = "indicator",
                          values_to = "mean") %>%
      dplyr::mutate(age_group_label = dplyr::if_else(age_group == "Y015_024", "15-24", "25-49"))
  }

  # Naomi indicators for aggregate age groups
  df1 <- dplyr::bind_rows(summarise_naomi_ind(naomi_ind_labelled, "Y015_024"),
                          summarise_naomi_ind(naomi_ind_labelled, "Y025_049"))

  # Naomi indicators for 5-year age groups + 15-49
  df2 <- naomi_ind_labelled %>%
    dplyr::filter(age_group %in% c("Y015_019", "Y020_024", "Y025_029", "Y030_034",
                                   "Y035_039", "Y040_044", "Y045_049", "Y015_049")) %>%
    dplyr::select(names(df1)) %>%
    # Add aggregate indicators
    dplyr::bind_rows(df1) %>%
    # Format incidence from 1000 person years to 100 person years
    dplyr::mutate(mean = dplyr::if_else(indicator == "incidence", mean * 100, mean))

  # Format for workbook
  df3 <- df2 %>%
    dplyr::mutate(indicator = dplyr::recode(indicator,
                                            "population" = "Pop", "plhiv" = "PLHIV",
                                            "infections" = "new","incidence" = "Inci"),
                  sex = dplyr::recode(sex,
                                      "female" = "f", "male" = "m",
                                      "both" = "all"),
                  mean = as.character(mean))

  # Incidence categories
  df4 <- df3 %>%
    dplyr::filter(indicator == "Inci") %>%
    dplyr::mutate(mean = dplyr::case_when(mean < 0.3 ~ "Low",
                                          mean >= 0.3 & mean< 1 ~ "Moderate",
                                          mean >= 1 & mean < 3 ~ "High",
                                          mean >= 3 ~ "Very High",
                                          TRUE ~ NA_character_),
                  indicator = "Incicategory")

  # Incidence for all age groups + sexes
  df5 <- naomi_ind_labelled %>%
    dplyr::filter(indicator == "incidence", age_group == "Y000_999", sex == "both",
                  area_level == options$area_level)

  country <- outputs$meta_area$area_name[outputs$meta_area$area_id == options$area_scope]


  # Format
  naomi_wide <- dplyr::bind_rows(df3, df4) %>%
  dplyr::filter(area_level == options$area_level) %>%
  tidyr::pivot_wider(id_cols = c(area_id,area_name),
                     names_from = c(indicator,age_group_label,sex),
                     names_sep = "", values_from = mean) %>%
    dplyr::mutate(Country = country, newAll = df5$mean) %>%
    dplyr::select(Country,area_id,area_name,`Pop15-24all`,`Pop15-24f`,`Pop15-24m`,
                  `PLHIV15-24all`,`PLHIV15-24f`,`PLHIV15-24m`,
                  newAll, `new15-24all`,`new15-24f`,`new15-24m`,
                  `Inci15-24f`,`Incicategory15-24f`,`Inci15-24m`,`Incicategory15-24m`,
                  `Pop15-19all`,`Pop15-19f`,`Pop15-19m`,
                  `PLHIV15-19all`,`PLHIV15-19f`,`PLHIV15-19m`,
                  `new15-19all`,`new15-19f`,`new15-19m`,
                  `Inci15-19f`,`Incicategory15-19f`,`Inci15-19m`,`Incicategory15-19m`,
                  `Pop20-24all`,`Pop20-24f`,`Pop20-24m`,
                  `PLHIV20-24all`,`PLHIV20-24f`,`PLHIV20-24m`,
                  `new20-24all`,`new20-24f`,`new20-24m`,
                  `Inci20-24f`,`Incicategory20-24f`,`Inci20-24m`,`Incicategory20-24m`,
                  `Pop25-49all`,`Pop25-49f`,`Pop25-49m`,
                  `PLHIV25-49all`,`PLHIV25-49f`,`PLHIV25-49m`,
                  `new25-49all`,`new25-49f`,`new25-49m`,
                  `Inci25-49f`,`Incicategory25-49f`,`Inci25-49m`,`Incicategory25-49m`,
                  `Pop25-29all`,`Pop25-29f`,`Pop25-29m`,
                  `PLHIV25-29all`,`PLHIV25-29f`,`PLHIV25-29m`,
                  `new25-29all`,`new25-29f`,`new25-29m`,
                  `Inci25-29f`,`Incicategory25-29f`,`Inci25-29m`,`Incicategory25-29m`,
                  `Pop30-34all`,`Pop30-34f`,`Pop30-34m`,
                  `PLHIV30-34all`,`PLHIV30-34f`,`PLHIV30-34m`,
                  `new30-34all`,`new30-34f`,`new30-34m`,
                  `Inci30-34f`,`Incicategory30-34f`,`Inci30-34m`,`Incicategory30-34m`,
                  `Pop35-39all`,`Pop35-39f`,`Pop35-39m`,
                  `PLHIV35-39all`,`PLHIV35-39f`,`PLHIV35-39m`,
                  `new35-39all`,`new35-39f`,`new35-39m`,
                  `Inci35-39f`,`Incicategory35-39f`,`Inci35-39m`,`Incicategory35-39m`,
                  `Pop40-44all`,`Pop40-44f`,`Pop40-44m`,
                  `PLHIV40-44all`,`PLHIV40-44f`,`PLHIV40-44m`,
                  `new40-44all`,`new40-44f`,`new40-44m`,
                  `Inci40-44f`,`Incicategory40-44f`,`Inci40-44m`,`Incicategory40-44m`,
                  `Pop45-49all`,`Pop45-49f`,`Pop45-49m`,
                  `PLHIV45-49all`,`PLHIV45-49f`,`PLHIV45-49m`,
                  `new45-49all`,`new45-49f`,`new45-49m`,
                  `Inci45-49f`,`Incicategory45-49f`,`Inci45-49m`,`Incicategory45-49m`,
                  `Pop15-49all`,`Pop15-49f`,`Pop15-49m`,
                  `PLHIV15-49all`,`PLHIV15-49f`,`PLHIV15-49m`,
                  `new15-49all`,`new15-49f`,`new15-49m`,
                  `Inci15-49f`,`Incicategory15-49f`,`Inci15-49m`,`Incicategory15-49m`)

  v <- list(naomi_long = df2,
            naomi_wide = naomi_wide)

  }



#' Dissagreggate admin1 FSW proportions from Oli's KP model to 5-age groups
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param naomi_population Naomi population estimates for T2.
#' @param kp_consensus Key pop consensus estimates.
#'
#'
#' @return District level FSW estimates by 5-year age bands for ages 15-49.
#'
#' @export

agyw_disaggregate_fsw <- function(outputs,
                                  options,
                                  naomi_pop,
                                  kp_consensus)
{

  #' Extract country specific national FSW PSEs
  iso3 <- options$area_scope

  pse <- naomi.resources::load_agyw_exdata("kp_estimates", iso3) %>%
    dplyr::filter(kp == "FSW", indicator == "pse_prop")

  fsw_pse <- pse %>%
    dplyr::rename(prop_fsw = median) %>%
    dplyr::select(-indicator,-lower,-upper)

  age_groups <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034",
                  "Y035_039", "Y040_044", "Y045_049")

  #'Calculating FSW proportion of total female population
  fsw <- fsw_pse %>%
    dplyr::mutate(age_group = "Y015_049") %>%
    dplyr::left_join(naomi_pop %>% dplyr::filter(sex == "female"),
                     by = dplyr::join_by(iso3, area_id, age_group)) %>%
    dplyr::mutate(total_fsw = population * prop_fsw) %>%
    dplyr::select(iso3, area_id, total_fsw, age_group, area_level, spectrum_region_code)

  #' Check for consensus estimate of FSW
  fsw_consensus <- kp_consensus[kp_consensus$key_population == "FSW", ]$population_size

  if(!is.na(fsw_consensus)){

    # Check if consensus estimate is larger than age matched population denominator
    pop <- naomi_pop[naomi_pop$area_level == 0 & naomi_pop$age_group == "Y015_049" & naomi_pop$sex == "female",]$population
    stopifnot(fsw_consensus < pop)

    # Scale total FSW population to consensus PSE estimate
    fsw_scaled <- fsw %>%
      dplyr::mutate(
        relative_prop = total_fsw/sum(total_fsw),
        consensus_pse = fsw_consensus,
        total_fsw = consensus_pse * relative_prop)

    fsw <- fsw_scaled %>% dplyr::select(-consensus_pse, relative_prop)

  }


  #' FSW age distribution parameters in ZAF from Thembisa
  #' Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3
  gamma_mean <- 29
  gamma_sd <- 9
  beta <- gamma_mean / gamma_sd^2 #' rate
  alpha <- gamma_mean * beta #' shape

  #' Distribution function of the gamma
  zaf_gamma <- data.frame(
    dist = diff(pgamma(c(15, 20, 25, 30, 35, 40, 45, 50), shape = alpha, rate = beta)),
    age_group = age_groups) %>%
    dplyr::mutate(dist = dist / sum(dist))

  pskewlogis <- function(t, scale, shape, skew) {
    (1 + (scale * t)^-shape)^-skew
  }

  #' Calculate proportion of sexually active population using Kinh's country specific
  #' estimates of age at first sex and naomi population
  afs <- naomi.resources::load_agyw_exdata("afs", iso3)

  #' Select birth cohort from 2000, to turn 15 in 2015
  cohort <- 2000

  afs <- afs %>%
    dplyr::filter(yob == cohort, sex == "female", iso3 == options$area_scope) %>%
    dplyr::full_join(dplyr::select(fsw,iso3,area_id), multiple = "all", by = dplyr::join_by(iso3))

  df <- data.frame()

  #' Calculate sexually active population by age and sex for each district
  for (x in unique(afs$area_id)) {
    afs_x <- dplyr::filter(afs, area_id == x)
    ages <- 15:49

    df_x <- data.frame(
      area_id = x,
      age = ages,
      eversex = pskewlogis(
        ages,
        scale = afs_x$lambda,
        skew = afs_x$skew,
        shape = afs_x$shape
      ),
      age_group = rep(age_groups, each = 5)
    )

    df_x <- df_x %>%
      dplyr::group_by(area_id, age_group) %>%
      dplyr::summarise(eversex = mean(eversex), .groups = "drop") %>%
      dplyr::left_join(
        naomi_pop %>% dplyr::filter(sex == "female"),
        by = c("area_id", "age_group")
      ) %>%
      dplyr::mutate(
        eversexpop = eversex * population,
        eversexpop_prop = eversexpop / sum(eversexpop)
      )

    df <- dplyr::bind_rows(df, df_x)
  }

  #' Adjusting country specific sexual debut estimates with age distribution of
  #' FSW from Thembisa
  #'Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3
  zaf_propensity <- naomi.resources::load_agyw_exdata("zaf_propensity", iso3 = "ZAF") %>%
    dplyr::filter(kp == "FSW")

  fsw_est <- df %>%
    #  Add FSW propensity estimates from ZAF
    dplyr::left_join(zaf_propensity, by = "age_group") %>%
    # Calculate distribution of FSWs
    dplyr::mutate(dist = eversexpop_prop * propensity) %>%
    dplyr::group_by(area_id) %>%
    dplyr::mutate(dist = dist / sum(dist)) %>%
    dplyr::ungroup() %>%
    # Add FSW PSEs
    dplyr::full_join(
      fsw %>% dplyr::select(total_fsw, iso3, area_id, area_level),
      by = dplyr::join_by(area_id, iso3, area_level)
    ) %>%
    # Calculate FSW proportions
    dplyr::mutate(
      fsw = dist * total_fsw,
      fsw_prop = fsw / population,
      consensus_estimate = fsw_consensus
    ) %>%
    dplyr::select(-eversexpop, -eversexpop_prop, -propensity, -dist, -total_fsw)

  fsw_est

}


#' Disaggregate admin1 PWID proportions from Oli's KP model to 5-age groups
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param naomi_population Naomi population estimates for T2.
#' @param kp_consensus Key pop consensus estimates.
#'
#' @return District level PWID estimates by 5-year age bands for ages 15-49.

agyw_disaggregate_pwid <- function(outputs,
                                   options,
                                   naomi_pop,
                                   kp_consensus)
{

  #' Extract country specific national PWID PSEs
  iso3 <- options$area_scope

  pse <- naomi.resources::load_agyw_exdata("kp_estimates", iso3) %>%
    dplyr::filter(kp == "PWID", indicator == "pse_prop")

  pwid_pse <- pse %>%
    dplyr::rename(prop_pwid = median) %>%
    dplyr::select(-indicator,-lower,-upper)

  age_groups <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034",
                  "Y035_039", "Y040_044", "Y045_049")

  pwid <- pwid_pse %>%
    dplyr::mutate(age_group = "Y015_049") %>%
    dplyr::left_join(naomi_pop %>% dplyr::filter(sex == "male"),
                     by = dplyr::join_by(iso3, area_id, age_group)) %>%
    dplyr::mutate(total_pwid = population * prop_pwid) %>%
    dplyr::select(iso3, area_id, total_pwid, age_group, area_level)

  #' Check for consensus estimate of MSM
  pwid_consensus <- kp_consensus[kp_consensus$key_population == "PWID", ]$population_size

  if(!is.na(pwid_consensus)){

    # Check if consensus estimate is larger than age matched population denominator
    pop <- naomi_pop[naomi_pop$area_level == 0 & naomi_pop$age_group == "Y015_049" & naomi_pop$sex == "male",]$population
    stopifnot(pwid_consensus < pop)

    # Scale total PWID population to consensus PSE estimate
    pwid_scaled <- pwid %>%
      dplyr::mutate(
        relative_prop = total_pwid/sum(total_pwid),
        consensus_pse = pwid_consensus,
        total_pwid = consensus_pse * relative_prop)

    pwid <- pwid_scaled %>% dplyr::select(-consensus_pse, relative_prop)
  }


  #' Assumption from literature that 9% of PWID are female so remove them from
  #' the male denominator

  pwid$total_pwid <- pwid$total_pwid * 0.91

  #' PWID age distribution
  #' Review of literature - Hines et al Lancet Global Health 2020
  gamma_mean <- 29.4
  gamma_sd <- 7
  beta <- gamma_mean / gamma_sd^2 #' rate
  alpha <- gamma_mean * beta #' shape

  #' Distribution function of the gamma
  zaf_gamma <- data.frame(
    dist = diff(pgamma(c(15, 20, 25, 30, 35, 40, 45, 50), shape = alpha, rate = beta)),
    age_group = age_groups) %>%
    dplyr::mutate(dist = dist / sum(dist))


  # Naomi population
  pop <- naomi_pop %>%
    dplyr::filter(area_id %in% unique(pwid$area_id),
                  age_group %in% age_groups,
                  sex == "male")

  pwid_est <- dplyr::left_join(
    pop, zaf_gamma,
    by = dplyr::join_by(age_group)) %>%
    dplyr::full_join(
      dplyr::select(pwid,total_pwid, iso3, area_id),
      by = c("area_id", "iso3")
    ) %>%
    dplyr::mutate(pwid = dist * total_pwid,
                  pwid_prop = pwid / population,
                  consensus_estimate = pwid_consensus) %>%
    dplyr::select( -dist, -total_pwid, -sex)

  pwid_est
}

#' Disaggregate admin1 MSM proportions from Oli's KP model to 5-age groups
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param naomi_population Naomi population estimates for T2.
#' @param kp_consensus Key pop consensus estimates.
#'
#' @return District level MSM estimates by 5-year age bands for ages 15-49.

agyw_disaggregate_msm <- function(outputs,
                                  options,
                                  naomi_pop,
                                  kp_consensus)
{

  #' Extract country specific national MSM PSEs
  iso3 <- options$area_scope
  pse <- naomi.resources::load_agyw_exdata("kp_estimates", iso3) %>%
    dplyr::filter(kp == "MSM", indicator == "pse_prop")

  msm_pse <- pse %>%
    dplyr::rename(prop_msm = median) %>%
    dplyr::select(-indicator,-lower,-upper)

  age_groups <- c("Y015_019", "Y020_024", "Y025_029", "Y030_034",
                  "Y035_039", "Y040_044", "Y045_049")

  msm <- msm_pse %>%
    dplyr::mutate(age_group = "Y015_049") %>%
    dplyr::left_join(naomi_pop %>% dplyr::filter(sex == "male"),
                     by = dplyr::join_by(iso3, area_id, age_group)) %>%
    dplyr::mutate(total_msm = population * prop_msm) %>%
    dplyr::select(iso3, area_id, total_msm, age_group, area_level)

  #' Check for consensus estimate of MSM
  msm_consensus <- kp_consensus[kp_consensus$key_population == "MSM", ]$population_size

  if(!is.na(msm_consensus)){

    # Check if consensus estimate is larger than age matched population denominator
    pop <- naomi_pop[naomi_pop$area_level == 0 & naomi_pop$age_group == "Y015_049" & naomi_pop$sex == "male",]$population
    stopifnot(msm_consensus < pop)

    # Scale total MSM population to consensus PSE estimate
    msm_scaled <- msm %>%
      dplyr::mutate(
        relative_prop = total_msm/sum(total_msm),
        consensus_pse = msm_consensus,
        total_msm = consensus_pse * relative_prop)

    msm <- msm_scaled %>% dplyr::select(-consensus_pse, relative_prop)
  }


  #' MSM age distribution parameters in ZAF from Thembisa
  #' Downloaded from: https://www.thembisa.org/content/downloadPage/Thembisa4_3report
  gamma_mean <- 25
  gamma_sd <- 7
  beta <- gamma_mean / gamma_sd^2 #' rate
  alpha <- gamma_mean * beta #' shape

  #' Distribution function of the gamma
  zaf_gamma <- data.frame(
    dist = diff(pgamma(c(15, 20, 25, 30, 35, 40, 45, 50), shape = alpha, rate = beta)),
    age_group = age_groups) %>%
    dplyr::mutate(dist = dist / sum(dist))


  pskewlogis <- function(t, scale, shape, skew) {
    (1 + (scale * t)^-shape)^-skew
  }

  #' Calculate proportion of sexually active population using Kinh's country specific
  #' estimates of age at first sex and naomi population
  afs <- naomi.resources::load_agyw_exdata("afs", iso3)

  #' Select birth cohort from 2000, to turn 15 in 2015
  cohort <- 2000

  afs <- afs %>%
    dplyr::filter(yob == cohort, sex == "male", iso3 == options$area_scope) %>%
    dplyr::full_join(dplyr::select(msm,iso3,area_id), multiple = "all", by = dplyr::join_by(iso3))

  df <- data.frame()

  #' Calculate sexually active population by age and sex for each district
  for(x in unique(afs$area_id)) {
    afs_x <- dplyr::filter(afs, area_id == x)
    ages <- 15:49

    df_x <- data.frame(
      area_id = x,
      age = ages,
      eversex = pskewlogis(
        ages,
        scale = afs_x$lambda,
        skew = afs_x$skew,
        shape = afs_x$shape
      ),
      age_group = rep(age_groups, each = 5)
    )

    df_x <- df_x %>%
      dplyr::group_by(area_id, age_group) %>%
      dplyr::summarise(eversex = mean(eversex), .groups = "drop") %>%
      dplyr::left_join(
        naomi_pop %>% dplyr::filter(sex == "male"),
        by = c("area_id", "age_group")
      ) %>%
      dplyr::mutate(
        eversexpop = eversex * population,
        eversexpop_prop = eversexpop / sum(eversexpop)
      )

    df <- dplyr::bind_rows(df, df_x)
  }

  #' Adjusting country specific sexual debut estimates with age distribution of
  #' MSM from Thembisa
  zaf_propensity <- naomi.resources::load_agyw_exdata("zaf_propensity", iso3 = "ZAF") %>%
    dplyr::filter(kp == "MSM")


  msm_est <- df %>%
    #  Add MSM propensity estimates from ZAF
    dplyr::left_join(zaf_propensity, by = "age_group") %>%
    # Calculate distribution of MSM
    dplyr::mutate(dist = eversexpop_prop * propensity) %>%
    dplyr::group_by(area_id) %>%
    dplyr::mutate(dist = dist / sum(dist)) %>%
    dplyr::ungroup() %>%
    # Add MSM PSEs
    dplyr::full_join(
      msm %>% dplyr::select(total_msm, iso3, area_id, area_level),
      by = dplyr::join_by(area_id, iso3, area_level)
    ) %>%
    # Calculate FSW proportions
    dplyr::mutate(msm = dist * total_msm,
                  msm_prop = msm / population,
                  consensus_estimate = msm_consensus) %>%
    dplyr::select(-eversexpop, -eversexpop_prop, -propensity, - dist, -total_msm)

  msm_est
}

#' Adjust female sexual behavior risk groups by FSW proportions
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param fsw_est 5-year estimates of FSW PSEs generated from `agyw_disaggregate_fsw()`.
#' @param female_sae_path Path to female estimates of sexual behavior risk group.
#'
#' @return District level estimates of female sexual risk behaviour groups
#'
#' Estimates are generated for the following groups:
#'
#' * `nosex12m`:
#' * `sexcohab`:
#' * `sexnonregplus`:
#' * `sexnonreg`:
#' * `sexpaid12m`:
#' * `nosex12m`:
#'
#' Calculation steps:
#' 1. Align admin0/admin1 FSW proportions with SRB SAE estimates.
#' 2. Subtract the proportion of FSW from total high risk female population.


agyw_adjust_sexbehav_fsw <- function(outputs,
                                     options,
                                     fsw_est)
{


  #' Match FSW estimates (admin0 or admin1) with SAE estimates
  fsw_analysis_level <- paste0("area_id",unique(fsw_est$area_level))

  areas_wide <-  naomi::spread_areas(outputs$meta_area) %>%
    sf::st_drop_geometry()

  map <- dplyr::select(areas_wide, area_id, dplyr::all_of(fsw_analysis_level)) %>%
    dplyr::rename(fsw_match_area = 2)

  #' Allocate admin1 FSW proportions
  fsw_df <- fsw_est %>% dplyr::select(age_group, fsw_match_area = area_id, fsw_prop)

  #' Load female SRB proportions
  female_srb <- naomi.resources::load_agyw_exdata("srb_female", options$area_scope)

  adj_female_srb <- female_srb %>%
    dplyr::filter(iso3 == options$area_scope) %>%
    dplyr::left_join(map, by = dplyr::join_by(area_id)) %>%
    dplyr::left_join(fsw_df, by = dplyr::join_by(age_group, fsw_match_area)) %>%
    dplyr::select(iso3, year, indicator, survey_id, age_group, area_id, area_name, estimate_smoothed, fsw_prop) %>%
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = estimate_smoothed
    ) %>%
    # Subtracting proportion FSW from total high risk female population
    dplyr::mutate(
      sexpaid12m = fsw_prop,
      sexnonreg = 1 - nosex12m - sexcohab - sexpaid12m,
      fsw_prop = NULL
    ) %>%
    tidyr::pivot_longer(
      cols = c(nosex12m,sexcohab,sexnonregplus,sexnonreg,sexpaid12m),
      names_to = "indicator",
      values_to = "estimate_smoothed"
    )

  adj_female_srb

}

#' Adjust male sexual behavior risk groups by MSM + PWID proportions
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param msm_est 5-year estimates of MSM PSEs generated from `agyw__disaggregate_msm()`.
#' @param pwid_est 5-year estimates of MSM PSEs generated from `agyw__disaggregate_pwid()`.
#' @param sae_path Path to female estimates of sexual behavior risk group.
#'
#' @return District level estimates of male sexual risk behaviour groups
#'
#' Estimates are generated for the following groups:
#'
#' * `nosex12m`:
#' * `sexcohab`:
#' * `sexonregplus`:
#' * `sexonreg`:
#' * `msm`:
#' * `pwid`:
#'
#'
#' Calculation steps:
#' 1. Align admin0/admin1 MSM and PWID proportions with SRB SAE estimates.
#' 2. Subtracting MSM and PWID proportionally from all SRB groups.
#'

agyw_adjust_sexbehav_msm_pwid <- function(outputs,
                                          options,
                                          msm_est,
                                          pwid_est) {


  # Match KP estimates (admin0 or admin1) with SAE estimates
  msm_analysis_level <- paste0("area_id",unique(msm_est$area_level))

  areas_wide <-  naomi::spread_areas(outputs$meta_area) %>%
    sf::st_drop_geometry()

  map <- dplyr::select(areas_wide, area_id, dplyr::all_of(msm_analysis_level)) %>%
    dplyr::rename(kp_match_area = 2)

  #   Allocate KP
  msm_df <- msm_est %>% dplyr::select(age_group, kp_match_area = area_id, msm_prop)
  pwid_df <- pwid_est %>% dplyr::select(age_group, kp_match_area = area_id, pwid_prop)

  #' Load male SRB proportions
  male_srb <- naomi.resources::load_agyw_exdata("srb_male", options$area_scope)

  adj_male_srb <- male_srb %>%
    dplyr::filter(iso3 == options$area_scope) %>%
    dplyr::left_join(map, by = dplyr::join_by(area_id)) %>%
    dplyr::left_join(msm_df, by = dplyr::join_by(age_group, kp_match_area)) %>%
    dplyr::left_join(pwid_df, by = dplyr::join_by(age_group, kp_match_area)) %>%
    dplyr::select(iso3, year, indicator, survey_id, age_group, area_id, area_name,
                  estimate_smoothed, msm_prop, pwid_prop) %>%
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = estimate_smoothed
    ) %>%
    #' Subtracting MSM and PWID proportionally from all SRB risk groups
    #' (FSW was just from high-risk females)
    dplyr::mutate(
      nosex12m = nosex12m * (1 - pwid_prop - msm_prop),
      sexcohab = sexcohab * (1 - pwid_prop - msm_prop),
      sexnonregplus = sexnonregplus * (1 - pwid_prop - msm_prop),
      sexnonreg = sexnonregplus,
      msm = msm_prop, msm_prop = NULL,
      pwid = pwid_prop, pwid_prop = NULL
    ) %>%
    tidyr::pivot_longer(
      cols = c(nosex12m, sexcohab, sexnonregplus, sexnonreg, msm, pwid),
      names_to = "indicator",
      values_to = "estimate_smoothed"
    )

  adj_male_srb

}

#' Calculate prevalence for female SRB groups.
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param fsw_est 5-year estimates of MSM PSEs generated from `agyw_disaggregate_fse()`.
#' @param female_sexbehav KP adjusted estimates of female SRB groups generated by `agyw_adjust_sexbehav_fsw()`
#' @param female_hiv_path Path to SRB HIV estimates from household surveys (last updated XX-XX-XX).
#' @param pse_path Path to KP PSEs last updated (XX-XX-XX).
#' @param survey_year Year of survey to sample estimates.
#'
#' @return SRB PSEs with logit prevalence estimates.
#'
#' To calculate district-age-sex-sexual behaviour-specific HIV prevalence, we maintain
#' HIV prevalence from Naomi for a district-age-sex, but disaggregate to different
#' risk behaviours using:
#'  (1) HIV prevalence ratios from household surveys for those reporting no
#'  sex vs one cohabiting vs non-regular sexual partner(s), and
#'  (2) a linear regression through admin-1 level estimates of the ratio of KP
#'  prevalence to gen-pop prevalence used to predict an age-district-specific
#'  FSW to general population prevalence ratio.
#'
agyw_calculate_prevalence_female <- function(naomi_output,
                                             options,
                                             fsw_est,
                                             female_srb,
                                             survey_year_sample = 2018) {

  #' Naomi estimates of PLHIV and population by district and age band
  naomi_est <- naomi_output %>%
    dplyr::filter(calendar_quarter == options$calendar_quarter_t2,
                  sex == "female",
                  indicator %in% c("population", "plhiv", "infections", "prevalence")) %>%
    dplyr::select(area_id, area_level, age_group, indicator, mean) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = mean) %>%
    dplyr::rename(gen_prev = prevalence)

  # Naomi general population prevalence
  genpop_prev <- naomi_est %>%
    dplyr::filter(age_group == "Y015_049") %>%
    dplyr::select(area_id, gen_prev)

  #' Extract country specific national FSW prevalence
  iso3 <- options$area_scope
  #' THIS IS NOW USING SINGLE COUNTRY INSTEAD OF ALL COUNTRIES
  fsw_prev <- naomi.resources::load_agyw_exdata("kp_estimates", iso3) %>%
    dplyr::filter(kp == "FSW", indicator == "prevalence")

  kp_prev <- fsw_prev %>%
    dplyr::select(iso3,area_id,median) %>%
    dplyr::left_join(genpop_prev, by = dplyr::join_by(area_id)) %>%
    dplyr::mutate(prev_fsw_logodds = log(median / (1 - median)),
                  prev_logodds = log(gen_prev / (1 - gen_prev)))

  #' KP regression: FSW prevalence relative to general prevalence
  #' ########## THIS REGRESSION SHOULD BE TAKING DATA FROM ALL ADMIN-1 LEVEL
  kp_fit <- lm(prev_fsw_logodds ~ prev_logodds, data = kp_prev)

  #' Modelled estimates of proportion in each risk group
  risk_group_prop <- female_srb %>%
    dplyr::filter(year == survey_year_sample) %>%
    dplyr::select(area_id, age_group, indicator, estimate_smoothed) %>%
    tidyr::pivot_wider( names_from = indicator, values_from = estimate_smoothed,
                        values_fn = mean, names_prefix = "prop_") %>%
    dplyr::right_join(naomi_est,  by = c("area_id", "age_group")) %>%
    dplyr::filter(!is.na(prop_nosex12m)) %>%
    dplyr::mutate(
      population_nosex12m = population * prop_nosex12m,
      population_sexcohab = population * prop_sexcohab,
      population_sexnonreg = population * prop_sexnonreg,
      population_sexpaid12m = population * prop_sexpaid12m
    )

  #' Calculate prevalence in each category
  calculate_prevalence <- function(x, iso3){

    #' Log odds ratio from SRB group survey prevalence
    lor <- naomi.resources:::load_agyw_exdata("srb_survey_lor", iso3) %>%
      dplyr::filter(sex == "female")

    lor_15to29 <- lor$lor_15to29
    names(lor_15to29) <- lor$srb_group

    lor_30to49 <- lor$lor_30to49
    names(lor_30to49) <- lor$srb_group

    if (x$age_group[1] %in% c("Y015_019","Y020_024","Y025_029")) {
      lor <- lor_15to29
    } else {
      lor <- lor_30to49
    }

    population_fine <- dplyr::filter(x, indicator == "population")$estimate
    plhiv <- x$plhiv[1]
    ywkp_lor <- c("ywkp_lor" = x$ywkp_lor[1])
    lor <- c(lor[1:3],ywkp_lor)
    prev <- logit_scale_prev(lor, population_fine, plhiv)
    y <- dplyr::filter(x, indicator == "prop") %>%
      dplyr::mutate( indicator = "prev", estimate = prev)
    dplyr::bind_rows(x, y)
  }

  #' Calculate logit prevalence and format
  logit_prev <- risk_group_prop %>%
    dplyr::mutate(ywkp_lor = calculate_ywkp_pr_lor(gen_prev, fit = kp_fit)$lor) %>%
    dplyr::select(-starts_with("pr_"), -gen_prev) %>%
    tidyr::pivot_longer(
      cols = starts_with(c("population_", "prop_")),
      names_to = "indicator",
      values_to = "estimate"
    ) %>%
    tidyr::separate(
      indicator,
      into = c("indicator", "behav")
    ) %>%
    dplyr::filter(behav %in% c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")) %>%
    split(~ area_id + age_group) %>%
    lapply(calculate_prevalence, iso3) %>%
    dplyr::bind_rows() %>%
    tidyr::unite("indicator", indicator, behav, sep = "_") %>%
    tidyr::pivot_wider( names_from = indicator, values_from = estimate) %>%
    dplyr::mutate_if(is.numeric, as.numeric) %>%
    dplyr::mutate_if(is.factor, as.character)

  logit_prev


}

#' Calculate prevalence for male SRB groups.
#'
#' @param outputs Naomi output.
#' @param options Naomi model options.
#' @param male_srb
#' @param areas
#' @param msm_est .
#' @param survey_year Year of survey to sample estimates.
#'
#' @return SRB PSEs with logit prevalence estimates.
#'
#' To calculate district-age-sex-sexual behaviour-specific HIV prevalence, we maintain
#' HIV prevalence from Naomi for a district-age-sex, but disaggregate to different
#' risk behaviours using:
#'  (1) HIV prevalence ratios from household surveys for those reporting no
#'  sex vs one cohabiting vs non-regular sexual partner(s), and
#'  (2) admin-1 level estimates of the ratio of KP prevalence to gen-pop prevalence
#'   among 15-24 year olds for MSM (due to the young age distribution of MSM) or
#'   among 15-49 year olds for PWID (due to the older age distribution of PWID)
#'   applied to all age groups among MSM and PWID in districts by admin-1 unit.


agyw_calculate_prevalence_male <- function(naomi_output,
                                           areas,
                                           options,
                                           msm_est,
                                           male_srb,
                                           survey_year_sample = 2018) {

  #' Naomi estimates of PLHIV and population by district and age band
  naomi_est <- naomi_output %>%
    dplyr::filter(calendar_quarter == options$calendar_quarter_t2,
                  sex == "male",
                  indicator %in% c("population", "plhiv", "infections", "prevalence")) %>%
    dplyr::select(area_id, area_level, age_group, indicator, mean) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = mean) %>%
    dplyr::rename(gen_prev = prevalence)

  # Naomi general population prevalence
  genpop_prev <- naomi_est %>%
    dplyr::filter(age_group == "Y015_024" | age_group == "Y015_049") %>%
    dplyr::select(area_id, area_level, age_group, gen_prev) %>%
    tidyr::pivot_wider(names_from = age_group, values_from = gen_prev) %>%
    dplyr::mutate(logit_gen_prev_msm = log(Y015_024 / (1 - Y015_024)),
                  logit_gen_prev_pwid = log(Y015_049 / (1 - Y015_049))) %>%
    dplyr::select(area_id, logit_gen_prev_msm, logit_gen_prev_pwid, area_level)

  #' Extract country specific national MSM + PWID prevalence
  iso3 <- options$area_scope

  msm_pwid_prev <- naomi.resources::load_agyw_exdata("kp_estimates", iso3) %>%
    dplyr::filter(indicator == "prevalence", kp %in% c("MSM", "PWID"))

 # KP population prevalence
  kp_prev <- msm_pwid_prev %>%
    dplyr::select(-indicator,-lower, -upper) %>%
    dplyr::mutate(median = log(median / (1 - median))) %>%
    # Add in Naomi general pop prevalence
    dplyr::left_join(genpop_prev, by = dplyr::join_by(area_id)) %>%
    dplyr::select(kp, iso3, area_id, logit_gen_prev_msm, logit_gen_prev_pwid, median, area_level) %>%
    # Calculate Log-Odds ratio
    tidyr::pivot_wider(names_from = kp,
                       names_glue = "{.value}_{kp}",
                       values_from = c("median")) %>%
    dplyr::mutate(msm_lor = median_MSM - logit_gen_prev_msm,
                  pwid_lor = median_PWID - logit_gen_prev_pwid) %>%
    dplyr::select(-c("logit_gen_prev_pwid","logit_gen_prev_msm","median_PWID","median_MSM","area_level"))

  # Match KP estimates (admin0 or admin1) with SAE estimates
  msm_analysis_level <- paste0("area_id",unique(msm_est$area_level))
  areas_wide <-  naomi::spread_areas(areas) %>%
    sf::st_drop_geometry()
  map <- dplyr::select(areas_wide, area_id, dplyr::all_of(msm_analysis_level)) %>%
    dplyr::rename(kp_match_area = 2)

  #' Modelled estimates of proportion in each risk group
  risk_group_prop <- male_srb %>%
    dplyr::left_join(map, by = dplyr::join_by(area_id)) %>%
    dplyr::filter(year == survey_year_sample, iso3 == options$area_scope) %>%
    dplyr::select(area_id,kp_match_area, age_group, indicator, estimate_smoothed) %>%
    tidyr::pivot_wider( names_from = indicator, values_from = estimate_smoothed,
                        values_fn = mean, names_prefix = "prop_") %>%
    # Add in Naomi indicators
    dplyr::right_join(naomi_est,  by = c("area_id", "age_group")) %>%
    dplyr::filter(!is.na(prop_nosex12m)) %>%
    dplyr::mutate(
      population_nosex12m = population * prop_nosex12m,
      population_sexcohab = population * prop_sexcohab,
      population_sexnonreg = population * prop_sexnonreg,
      population_msm = population * prop_msm,
      population_pwid = population * prop_pwid
    )

  #' Calculate prevalence in each category
  calculate_prevalence <- function(x, iso3){

    #' Log odds ratio from SRB group survey prevalence
    lor <- naomi.resources:::load_agyw_exdata("srb_survey_lor", iso3) %>%
      dplyr::filter(sex == "male")

    lor_15to29 <- lor$lor_15to29
    names(lor_15to29) <- lor$srb_group

    lor_30to49 <- lor$lor_30to49
    names(lor_30to49) <- lor$srb_group

    if (x$age_group[1] %in% c("Y015_019","Y020_024","Y025_029")) {
      lor <- lor_15to29
    } else {
      lor <- lor_30to49
    }

    population_fine <- dplyr::filter(x, indicator == "population")$estimate
    plhiv <- x$plhiv[1]
    kp_lor <- c("msm_lor" = x$msm_lor[1],"pwid_lor"= x$pwid_lor[1])
    lor <- c(lor[1:3], kp_lor)
    prev <- logit_scale_prev(lor, population_fine, plhiv)
    y <- dplyr::filter(x, indicator == "prop") %>%
      dplyr::mutate(indicator = "prev",estimate = prev)
    dplyr::bind_rows(x, y)
  }


  #' Calculate logit prevalence and format
  logit_prev <- risk_group_prop %>%
    dplyr::left_join(kp_prev, by = c("kp_match_area" = "area_id")) %>%
    tidyr::pivot_longer(
      cols = starts_with(c("population_", "prop_")),
      names_to = "indicator",
      values_to = "estimate") %>%
    tidyr::separate(indicator, into = c("indicator", "behav")) %>%
    dplyr::filter(behav %in% c("nosex12m", "sexcohab", "sexnonreg", "msm", "pwid")) %>%
    split(~ area_id + age_group) %>%
    lapply(calculate_prevalence, iso3) %>%
    dplyr::bind_rows() %>%
    tidyr::unite("indicator", indicator, behav, sep = "_") %>%
    tidyr::pivot_wider( names_from = indicator, values_from = estimate) %>%
    dplyr::mutate_if(is.numeric, as.numeric) %>%
    dplyr::mutate_if(is.factor, as.character)

  logit_prev

}


#' Calculate the odds
#'
#' @param p Probability in [0, 1]
odds <- function(p) p / (1 - p)

#' Calculate YWKP prevalence ratio and log odds ratio
#'
#' @param prev (General population) prevalence
#' @param fit A model relating log-odds prevalence to YWKP log odds prevalence
calculate_ywkp_pr_lor <- function(prev, fit = ywkp_fit) {
  prev_logodds <- qlogis(prev)
  prev_ywkp_logodds <- predict(fit, data.frame(prev_logodds = prev_logodds))
  #' Ensure that the LOR is above that of e.g. the sexnonreg risk group
  prev_ywkp_logodds <- pmax(prev_ywkp_logodds, prev_logodds + 0.25)
  prev_ywkp <- plogis(prev_ywkp_logodds)
  #' Prevalence ratio
  pr <- prev_ywkp / prev
  #' Log-odds ratio
  lor <- prev_ywkp_logodds - prev_logodds
  return(list(pr = pr, lor = lor, prev = prev, prev_ywkp = prev_ywkp))
}

#' Calculate prevalence and PLHIV using logit-scale disaggregation
#'
#' @param lor Log odds-ratios
#' @param N_fine Number of individuals in each group
#' @param plhiv Total number of people living with HIV
logit_scale_prev <- function(lor, N_fine, plhiv) {
  #' theta represents prevalence in baseline risk group
  #' plogis(lor + theta) is prevalence in each risk group
  #' plogis(lor + theta) * N_fine is PLHIV in each risk group
  optfn <- function(theta) (sum(plogis(lor + theta) * N_fine) - plhiv)^2
  #' Optimisation for baseline risk group prevalence
  #' On the logit scale should be more numerically stable
  opt <- optimise(optfn, c(-10, 10), tol = .Machine$double.eps^0.5)
  #' Return prevalence
  plogis(lor + opt$minimum)

}


#' Calculate incidence for female SRB groups.
#'
#' @param outputs Naomi output.
#' @param options Naomi options extracted from outputs
#' @param female_srb Estimates of female sexual risk groups generated by `agyw_adjust_sexbehav_fsw()`
#' @param female_logit_prevalence Risk adjusted estimates of female prevalence in sexual risk groups generated by `agyw_calculate_prevalence_female()`
#' @param survey_year Survey year to sample from the SAE model. Default is 2018.
#'
#'
#' @return Wide format output required for the AGYW workbook.
#'
#' While maintaining age/sex/district-specific HIV incidence from Naomi, distribute
#' HIV incidence between our 4 different behavioural groups utilizing IRRs from the
#' literature
#'

agyw_calculate_incidence_female <- function(naomi_output,
                                            options,
                                            female_srb,
                                            female_logit_prevalence,
                                            survey_year = 2018) {

  naomi_indicators <- naomi_output  %>%
    dplyr::filter(indicator %in% c("population", "plhiv","prevalence","infections", "incidence"),
                  sex == "female", area_level == options$area_level) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = mean) %>%
    dplyr::mutate(
      incidence_cat = cut(incidence,
                          c(0, 0.3, 1, 3, 10^6),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE, right = TRUE))

  risk_group_prevalence <- female_logit_prevalence %>%
    dplyr::select(area_id, age_group, starts_with("prev_"))

  df <- female_srb %>%
    dplyr::filter(year == survey_year) %>%
    dplyr::select(area_id, age_group, indicator, estimate_smoothed) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = estimate_smoothed, values_fn = mean) %>%
    dplyr::left_join(naomi_indicators, by = dplyr::join_by(area_id, age_group)) %>%
    dplyr::left_join(risk_group_prevalence, by = dplyr::join_by(area_id, age_group)) %>%
    dplyr::filter(!is.na(population))


  #' Risk ratios for people non-regular sex partners relative to those with a
  #' single cohabiting sex partner
  #' ALPHA Network pooled analysis (Slaymaker et al CROI 2020), Jia et al systematic review, Ssempijja et al JAIDS 2022
  rr_sexcohab <- 1
  rr_sexnonreg_young <- 1.72
  rr_sexnonreg_old <- 2.1

  #' Tiered HIV risk ratio for the FSW group depending on district-level HIV
  #' incidence in general population
  #' Jones et al medRxiv "HIV incidence among women engaging in sex work in sub-Saharan Africa: a systematic review and meta-analysis"
  #' https://www.medrxiv.org/content/10.1101/2023.10.17.23297108v2
  #' linear relationship between log(FSW incidence) and log(gen pop incidence)
  #' regression points shared in confidence, y = mx + b slope is 0.604104017 and
  #' intercept is 0.075090952

  rr_reg_dat <- data.frame(genpop_incidence = df$incidence/100) %>%
    dplyr::mutate(log_gen = log(genpop_incidence),
                  log_sexpaid12m = 0.604104017 * log_gen + 0.075090952,
                  sexpaid12m_incidence = exp(log_sexpaid12m),
                  rr_sexpaid12m = sexpaid12m_incidence / genpop_incidence)

  rr_sexpaid12m <- rr_reg_dat$rr_sexpaid12m
  # This gives implausibly high RRs for very low districts (e.g. IRR = 297!)
  # capping at 100
  rr_sexpaid12m[rr_sexpaid12m > 100] <- 100

  #' TODO: Get distributions on these and using a sampling method to get
  #' uncertainty in economic analysis e.g.
  rr_sexnonreg_se <- 0.2
  rr_sexnonreg_se <- 1


  #' Calculate risk group incidence
  Y015_024 <-  c("Y015_019", "Y020_024")
  Y025_049 <-  c("Y025_029","Y030_034","Y035_039","Y040_044", "Y045_049")

  df1 <- df %>%
    dplyr::mutate(
      rr_sexpaid12m = rr_sexpaid12m,
      rr_sexnonreg = dplyr::case_when(
        age_group %in% Y015_024 ~ rr_sexnonreg_young,
        age_group %in% Y025_049 ~ rr_sexnonreg_old,
        TRUE ~ NA_real_),
      population_nosex12m = population * nosex12m,
      population_sexcohab = population * sexcohab,
      population_sexnonreg = population * sexnonreg,
      population_sexpaid12m = population * sexpaid12m,
      plhiv_nosex12m = population_nosex12m * prev_nosex12m,
      plhiv_sexcohab = population_sexcohab * prev_sexcohab,
      plhiv_sexnonreg = population_sexnonreg * prev_sexnonreg,
      plhiv_sexpaid12m = population_sexpaid12m * prev_sexpaid12m,
      susceptible_nosex12m = population_nosex12m - plhiv_nosex12m,
      susceptible_sexcohab = population_sexcohab - plhiv_sexcohab,
      susceptible_sexnonreg = population_sexnonreg - plhiv_sexnonreg,
      susceptible_sexpaid12m = population_sexpaid12m - plhiv_sexpaid12m,
      incidence_sexpaid12m = (incidence/100) * rr_sexpaid12m,
      infections_sexpaid12m = susceptible_sexpaid12m * incidence_sexpaid12m,
      incidence_nosex12m = 0,
      incidence_sexcohab = (infections - infections_sexpaid12m) / (susceptible_sexcohab + rr_sexnonreg * susceptible_sexnonreg),
      incidence_sexnonreg = incidence_sexcohab * rr_sexnonreg,
      infections_nosex12m = 0,
      infections_sexcohab = susceptible_sexcohab * incidence_sexcohab,
      infections_sexnonreg = susceptible_sexnonreg * incidence_sexnonreg)

  #' Calculate risk group incidence for aggregate age groups

  summarise_age_cat_female <- function(dat, age_cat) {

    if (age_cat == "Y015_024") {age_groups <- c("Y015_019", "Y020_024")}
    if (age_cat == "Y025_049") {age_groups <- c("Y025_029","Y030_034","Y035_039",
                                                "Y040_044", "Y045_049")}
    if (age_cat == "Y015_049") {age_groups <- c("Y015_019", "Y020_024","Y025_029",
                                                "Y030_034","Y035_039","Y040_044",
                                                "Y045_049")}


    x <- dat %>%
      dplyr::group_by(area_id, area_name, area_level, sex, calendar_quarter) %>%
      dplyr::summarise(
        "population" = sum(population * as.integer(age_group %in% age_groups)),
        "plhiv" = sum(plhiv * as.integer(age_group %in% age_groups)),
        "infections" = sum(infections * as.integer(age_group %in% age_groups)),
        "population_nosex12m" = sum(population_nosex12m * as.integer(age_group %in% age_groups)),
        "population_sexcohab" = sum(population_sexcohab * as.integer(age_group %in% age_groups)),
        "population_sexnonreg" = sum(population_sexnonreg * as.integer(age_group %in% age_groups)),
        "population_sexpaid12m" = sum(population_sexpaid12m * as.integer(age_group %in% age_groups)),
        "plhiv_nosex12m" = sum(plhiv_nosex12m * as.integer(age_group %in% age_groups)),
        "plhiv_sexnonreg" = sum(plhiv_sexnonreg * as.integer(age_group %in% age_groups)),
        "plhiv_sexpaid12m" = sum(plhiv_sexpaid12m * as.integer(age_group %in% age_groups)),
        "plhiv_sexcohab" = sum(plhiv_sexcohab * as.integer(age_group %in% age_groups)),
        "susceptible_nosex12m" = sum(susceptible_nosex12m * as.integer(age_group %in% age_groups)),
        "susceptible_sexcohab" = sum(susceptible_sexcohab * as.integer(age_group %in% age_groups)),
        "susceptible_sexnonreg" = sum(susceptible_sexnonreg * as.integer(age_group %in% age_groups)),
        "susceptible_sexpaid12m" = sum(susceptible_sexpaid12m * as.integer(age_group %in% age_groups)),
        "infections_nosex12m" = sum(infections_nosex12m * as.integer(age_group %in% age_groups)),
        "infections_sexcohab" = sum(infections_sexcohab * as.integer(age_group %in% age_groups)),
        "infections_sexnonreg" = sum(infections_sexnonreg * as.integer(age_group %in% age_groups)),
        "infections_sexpaid12m" = sum(infections_sexpaid12m * as.integer(age_group %in% age_groups)),
        "sexnonregplus" = sum(susceptible_sexnonreg, susceptible_sexpaid12m)/(population - plhiv),
        .groups = "drop") %>%
      dplyr::mutate(age_group = age_cat,
                    nosex12m = susceptible_nosex12m/(population - plhiv),
                    sexcohab = susceptible_sexcohab/(population - plhiv),
                    sexnonreg = susceptible_sexnonreg/(population - plhiv),
                    sexpaid12m = susceptible_sexpaid12m/(population - plhiv),
                    incidence = (infections/(population - plhiv))*100,
                    incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
                    incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
                    incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
                    incidence_sexpaid12m = infections_sexpaid12m/susceptible_sexpaid12m,
                    prev_nosex12m = plhiv_nosex12m/(susceptible_nosex12m + plhiv_nosex12m),
                    prev_sexcohab = plhiv_sexcohab/(susceptible_sexcohab + plhiv_sexcohab),
                    prev_sexnonreg = plhiv_sexnonreg/(susceptible_sexnonreg + plhiv_sexnonreg),
                    prev_sexpaid12m = plhiv_sexpaid12m/(susceptible_sexpaid12m + plhiv_sexpaid12m),
                    rr_sexpaid12m = NA)
  }

  # Aggregate data
  df2 <- dplyr::bind_rows(summarise_age_cat_female(df1, "Y015_024"),
                          summarise_age_cat_female(df1, "Y025_049"),
                          summarise_age_cat_female(df1, "Y015_049"))

  # Calculate incidence
  df3 <- dplyr::bind_rows(df1, df2) %>%
    dplyr::mutate(incidence_cat = cut(incidence,
                                      c(0, 0.3, 1, 3, 10^6),
                                      labels = c("Low", "Moderate", "High", "Very High"),
                                      include.lowest = TRUE, right = TRUE))

  #' Check that sum of disaggregated infections is the same as total infections
  sum_infections <- df3$infections_nosex12m + df3$infections_sexcohab + df3$infections_sexnonreg + df3$infections_sexpaid12m

  # TO DO: Flag this to add in warning - stop please contact support (usually an issue with mapping boundaries)
  # ADD IN WARNIING HERE
  stopifnot(max(df3$infections - sum_infections) < 10^{-9})


  df3 %>%
    dplyr::mutate(concat = paste0(area_id, age_group), iso3 = options$area_scope) %>%
    dplyr::select(area_id, age_group, concat,
                  nosex12m, sexcohab, sexnonregplus, sexnonreg, sexpaid12m,
                  iso3, area_level,
                  population, plhiv, infections, incidence, incidence_cat,
                  prev_nosex12m, prev_sexcohab, prev_sexnonreg, prev_sexpaid12m,
                  rr_sexpaid12m, rr_sexnonreg,
                  population_nosex12m, population_sexcohab,
                  population_sexnonreg, population_sexpaid12m,
                  plhiv_nosex12m, plhiv_sexcohab,
                  plhiv_sexnonreg, plhiv_sexpaid12m,
                  susceptible_nosex12m, susceptible_sexcohab,
                  susceptible_sexnonreg, susceptible_sexpaid12m,
                  incidence_nosex12m, incidence_sexcohab,
                  incidence_sexnonreg, incidence_sexpaid12m,
                  infections_nosex12m,infections_sexcohab,
                  infections_sexnonreg, infections_sexpaid12m,
                  incidence_cat) %>%
    dplyr::mutate_if(is.numeric, as.numeric) %>%
    dplyr::mutate_if(is.factor, as.character)

}

#' Calculate incidence in high risk male key populations
#'
#' @param outputs Naomi output.
#' @param options Naomi options extracted from outputs
#' @param male_srb Estimates of male sexual risk groups generated by `agyw_adjust_sexbehav_msm_pwid()`
#' @param male_logit_prevalence Risk adjusted estimates of male prevalence in sexual risk groups generated by `agyw_calculate_prevalence_male()`
#' @param survey_year NOTES:: add in description + when this should be adjusted. Hardcoded to 2018.
#'
#' @return Wide format output required for the AGYW workbook
#'
#' @export

agyw_calculate_incidence_male <- function(naomi_output,
                                          options,
                                          male_srb,
                                          male_logit_prevalence,
                                          survey_year = 2018) {


  naomi_indicators <- naomi_output  %>%
    dplyr::filter(indicator %in% c("population", "plhiv","prevalence","infections", "incidence"),
                  sex == "male", area_level == options$area_level) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = mean) %>%
    dplyr::mutate(
      incidence_cat = cut(incidence,
                          c(0, 0.3, 1, 3, 10^6),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE, right = TRUE))

  risk_group_prevalence <- male_logit_prevalence %>%
    dplyr::select(area_id, age_group, gen_prev, starts_with( "prev_")) %>%
    dplyr::mutate(msm_pr = prev_msm/ gen_prev,
                  pwid_pr = prev_pwid / gen_prev)

  df <- male_srb %>%
    dplyr::filter(year == survey_year) %>%
    dplyr::select(area_id, age_group, indicator, estimate_smoothed) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = estimate_smoothed, values_fn = mean) %>%
    dplyr::left_join(naomi_indicators, by = dplyr::join_by(area_id, age_group)) %>%
    dplyr::left_join(risk_group_prevalence, by = dplyr::join_by(area_id, age_group)) %>%
    dplyr::filter(!is.na(population))


  # ALPHA Network pooled analysis (Slaymaker et al CROI 2020), Hoffman et al JAIDS 2022, Ssempijja et al JAIDS 2022
  rr_sexcohab <- 1
  rr_sexnonreg_young <- 1.89
  rr_sexnonreg_old <- 2.1

  #' TODO: Get distributions on these and using a sampling method to get uncertainty in economic analysis e.g.
  rr_sexnonreg_se <- 0.2
  rr_sexnonreg_se <- 1


  #' Calculate risk group incidence
  Y015_024 <-  c("Y015_019", "Y020_024")
  Y025_049 <-  c("Y025_029","Y030_034","Y035_039","Y040_044", "Y045_049")

  df1 <- df %>%
    dplyr::mutate(
      msm_pr = round(prev_msm / prevalence, 2),
      pwid_pr = round(prev_pwid / prevalence, 2),
      # Setting artificial cutoff of IRR of 2.5 due to Stannah et al
      # Lancet HIV systematic review of MSM vs gen pop IRR
      # https://www.thelancet.com/journals/lanhiv/article/PIIS2352-3018(23)00111-X/fulltext
      rr_msm = dplyr::if_else(msm_pr > 2.5, msm_pr, 2.5),
      rr_pwid = dplyr::if_else(pwid_pr > 2.5, pwid_pr, 2.5),
      rr_sexnonreg = dplyr::case_when(
        age_group %in% Y015_024 ~ rr_sexnonreg_young,
        age_group %in% Y025_049 ~ rr_sexnonreg_old,
        TRUE ~ NA_real_),
      population_nosex12m = population * nosex12m,
      population_sexcohab = population * sexcohab,
      population_sexnonreg = population * sexnonreg,
      population_msm = population * msm,
      population_pwid = population * pwid,
      plhiv_nosex12m = population_nosex12m * prev_nosex12m,
      plhiv_sexcohab = population_sexcohab * prev_sexcohab,
      plhiv_sexnonreg = population_sexnonreg * prev_sexnonreg,
      plhiv_msm = population_msm * prev_msm,
      plhiv_pwid = population_pwid * prev_pwid,
      susceptible_nosex12m = population_nosex12m - plhiv_nosex12m,
      susceptible_sexcohab = population_sexcohab - plhiv_sexcohab,
      susceptible_sexnonreg = population_sexnonreg - plhiv_sexnonreg,
      susceptible_msm = population_msm - plhiv_msm,
      susceptible_pwid = population_pwid - plhiv_pwid,
      incidence_msm = (incidence/100) * rr_msm,
      incidence_pwid = (incidence/100) * rr_pwid,
      infections_msm = susceptible_msm * incidence_msm,
      infections_pwid = susceptible_pwid * incidence_pwid,
      incidence_nosex12m = 0,
      incidence_sexcohab = (infections - infections_msm - infections_pwid) / (susceptible_sexcohab +
                                           rr_sexnonreg * susceptible_sexnonreg),
      incidence_sexnonreg = incidence_sexcohab * rr_sexnonreg,
      infections_nosex12m = 0,
      infections_sexcohab = susceptible_sexcohab * incidence_sexcohab,
      infections_sexnonreg = susceptible_sexnonreg * incidence_sexnonreg

    )

  #' Calculate risk group incidence for aggregate age groups

  summarise_age_cat_male <- function(dat, age_cat) {

    if (age_cat == "Y015_024") {age_groups <- c("Y015_019", "Y020_024")}
    if (age_cat == "Y025_049") {age_groups <- c("Y025_029","Y030_034","Y035_039",
                                                "Y040_044", "Y045_049")}
    if (age_cat == "Y015_049") {age_groups <- c("Y015_019", "Y020_024","Y025_029",
                                                "Y030_034","Y035_039","Y040_044",
                                                "Y045_049")}

    dat %>%
      dplyr::group_by(area_id, area_name, area_level, sex, calendar_quarter) %>%
      dplyr::summarise(
        "population" = sum(population * as.integer(age_group %in% age_groups)),
        "plhiv" = sum(plhiv * as.integer(age_group %in% age_groups)),
        "infections" = sum(infections * as.integer(age_group %in% age_groups)),
        "population_nosex12m" = sum(population_nosex12m * as.integer(age_group %in% age_groups)),
        "population_sexcohab" = sum(population_sexcohab * as.integer(age_group %in% age_groups)),
        "population_sexnonreg" = sum(population_sexnonreg * as.integer(age_group %in% age_groups)),
        "population_msm" = sum(population_pwid * as.integer(age_group %in% age_groups)),
        "population_pwid" = sum(population_pwid * as.integer(age_group %in% age_groups)),
        "plhiv_nosex12m" = sum(plhiv_nosex12m * as.integer(age_group %in% age_groups)),
        "plhiv_sexnonreg" = sum(plhiv_sexnonreg * as.integer(age_group %in% age_groups)),
        "plhiv_sexcohab" = sum(plhiv_sexcohab * as.integer(age_group %in% age_groups)),
        "plhiv_msm" = sum(plhiv_msm * as.integer(age_group %in% age_groups)),
        "plhiv_pwid" = sum(plhiv_pwid * as.integer(age_group %in% age_groups)),
        "susceptible_nosex12m" = sum(susceptible_nosex12m * as.integer(age_group %in% age_groups)),
        "susceptible_sexcohab" = sum(susceptible_sexcohab * as.integer(age_group %in% age_groups)),
        "susceptible_sexnonreg" = sum(susceptible_sexnonreg * as.integer(age_group %in% age_groups)),
        "susceptible_msm" = sum(susceptible_msm * as.integer(age_group %in% age_groups)),
        "susceptible_pwid" = sum(susceptible_pwid * as.integer(age_group %in% age_groups)),
        "infections_nosex12m" = sum(infections_nosex12m * as.integer(age_group %in% age_groups)),
        "infections_sexcohab" = sum(infections_sexcohab * as.integer(age_group %in% age_groups)),
        "infections_sexnonreg" = sum(infections_sexnonreg * as.integer(age_group %in% age_groups)),
        "infections_msm" = sum(infections_msm * as.integer(age_group %in% age_groups)),
        "infections_pwid" = sum(infections_pwid * as.integer(age_group %in% age_groups)),
        "sexnonregplus" = sum(susceptible_sexnonreg)/(population - plhiv),
        .groups = "drop") %>%
      dplyr::mutate(age_group = age_cat,
                    nosex12m = susceptible_nosex12m/(population - plhiv),
                    sexcohab = susceptible_sexcohab/(population - plhiv),
                    sexnonreg = susceptible_sexnonreg/(population - plhiv),
                    msm = susceptible_msm/(population - plhiv),
                    pwid = susceptible_pwid/(population - plhiv),
                    incidence = (infections/(population - plhiv)) * 100,
                    incidence_nosex12m = infections_nosex12m/susceptible_nosex12m,
                    incidence_sexcohab = infections_sexcohab/susceptible_sexcohab,
                    incidence_sexnonreg = infections_sexnonreg/susceptible_sexnonreg,
                    incidence_msm = infections_msm/susceptible_msm,
                    incidence_pwid = infections_pwid/susceptible_pwid,
                    prev_nosex12m = plhiv_nosex12m/(susceptible_nosex12m + plhiv_nosex12m),
                    prev_sexcohab = plhiv_sexcohab/(susceptible_sexcohab + plhiv_sexcohab),
                    prev_sexnonreg = plhiv_sexnonreg/(susceptible_sexnonreg + plhiv_sexnonreg),
                    prev_msm = plhiv_msm/(susceptible_msm + plhiv_msm),
                    prev_pwid = plhiv_pwid/(susceptible_pwid + plhiv_pwid),
                    rr_msm = NA,
                    rr_pwid = NA)
  }

  # Aggregate data
  df2 <- dplyr::bind_rows(summarise_age_cat_male(df1, "Y015_024"),
                          summarise_age_cat_male(df1, "Y025_049"),
                          summarise_age_cat_male(df1, "Y015_049"))

  # Calculate incidence
  df3 <- dplyr::bind_rows(df1, df2) %>%
    dplyr::mutate(incidence_cat = cut(incidence,
                                      c(0, 0.3, 1, 3, 10^6),
                                      labels = c("Low", "Moderate", "High", "Very High"),
                                      include.lowest = TRUE, right = TRUE))



  #' Check that sum of disaggregated infections is the same as total infections
  #  TO DO: add warning for sum not matching - contact admin
  sum_infections <- df3$infections_nosex12m + df3$infections_sexcohab + df3$infections_sexnonreg + df3$infections_msm + df3$infections_pwid

  stopifnot(max(df3$infections - sum_infections) < 10^{-9})


  df3 %>%
    dplyr::mutate(concat = paste0(area_id, age_group), iso3 = options$area_scope) %>%
    dplyr::select(area_id, age_group, concat,
                  nosex12m, sexcohab, sexnonregplus, sexnonreg, msm, pwid,
                  iso3, area_level,
                  population, plhiv, infections, incidence, incidence_cat,
                  prev_nosex12m, prev_sexcohab,
                  prev_sexnonreg, prev_msm, prev_pwid,
                  rr_msm, rr_pwid, rr_sexnonreg,
                  population_nosex12m, population_sexcohab,
                  population_sexnonreg, population_msm, population_pwid,
                  plhiv_nosex12m, plhiv_sexcohab,
                  plhiv_sexnonreg, plhiv_msm, plhiv_pwid,
                  susceptible_nosex12m, susceptible_sexcohab,
                  susceptible_sexnonreg, susceptible_msm, susceptible_pwid,
                  incidence_nosex12m, incidence_sexcohab,
                  incidence_sexnonreg, incidence_msm, incidence_pwid,
                  infections_nosex12m,infections_sexcohab,
                  infections_sexnonreg, infections_msm, infections_pwid) %>%
    dplyr::mutate_if(is.numeric, as.numeric) %>%
    dplyr::mutate_if(is.factor, as.character)



}


#' Calculate incidence in high risk male key populations
#'
#' @param outputs Naomi output.
#' @param options Naomi options extracted from outputs
#' @param male_srb Estimates of male sexual risk groups generated by `agyw_adjust_sexbehav_msm_pwid()`
#' @param male_logit_prevalence Risk adjusted estimates of male prevalence in sexual risk groups generated by `agyw_calculate_prevalence_male()`
#' @param survey_year NOTES:: add in description + when this should be adjusted. Hardcoded to 2018.
#'
#' @return Wide format output required for the AGYW workbook
#'
#' @export
#'
#' Survey year should be updated to most current household survey in the country -
#' for countries without recent household surveys, leave at 2018 - the spatiotemporal
#' model of sexual behaviour fitted to all countries has the most data for in roughly 2018

agyw_generate_risk_populations <- function(naomi_output,
                                           pjnz,
                                           survey_year = 2018) {

  # Read in naomi outputs
  if (tolower(tools::file_ext(naomi_output)) %in% c("rds", "qs")) {
    # Read files if hintr rds provided
    model_object <- read_hintr_output(naomi_output)
    outputs <- model_object$output_package
    options <- outputs$fit$model_options

  } else if (grepl("\\.zip$", naomi_output)) {
    # Read files if output zip is provided
    output_zip <- naomi_output
    outputs <- naomi::read_output_package(output_zip)
    options <- outputs$fit$model_options
  }

  #' Format naomi output
  naomi <- agyw_format_naomi(outputs, options)

  #' Naomi population
  naomi_pop <- naomi$naomi_long %>%
    dplyr::filter(indicator == "population") %>%
    dplyr::select(area_id, area_level,sex, age_group, area_level,
                  spectrum_region_code, population = mean)

  naomi_pop$iso3 <- options$area_scope

  #' Disaggregate KP PSEs from Oli's analysis to 5-year bands
  kp_consensus <- extract_kp_workbook(pjnz)
  fsw_est <- agyw_disaggregate_fsw(outputs, options, naomi_pop, kp_consensus)
  pwid_est <- agyw_disaggregate_pwid(outputs, options, naomi_pop, kp_consensus)
  msm_est <- agyw_disaggregate_msm(outputs, options, naomi_pop, kp_consensus)

  #' Adjust SAE model output with KP proportions
  female_srb <- agyw_adjust_sexbehav_fsw(outputs, options, fsw_est)
  male_srb <- agyw_adjust_sexbehav_msm_pwid(outputs, options, msm_est, pwid_est)

  #' Calculate risk group prevalence
  female_logit_prevalence <- agyw_calculate_prevalence_female(naomi$naomi_long,
                                                              options,
                                                              fsw_est,
                                                              female_srb,
                                                              survey_year)

  male_logit_prevalence <- agyw_calculate_prevalence_male(naomi$naomi_long,
                                                          outputs$meta_area,
                                                          options,
                                                          msm_est,
                                                          male_srb,
                                                          survey_year)

  #' Calculate risk group incidence
  female_incidence <- agyw_calculate_incidence_female(naomi$naomi_long,
                                                      options,
                                                      female_srb,
                                                      female_logit_prevalence,
                                                      survey_year)

  male_incidence <- agyw_calculate_incidence_male(naomi$naomi_long,
                                                  options,
                                                  male_srb,
                                                  male_logit_prevalence,
                                                  survey_year)

  meta <- data.frame(kp = c("FSW", "MSM", "PWID"),
                     consensus_estimate = c(unique(fsw_est$consensus_estimate),
                                            unique(msm_est$consensus_estimate),
                                            unique(pwid_est$consensus_estimate)))



  v <- list(female_incidence = female_incidence,
            male_incidence = male_incidence,
            naomi_output = naomi$naomi_wide,
            meta_consensus = meta)

  v

}

