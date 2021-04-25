library(tidyverse)
library(sf)
library(naomi.utils)
library(here)

devtools::load_all()

#' 1. MDHS 2015-16 Final Report
#'    * Available from: https://dhsprogram.com/pubs/pdf/FR319/FR319.pdf
#'    * Accessed: 24 March 2021
#' 2. MPHIA 2015-16
#'    * Available from: https://phia-data.icap.columbia.edu/files#malawi
#'    * Accessed: 1 March 2019
#'    * Note: cluster coordinates not yet available. IMPUTED for demonstration.
#' 


#' ## Load datasets

data(demo_area_hierarchy)
data(demo_area_boundaries)

areas_wide <- spread_areas(demo_area_hierarchy)

data(demo_population_agesex)


#' ## DHS

set.seed(1234)

mdhs <- read_csv("mdhs15-district-prevalence.csv")

mdhs <- mdhs %>%
  filter(group %in% c("women 15-49", "men 15-54"))

#' Calculate number negative and number positive

mdhs <- mdhs %>%
  mutate(
    n_pos = round(est * n_unwgt / deft^2),
    n_neg = round((1 - est) * n_unwgt / deft^2)
  )

#' Expand districts

#' From Table B.1
metro <- tribble( ~district, ~district_new, ~proportion,
                 "Lilongwe", "Lilongwe City", 0.359,
                 "Lilongwe", "Lilongwe", 0.641,
                 "Blantyre", "Blantyre City", 0.655,
                 "Blantyre", "Blantyre", 0.345,
                 "Zomba", "Zomba City", 0.118,
                 "Zomba", "Zomba", 0.882,
                 "Mzimba", "Mzuzu City", 0.118,
                 "Mzimba", "Mzimba", 0.882)

mdhsd <- mdhs %>%
  left_join(metro) %>%
  mutate(proportion = replace_na(proportion, 1)) %>%
  group_by(district, group) %>%
  mutate(
    n_pos = as.vector(rmultinom(1, n_pos, proportion)),
    n_neg = as.vector(rmultinom(1, n_neg, proportion)),
    district = if_else(is.na(district_new), district, district_new)
  )


#' From Table 14.3
ageprev <- tribble(   ~age, ~prev_f, ~n_f, ~prev_m, ~n_m, ~prev_b, ~n_b,
                   "15-19",   0.033, 1674,   0.010, 1731,   0.021, 3405,
                   "20-24",   0.064, 1639,   0.011, 1343,   0.040, 2982,
                   "25-19",   0.092, 1250,   0.064,  957,   0.080, 2207,
                   "30-34",   0.162, 1152,   0.092,  855,   0.132, 2006,
                   "35-39",   0.181,  889,   0.118,  831,   0.151, 1720,
                   "40-44",   0.198,  642,   0.140,  578,   0.171, 1221,
                   "45-49",   0.169,  489,   0.192,  425,   0.180,  914,
                   "50-54",   0.000,    0,   0.205,  322,      NA,   NA)

ageprevl <- ageprev %>%
  transmute(group = "women 15-49", age, prev = prev_f, n = n_f) %>%
  bind_rows(
    ageprev %>%
    transmute(group = "men 15-54", age, prev = prev_m, n = n_m)
  ) %>%
  group_by(group) %>%
  mutate(prop_pos = prop.table(n * prev),
         prop_neg = prop.table(n * (1 - prev)))

mdhsa <- mdhsd %>%
  left_join(
    select(ageprevl, group, age, prop_pos, prop_neg),
    by = "group"
  ) %>%
  group_by(district, group) %>%
  mutate(
    n_pos = as.vector(rmultinom(1, n_pos, prop_pos)),
    n_neg = as.vector(rmultinom(1, n_neg, prop_neg)),
    sex = recode(group, "men 15-54" = "male", "women 15-49" = "female"),
    age_group = age
  ) %>%
  ungroup()

  

stopifnot(sum(mdhsa$n_pos) == sum(mdhs$n_pos))
stopifnot(sum(mdhsa$n_neg) == sum(mdhs$n_neg))

mdhsa %>%
  group_by(sex, age_group) %>%
  summarise(across(c(n_pos, n_neg), sum)) %>%
  mutate(prev = n_pos / (n_neg + n_pos))

mdhs_dat <- mdhsa %>%
  select(district, sex, age_group, n_pos, n_neg) %>%
  bind_rows(
    {.} %>%
    filter(age_group != "50-54") %>%
    mutate(age_group = "15-49") %>%
    group_by(district, sex, age_group) %>%
    summarise(across(c(n_pos, n_neg), sum))
  ) %>%
  bind_rows(
    {.} %>%
    filter(age_group != "50-54") %>%
    mutate(age_group = "15-49", sex = "both") %>%
    group_by(district, sex, age_group) %>%
    summarise(across(c(n_pos, n_neg), sum))
  ) %>%
  mutate(
    area_name = recode(district, "Nkhatabay" = "Nkhata Bay", "Nkhota kota" = "Nkhotakota")
  ) %>% 
  left_join(
    demo_areas %>%
    filter(area_level == 4) %>%
    select(area_id, area_name),
    by = "area_name"
  )

mdhs_dat <- mdhs_dat %>%
  filter(!(sex %in% c("both", "female") & age_group == "50-54"),
         !(n_pos + n_neg == 0)) %>%
  transmute(
    indicator = "prevalence",
    survey_id = "DEMO2015DHSA",
    survey_mid_calendar_quarter = "CY2016Q1",
    area_id,
    area_name,
    res_type = "all",
    sex,
    age_group = sub("(..)-(..)", "Y0\\1_0\\2", age_group),
    n_clusters = NA,
    n_observations = n_pos + n_neg,
    n_eff_kish = n_observations,
    estimate = n_pos / n_observations,
    std_error = sqrt(estimate * (1-estimate) / n_observations),
    lest_se = std_error/(estimate * (1 - estimate)),
    lower = plogis(qlogis(estimate) - qnorm(0.975) * lest_se),
    upper = plogis(qlogis(estimate) + qnorm(0.975) * lest_se),
    lower = if_else(is.nan(lower), NA_real_, lower),
    upper = if_else(is.nan(upper), NA_real_, upper),
    lest_se = NULL
  )
    
#' ## MPHIA 2015-16
#'
#' ### Load datasets
#'
#' Datasets downloaded from https://phia-data.icap.columbia.edu/files#malawi
#' 

mphia_path <- "~/Data/household surveys/PHIA/datasets/Malawi/datasets/"

bio <- file.path(mphia_path, "02_MPHIA 2015-2016 Adult Biomarker Dataset (DTA).zip") %>%
  rdhs::read_zipdata(readfn = haven::read_dta)

ind <- file.path(mphia_path, "02_MPHIA 2015-2016 Adult Interview Dataset (DTA).zip") %>%
  rdhs::read_zipdata(readfn = haven::read_dta)

chbio <- file.path(mphia_path, "05_MPHIA 2015-2016 Child Biomarker Dataset (DTA).zip") %>%
  rdhs::read_zipdata(readfn = haven::read_dta)

chind <- file.path(mphia_path, "05_MPHIA 2015-2016 Child Interview Dataset (DTA).zip") %>%
  rdhs::read_zipdata(readfn = haven::read_dta)

phia <- bio %>%
  left_join(ind %>% select(personid, zone, urban, surveystdt, intwt0)) %>%
  mutate(survey_id = "DEMO2016PHIA",
         cluster_id = paste(varstrat, varunit))

chphia <- chbio %>%
  left_join(chind %>% select(personid, zone, urban, surveystdt, intwt0)) %>%
  mutate(survey_id = "DEMO2016PHIA",
         cluster_id = paste(varstrat, varunit))

mphia_zone_labels <- c("Northern" = 1L,
                       "Central-East" = 2L,
                       "Central-West" = 3L,
                       "Lilongwe City" = 4L,
                       "South-East" = 5L,
                       "South-West" = 6L,
                       "Blantyre City" = 7L)

#' ### Survey regions dataset
#'
#' Note: In MoH classifications, Mulanje is part of the South-East Zone.
#'       In MPHIA, it is allocated to South-West Zone.
#' 

demo_area_survey_region <- areas_wide %>%
  mutate(
    survey_id = "DEMO2016PHIA",
    survey_region_name = case_when(
      area_name4 %in% c("Lilongwe City", "Blantyre City") ~ area_name4,
      area_name3 == "Mulanje" ~ "South-West",
      TRUE ~ area_name2
    )
  ) %>%
  left_join(
    tibble(survey_region_id = mphia_zone_labels,
           survey_region_name = names(mphia_zone_labels)),
    by = "survey_region_name"
  )

demo_area_survey_region %>% filter(is.na(survey_region_id))

#' Lowest area containing each survey region

phia_regions <- demo_area_survey_region %>%
  group_by(survey_id, survey_region_id, survey_region_name) %>%
  mutate(
    survey_region_area_id = case_when(
      n_distinct(area_id4, na.rm = TRUE) == 1 ~ area_id4,
      n_distinct(area_id3, na.rm = TRUE) == 1 ~ area_id3,
      n_distinct(area_id2, na.rm = TRUE) == 1 ~ area_id2,
      n_distinct(area_id1, na.rm = TRUE) == 1 ~ area_id1,
      n_distinct(area_id0, na.rm = TRUE) == 1 ~ area_id0
    )
  ) %>%
  distinct(survey_id, survey_region_id, survey_region_name, survey_region_area_id) %>%
  ungroup()


#' ### Survey cluster dataset
#'
#' Note: cluster geolocations are not available yet. Allocate clusters to
#'       districts randomly proportional to district population size.
#' 


#' PHIA survey cluster ids

ge <- bind_rows(
  phia %>% select(survey_id, cluster_id, urban, zone),
  chphia %>% select(survey_id, cluster_id, urban, zone)
) %>%
  distinct(survey_id,
           cluster_id,
           res_type = factor(urban, 1:2, c("urban", "rural")),
           survey_region_id = as.integer(zone))



#' Aggregate area_ids and population size by survey regions

area_sample <- demo_population_agesex %>%
  filter(source == "Census 2018") %>%
  interpolate_population_agesex(calendar_quarters = "CY2016Q1") %>%
  inner_join(
    get_age_groups() %>%
    filter(age_group_start >= 15,
           age_group_start + age_group_span < 65)
  ) %>%
  left_join(demo_area_survey_region %>%
            select(area_id, survey_id, survey_region_id)) %>%
  count(area_id, survey_id, survey_region_id, wt = population, name = "pop15to64") %>%
  group_by(survey_id, survey_region_id) %>%
  summarise(area_ids = list(area_id),
            area_pops = list(pop15to64),
            .groups = "drop")
  
#' Sample area_id for each cluster proportional to population size
sample2 <- function(x, size, replace = FALSE, prob = NULL) {
  x[sample.int(length(x), size, replace, prob)]
}

set.seed(3261736)
phia_clusters <- ge %>%
  left_join(area_sample) %>%
  mutate(
    geoloc_area_id = Map(sample2, area_ids, 1, TRUE, area_pops) %>% unlist,
    area_ids = NULL,
    area_pops = NULL
  ) %>%
  ungroup
  


#' Check to confirm area_id is in correct zone
phia_clusters %>%
  left_join(
    areas_wide %>%
    select(area_name2, area_id),
    by = c("geoloc_area_id" = "area_id")
  ) %>%
  count(survey_region_id, area_name2)

#' Number of clusters by district
phia_clusters %>%
  count(survey_region_id, geoloc_area_id) %>%
  arrange(n) %>%
  as.data.frame %>%
  left_join(demo_area_hierarchy %>% select(area_id, area_name),
            by = c("geoloc_area_id" = "area_id"))


#' ### Individuals dataset

phia_individuals <-
  bind_rows(
    phia %>%
    transmute(
      survey_id,
      cluster_id,
      individual_id = personid,
      household = householdid,
      line = personid,
      interview_cmc = cmc_date(surveystdt),
      sex = factor(gender, 1:2, c("male", "female")),
      age,
      dob_cmc = NA,
      indweight = intwt0
    ),
    chphia %>%
    transmute(
      survey_id,
      cluster_id,
      individual_id = personid,
      household = householdid,
      line = personid,
      interview_cmc = cmc_date(surveystdt),
      sex = factor(gender, 1:2, c("male", "female")),
      age,
      dob_cmc = interview_cmc - agem,
      indweight = intwt0
    )
  ) %>%
  mutate(age = as.integer(age),
         indweight = indweight / mean(indweight, na.rm=TRUE))

phia_biomarker <-
  bind_rows(
    phia %>%
    filter(!is.na(hivstatusfinal)) %>%
    transmute(
      survey_id,
      individual_id = personid,
      hivweight = btwt0,
      hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                            hivstatusfinal == 2 ~ 0),
      arv = case_when(arvstatus == 1 ~ 1,
                      arvstatus == 2 ~ 0),
      artself = case_when(artselfreported == 1 ~ 1,
                          artselfreported == 2 ~ 0),
      vls = case_when(vls == 1 ~ 1,
                      vls == 2 ~ 0),
      cd4 = cd4count,
      recent = case_when(recentlagvlarv == 1 ~ 1,
                         recentlagvlarv == 2 ~ 0)
    )
   ,
    chphia %>%
    filter(!is.na(hivstatusfinal)) %>%
    transmute(
      survey_id,
      individual_id = personid,
      hivweight = btwt0,
      hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                            hivstatusfinal == 2 ~ 0),
      arv = case_when(arvstatus == 1 ~ 1,
                      arvstatus == 2 ~ 0),
      artself = case_when(pedartparentreported == 1 ~ 1,
                          pedartparentreported == 2 ~ 0),
      vls = case_when(vls == 1 ~ 1,
                      vls == 2 ~ 0),
      cd4 = cd4count,
      recent = case_when(recentlagvlarv == 1 ~ 1,
                         recentlagvlarv == 2 ~ 0)
    )
  ) %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))


phia_meta <- phia_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE)) %>%
  mutate(iso3 = "MWI",
         country = "Malawi",
         survey_type = "PHIA",
         survey_mid_calendar_quarter = recode(iso3, "MWI" = "CY2016Q1"),
         fieldwork_start = recode(iso3, "MWI" = "2015-11-01"),
         fieldwork_end   = recode(iso3, "MWI" = "2016-08-01")) %>%
    ungroup()


#' ## Calculate PHIA survey indicators

phia_hiv_indicators <- calc_survey_hiv_indicators(
  phia_meta,
  phia_regions,
  phia_clusters,
  phia_individuals,
  phia_biomarker,
  demo_area_hierarchy)

  
#' ## Save datasets
#'

demo_survey_hiv_indicators_alt <- bind_rows(mdhs_dat, phia_hiv_indicators)

write_csv(demo_survey_hiv_indicators_alt,
          here("inst/extdata/survey/demo_survey_hiv_indicators_alt.csv"))
