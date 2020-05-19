library(survey)
library(tidyverse)
library(sf)
library(rdhs)
library(here)

devtools::load_all()

#' 1. DHS datasets
#'    * Available from: https://dhsprogram.com/data/available-datasets.cfm
#'    * Accessed: 30 August 2019
#'    * Accessed via the package rdhs: https://CRAN.R-project.org/package=rdhs
#' 2. MPHIA 2015-16
#'    * Available from: https://phia-data.icap.columbia.edu/files#malawi
#'    * Accessed: 1 March 2019
#'    * Note: cluster coordinates not yet available. IMPUTED for demonstration.
#' 


#' ## Load datasets

data(mwi_area_hierarchy)
data(mwi_area_boundaries)

areas_wide <- spread_areas(mwi_area_hierarchy)

data(mwi_population_agesex)


#' ## DHS

surveys <- dhs_surveys(countryIds = "MW") %>%
  mutate(iso3 = "MWI",
         survey_id = sub("^MW", "MWI", SurveyId))

#' DHS survey boundaries dataset
#' * DHS survey boundary shapefiles: https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=MW

paths <- paste0("~/Data/shape files/DHS/", surveys$SurveyId, ".zip") %>%
  setNames(surveys$survey_id)

geom_raw <- Map(download_boundaries, surveyId = surveys$SurveyId, method = "sf") %>%
  unlist(recursive = FALSE)

geom <- geom_raw %>%
  lapply(select, DHSCC, SVYID, REG_ID, MULTLEVEL, LEVELRNK, REGVAR, REGCODE, REGNAME, OTHREGVAR, OTHREGCO, OTHREGNA) %>%
  do.call(rbind, .) %>%
  mutate_at(vars(-geometry), ~replace(., . == "NULL", NA)) %>%
  left_join(surveys %>% select(iso3, survey_id, SurveyNum),
            by = c("SVYID" = "SurveyNum"))

#' Surveys with multiple areas: 
#' * In general, choose the level that larger number of regions.
#' * Note: this code will fail if two levels have same number of regions as n = max(n)
#'   will return both.

geom %>%
  filter(MULTLEVEL == "yes") %>%
  as.data.frame %>%
  count(survey_id, MULTLEVEL, LEVELRNK) %>%
  spread(LEVELRNK, n)

geom <- geom %>%
  inner_join(
    geom %>%
    as.data.frame %>%
    count(survey_id, MULTLEVEL, LEVELRNK) %>%
    group_by(survey_id) %>%
    filter(n == max(n)) %>%
    select(-n)
  )


#' Assign every area to a survey region
#' 1. Cross surveys list with areas_wide to expand areas list for each survey.
#' 2. Join each area to single survey region based on largest area overlap.

dhs_areas_wide <- surveys %>%
  select(iso3, survey_id) %>%
  inner_join(areas_wide %>% mutate(iso3 = "MWI")) %>%
  left_join(mwi_area_boundaries) %>%
  st_as_sf %>%
  rmapshaper::ms_simplify(1.0) %>%
  split(.$survey_id, drop = TRUE)

geom_spl <- geom %>%
  select(survey_id, REGVAR, REGCODE, REGNAME) %>%
  split(.$survey_id, drop = TRUE) %>%
  lapply(select, -survey_id)

areas_survey_region <- Map(st_join,
                           dhs_areas_wide,
                           geom_spl[names(dhs_areas_wide)],
                           largest = TRUE) %>%
  lapply(as_tibble) %>%
  bind_rows %>%
  select(-geometry)


#' Check for spatial join errors
#' 1. Cases where area level 1 is split over multiple DHS regions
#' 2. Merge master DHS region list back to identify DHS regions that did not have any
#'    areas assigned (indicating likely misassignment elsewhere.

check  <- areas_survey_region %>%
  full_join(
    geom %>%
    as.data.frame %>%
    select(iso3, survey_id, REGVAR, REGCODE, REGNAME) %>%
    filter(iso3 %in% areas_wide$iso3)
  )

check %>% filter(is.na(area_id0))


#' MW2015DHS HR dataset has four city REGCODES that do not appear in boundaries:
#' * 107: mzuzu city     [MWI_4_4]
#' * 210: lilongwe city  [MWI_4_14]
#' * 314: zomba city     [MWI_4_21]
#' * 315: blantyre city  [MWI_4_29]

areas_survey_region <- areas_survey_region %>%
  mutate(REGCODE = case_when(
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_4" ~ 107,
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_14" ~ 210,
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_21" ~ 314,
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_29" ~ 315,
           TRUE ~ REGCODE),
         REGNAME = case_when(
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_4" ~ "Mzuzu City",
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_14" ~ "Lilongwe City",
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_21" ~ "Zomba City",
           survey_id == "MWI2015DHS" & area_id4 == "MWI_4_29" ~ "Blantyre City",
           TRUE ~ as.character(REGNAME)))


#' Calculate number of areas at each level within each survey region.
#' * This is used to identify the lowest area that contains each survey region.
#' * And also is an idication if lower level areas have been misplaced. This can
#'   be >1 if survey regions are coarser than level 1 areas (e.g. CIV),
#'   but in most cases it won't be unless an area has been assigned to incorrect
#'   survey region.

dhs_region_level <- areas_survey_region %>%
  group_by(iso3, survey_id, REGVAR, REGCODE, REGNAME) %>%
  summarise(
    n_level0 = n_distinct(area_id0, na.rm = TRUE),
    n_level1 = n_distinct(area_id1, na.rm = TRUE),
    n_level2 = n_distinct(area_id2, na.rm = TRUE),
    n_level3 = n_distinct(area_id3, na.rm = TRUE),
    n_level4 = n_distinct(area_id4, na.rm = TRUE)
   ) %>% 
  ungroup

dhs_region_level %>%
  filter(n_level1 > 1) %>%
  print(n = Inf)


#' ### Survey regions dataset

#' Lowest area containing each survey region

dhs_regions <- areas_survey_region %>%
  mutate(survey_region_id = REGCODE,
         survey_region_name = REGNAME) %>%
  group_by(iso3, survey_id, survey_region_id, survey_region_name) %>%
  mutate(
    survey_region_area_id = case_when(
      n_distinct(area_id4, na.rm = TRUE) == 1 ~ area_id4,
      n_distinct(area_id3, na.rm = TRUE) == 1 ~ area_id3,
      n_distinct(area_id2, na.rm = TRUE) == 1 ~ area_id2,
      n_distinct(area_id1, na.rm = TRUE) == 1 ~ area_id1,
      n_distinct(area_id0, na.rm = TRUE) == 1 ~ area_id0
    )
  ) %>%
  distinct(iso3, survey_id, survey_region_id, survey_region_name, survey_region_area_id)

dhs_regions %>%
  count(iso3, survey_id, survey_region_id) %>%
  filter(n > 1)


#' ## DHS clusters dataset

#' Extract clusters from household recode dataset

hrd <- dhs_datasets(surveyIds = surveys$SurveyId, 
                    fileType = "HR", fileFormat = "flat") %>%
  mutate(path = get_datasets(.) %>% unlist) %>%
  left_join(surveys %>% select(SurveyId, survey_id)) %>%
  left_join(
    geom %>%
    as.data.frame %>%
    distinct(survey_id, REGVAR) %>%
    mutate(REGVAR = sub("^v024", "hv024", REGVAR))
  )


extract_clusters <- function(path, survey_id, REGVAR){

  print(survey_id)

  hr <- readRDS(path)
  val <- hr %>%
    transmute(survey_id,
              cluster_id = hv001,
              survey_region_id = .data[[REGVAR]],
              restype = factor(hv025, 1:2, c("urban", "rural"))) %>%
    distinct

  val
}

hrclust <- Map(extract_clusters,
               hrd$path,
               hrd$survey_id,
               hrd$REGVAR) %>%
  bind_rows()

#' Check that all region IDs appear in survey_regions dataset
hrclust %>%
  left_join(dhs_regions) %>%
  filter(is.na(iso3))


#' Add geo-coordinates

ged <- dhs_datasets(surveyIds = surveys$SurveyId,
                    fileType = "GE", fileFormat = "flat") %>%
  left_join(surveys %>% select(iso3, survey_id, SurveyId)) %>%
  mutate(path = get_datasets(.))

ge <- lapply(ged$path, readRDS) %>%
  lapply(as_tibble) %>%
  Map(f = mutate,
      iso3 = ged$iso3,
      survey_id = ged$survey_id,
      SurveyYear = ged$SurveyYear,
      SurveyType = ged$SurveyType,
      CountryName = ged$CountryName) %>%
  Map(replace, ., lapply(., `==`, "NULL"), NA) %>%
  lapply(type.convert) %>%
  bind_rows() %>%
  st_as_sf(coords = c("LONGNUM", "LATNUM"), remove = FALSE)

#' Clusters missing coordinates
ge %>% filter(LONGNUM == 0) %>% count(survey_id)

clusters <- hrclust %>%
  left_join(
    ge %>% filter(LONGNUM != 0) %>%
    select(survey_id,
           cluster_id = DHSCLUST,
           longitude = LONGNUM,
           latitude = LATNUM)
  )

#' ## Snap coordinates to areas
#'
#' areas_svy_region is a list of candidate location areas
#' for each cluster. Join candidate areas and then select
#' the nearest area based on distance. Usually the coordinate
#' should be contained (distance = 0)

clusters <- clusters %>%
  mutate(regcode_match = if_else(is.na(longitude), NA_integer_, survey_region_id)) %>%
  left_join(
    areas_survey_region %>%
    select(survey_id, regcode_match = REGCODE, area_id) %>%
    left_join(mwi_area_boundaries) %>%
    rename(geometry_area = geometry)
  ) %>%
  mutate(distance = Map(st_distance, geometry, geometry_area) %>% unlist)

#' Check clusters contained in >1 area
clusters %>%
  filter(distance == 0) %>%
  group_by(survey_id, cluster_id) %>%
  filter(n() > 1)

clusters <- clusters %>%
  arrange(distance) %>%
  group_by(survey_id, cluster_id) %>%
  filter(row_number() == 1) %>%
  ungroup %>% 
  transmute(survey_id,
            cluster_id = cluster_id,
            restype,
            survey_region_id, 
            longitude,
            latitude,
            geoloc_area_id = area_id,
            geoloc_distance = distance)

#' Clusters outside admin area
clusters %>%
  filter(geoloc_distance > 0) %>%
  arrange(-geoloc_distance) %>%
  print(n = Inf)


#' # Individual dataset
#'
#' Scheme:
#' * survey_id
#' * cluster_id
#' * household
#' * line
#' * sex
#' * age
#' * dob_cmc
#' * intv_cmc
#' * hivstatus
#' * arv
#' * artself
#' * vls
#' * cd4
#' * artall
#' * hivweight

#' Extract HIV datasets


ird <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "IR", fileFormat = "flat") %>%
  left_join(surveys %>% select(iso3, survey_id, SurveyId)) %>%
  mutate(path = get_datasets(.) %>% unlist)
mrd <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "MR", fileFormat = "flat") %>%
  left_join(surveys %>% select(iso3, survey_id, SurveyId)) %>%
  mutate(path = get_datasets(.) %>% unlist)
ard <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "AR", fileFormat = "flat") %>%
  left_join(surveys %>% select(iso3, survey_id, SurveyId)) %>%
  mutate(path = get_datasets(.) %>% unlist)
brd <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "BR", fileFormat = "flat") %>%
  left_join(surveys %>% select(iso3, survey_id, SurveyId)) %>%
  mutate(path = get_datasets(.) %>% unlist)

extract_dhs <- function(SurveyId){

  print(SurveyId)

  ir <- ird %>% filter(SurveyId == !!SurveyId) %>% .$path %>% readRDS

  if(!exists("aidsex", ir)){
    ir$aidsex <- haven::labelled(2, c("men" = 1, "women" = 2), "Sex")
  }

  dat <- ir %>%
    transmute(cluster_id = v001,
              household = v002,
              line = v003,
              interview_cmc = v008,
              sex = factor(aidsex, 1:2, c("male", "female")),
              age = v012,
              dob_cmc = v011,
              indweight = v005 / 1e6)

  #' Male recode
  if(SurveyId %in% mrd$SurveyId){

    mr <- mrd %>% filter(SurveyId == !!SurveyId) %>% .$path %>% readRDS
    mr$aidsex <- haven::labelled(1, c("men" = 1, "women" = 2), "Sex")

    dat <- dat %>%
      bind_rows(
        mr %>%
        transmute(cluster_id = mv001,
                  household = mv002,
                  line = mv003,
                  interview_cmc = mv008,
                  sex = factor(aidsex, 1:2, c("male", "female")),
                  age = mv012,
                  dob_cmc = mv011,
                  indweight = mv005 / 1e5)
      )

  }

  if(SurveyId %in% ard$SurveyId) {
    
    ar <- ard %>% filter(SurveyId == !!SurveyId) %>% .$path %>% readRDS
    
    dat <- dat %>%
    left_join(
      ar %>%
      transmute(
        cluster_id = hivclust,
        household = hivnumb,
        line = hivline,
        hivweight = hiv05 / 1e6,
        hivstatus = case_when(hiv03 == 0 ~ 0,
                              hiv03 %in% 1:3 ~ 1)
      ),
      by = c("cluster_id", "household", "line")
    )
  }
  
  dat$SurveyId <- SurveyId

  dat
}

individual <- lapply(surveys$SurveyId, extract_dhs) %>%
  bind_rows()

#' Survey metadata
dhs_meta <- surveys %>%
  transmute(iso3,
            survey_id,
            country = CountryName,
            survey_type = SurveyType,
            survey_year = SurveyYear,
            fieldwork_start = FieldworkStart,
            fieldwork_end = FieldworkEnd,
            survey_mid_calendar_quarter = get_mid_calendar_quarter(as.Date(FieldworkStart)+15, as.Date(FieldworkEnd)+15),
            female_age_min = as.integer(MinAgeWomen),
            female_age_max = as.integer(MaxAgeWomen),
            male_age_min = as.integer(MinAgeMen),
            male_age_max = as.integer(MaxAgeMen),
            report_ref = NA_character_,
            dataset_ref = NA_character_,
            notes = NA_character_) 

dhs_regions <- dhs_regions %>%
  ungroup()

dhs_clusters <- clusters %>%
  ungroup()

dhs_individuals <- individual %>%
  left_join(surveys %>% select(survey_id, SurveyId)) %>%
  select(survey_id, cluster_id, household, line, interview_cmc, sex, age, dob_cmc, indweight)

dhs_biomarker <- individual %>%
  filter(!is.na(hivstatus)) %>%
  left_join(surveys %>% select(survey_id, SurveyId)) %>%
  select(survey_id, cluster_id, household, line, hivweight, hivstatus)

#' ## MPHIA 2015-16

#' ### Load datasets

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
  mutate(survey_id = "MWI2016PHIA",
         cluster_id = paste(varstrat, varunit))

chphia <- chbio %>%
  left_join(chind %>% select(personid, zone, urban, surveystdt, intwt0)) %>%
  mutate(survey_id = "MWI2016PHIA",
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

mwi_area_survey_region <- areas_wide %>%
  mutate(
    survey_id = "MWI2016PHIA",
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

mwi_area_survey_region %>% filter(is.na(survey_region_id))

#' Lowest area containing each survey region

phia_regions <- mwi_area_survey_region %>%
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
  ungroup


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
           restype = factor(urban, 1:2, c("urban", "rural")),
           survey_region_id = as.integer(zone))



#' Aggregate area_ids and population size by survey regions

area_sample <- mwi_population_agesex %>%
  filter(source == "Census 2018") %>%
  interpolate_population_agesex(calendar_quarters = "CY2016Q1") %>%
  inner_join(
    get_age_groups() %>%
    filter(age_group_start >= 15,
           age_group_start + age_group_span < 65)
  ) %>%
  left_join(mwi_area_survey_region %>%
            select(area_id, survey_id, survey_region_id)) %>%
  count(area_id, survey_id, survey_region_id, wt = population, name = "pop15to64") %>%
  group_by(survey_id, survey_region_id) %>%
  summarise(area_ids = list(area_id),
            area_pops = list(pop15to64))
  
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
  left_join(mwi_area_hierarchy %>% select(area_id, area_name),
            by = c("geoloc_area_id" = "area_id"))


#' ### Individuals dataset

phia_individuals <-
  bind_rows(
    phia %>%
    transmute(
      survey_id,
      cluster_id,
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
      cluster_id,
      household = householdid,
      line = personid,
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
      cluster_id,
      household = householdid,
      line = personid,
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
  mutate(iso3 = substr(survey_id, 1, 3),
         country = "Malawi",
         survey_type = "PHIA",
         survey_mid_calendar_quarter = recode(iso3, "MWI" = "CY2016Q1"),
         fieldwork_start = recode(iso3, "MWI" = "2015-11-01"),
         fieldwork_end   = recode(iso3, "MWI" = "2016-08-01")) %>%
    ungroup()


#' ## Combine DHS and PHIA dataset

survey_meta <- bind_rows(dhs_meta, phia_meta)
survey_regions <- bind_rows(dhs_regions, phia_regions)
survey_clusters <- bind_rows(
  dhs_clusters %>% mutate(cluster_id = as.character(cluster_id)),
  phia_clusters
)
survey_individuals <- bind_rows(
  dhs_individuals %>% mutate_at(vars(cluster_id, household, line), as.character),
  phia_individuals)
survey_biomarker <- bind_rows(
  dhs_biomarker %>% mutate_at(vars(cluster_id, household, line), as.character),
  phia_biomarker)


#' ## Calculate survey indicators

survey_hiv_indicators <- calc_survey_hiv_indicators(
  survey_meta,
  survey_regions,
  survey_clusters,
  survey_individuals,
  survey_biomarker,
  mwi_area_hierarchy)
  
#' ## Save datasets
#'
#' Survey data agreements stipulate not to reproduce individual survey data.
#' Consequently, save only empty data frames to show data structure.
#' Users may request data access and use this script to reproduce the datasets.

mwi_survey_meta <- survey_meta
mwi_survey_regions <- survey_regions
mwi_survey_clusters <- survey_clusters[NULL,]
mwi_survey_individuals <- survey_individuals[NULL,]
mwi_survey_biomarker <- survey_biomarker[NULL,]
mwi_survey_hiv_indicators <- survey_hiv_indicators

usethis::use_data(
           mwi_survey_meta,
           mwi_survey_regions,
           mwi_survey_clusters,
           mwi_survey_individuals,
           mwi_survey_biomarker,
           mwi_survey_hiv_indicators,
           overwrite = TRUE
         )



dir.create(here("inst/extdata/survey/"))
           
write_csv(mwi_survey_meta, here("inst/extdata/survey/survey_meta.csv"))
write_csv(mwi_survey_regions, here("inst/extdata/survey/survey_regions.csv"))
write_csv(mwi_survey_clusters, here("inst/extdata/survey/survey_clusters.csv"))
write_csv(mwi_survey_individuals, here("inst/extdata/survey/survey_individuals.csv"))
write_csv(mwi_survey_biomarker, here("inst/extdata/survey/survey_biomarker.csv"))
write_csv(mwi_survey_hiv_indicators, here("inst/extdata/survey/survey_hiv_indicators.csv"))
