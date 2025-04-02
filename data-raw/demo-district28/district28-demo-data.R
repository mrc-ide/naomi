library(naomi)
library(tidyverse)
library(sf)

area_merged <- read_sf(system.file("extdata/demo_areas.geojson", package = "naomi"))

pop_agesex <- read_csv(system.file("extdata/demo_population_agesex.csv", package = "naomi"))
art_number <- read_csv(system.file("extdata/demo_art_number.csv", package = "naomi"))
anc_testing <- read_csv(system.file("extdata/demo_anc_testing.csv", package = "naomi"))
survey <- read_csv(system.file("extdata/demo_survey_hiv_indicators.csv", package = "naomi"))

areas_wide <- spread_areas(area_merged)


extdata_path <- "../../inst/extdata/demo-district28"
dir.create(extdata_path)

#' Update areas for spectrum region code
  
areas_district28 <- area_merged %>%
  filter(area_level %in% 0:3)
                                     
write_sf(areas_district28, file.path(extdata_path, "demo_areas_district28.geojson"))


#' Create zone-level versions of ANC testing, ART, and population datasets
#'

anc_testing_district28 <- anc_testing %>%
  inner_join(
    areas_wide %>%
    st_drop_geometry() %>%
    select(area_id, area_id3, area_name3),
    by = "area_id"
  ) %>%
  group_by(area_id = area_id3, area_name = area_name3, age_group, year) %>%
  summarise(across(c(starts_with("anc"), "births_facility"), sum), .groups = "drop")

art_number_district28 <- art_number %>%
  inner_join(
    areas_wide %>%
    st_drop_geometry() %>%
    select(area_id, area_id3, area_name3),
    by = "area_id"
  ) %>%
  group_by(area_id = area_id3, area_name = area_name3, sex, age_group, year, calendar_quarter) %>%
  summarise(across(c(starts_with("art"), starts_with("vl")), sum), .groups = "drop")

population_district28 <- pop_agesex %>%
  inner_join(
    areas_wide %>%
    st_drop_geometry() %>%
    select(area_id, area_id3, area_name3),
    by = "area_id"
  ) %>%
  group_by(area_id = area_id3, area_name = area_name3, source, calendar_quarter, sex, age_group) %>%
  summarise(
    asfr = weighted.mean(asfr, population),
    population = sum(population),
    .groups = "drop"
  ) %>%
  select(-asfr, everything(), asfr)


survey_district28 <- survey %>%
  semi_join(areas_district28, by = "area_id")

write_csv(anc_testing_district28, file.path(extdata_path, "demo_anc_testing_district28.csv"))
write_csv(art_number_district28, file.path(extdata_path, "demo_art_number_district28.csv"))
write_csv(population_district28, file.path(extdata_path, "demo_population_district28.csv"), na = "")
write_csv(survey_district28, file.path(extdata_path, "demo_survey_district28.csv"), na = "")

