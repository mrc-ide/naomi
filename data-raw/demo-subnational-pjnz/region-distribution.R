library(naomi)
library(tidyverse)
library(sf)

area_merged <- read_sf(system.file("extdata/demo_areas.geojson", package = "naomi"))

pop_agesex <- read_csv(system.file("extdata/demo_population_agesex.csv", package = "naomi"))
art_number <- read_csv(system.file("extdata/demo_art_number.csv", package = "naomi"))
anc_testing <- read_csv(system.file("extdata/demo_anc_testing.csv", package = "naomi"))


areas_wide <- spread_areas(area_merged)

df <- pop_agesex %>% left_join(st_drop_geometry(areas_wide))

pop_reg <- df %>%
  count(calendar_quarter, area_id1, area_name1, wt = population, name = "population") %>%
  group_by(calendar_quarter) %>%
  mutate(prop = population / sum(population)) %>%
  pivot_wider(id_cols = calendar_quarter, names_from = area_name1, values_from = prop)

art_number %>%
  left_join(st_drop_geometry(areas_wide)) %>%
  count(calendar_quarter, area_id1, area_name1, wt = art_current, name = "art_current") %>%
  group_by(calendar_quarter) %>%
  mutate(prop = art_current / sum(art_current)) %>%
  pivot_wider(id_cols = calendar_quarter, names_from = area_name1, values_from = prop)

anc_testing %>%
  left_join(st_drop_geometry(areas_wide)) %>%
  group_by(year, area_id1, area_name1) %>%
  summarise(across(starts_with("anc"), sum)) %>%
  group_by(year) %>%
  mutate(prop = anc_clients / sum(anc_clients)) %>%
  pivot_wider(id_cols = year, names_from = area_name1, values_from = prop)

anc_testing %>%
  left_join(st_drop_geometry(areas_wide)) %>%
  group_by(year, area_id1, area_name1) %>%
  summarise(across(starts_with("anc"), sum)) %>%
  group_by(year) %>%
  mutate(prop = anc_known_pos / sum(anc_known_pos)) %>%
  pivot_wider(id_cols = year, names_from = area_name1, values_from = prop)

anc_testing %>%
  left_join(st_drop_geometry(areas_wide)) %>%
  group_by(area_id1, area_name1) %>%
  summarise(across(starts_with("anc"), sum)) %>%
  ungroup() %>%
  mutate(prop = anc_tested / sum(anc_tested))

anc_testing %>%
  left_join(st_drop_geometry(areas_wide)) %>%
  group_by(area_id1, area_name1) %>%
  summarise(across(starts_with("anc"), sum)) %>%
  ungroup() %>%
  mutate(prop = (anc_tested_pos + anc_known_pos) / sum(anc_tested_pos + anc_known_pos))


file.copy("demo_mwi2019_region-pjnz.zip", "../../inst/extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz.zip")

#' Update areas for spectrum region code

reg_code <- areas_wide %>%
  st_drop_geometry() %>%
  mutate(
    spectrum_region_code = case_when(area_name1 == "Northern" ~ 10L,
                                     area_name1 == "Central" ~ 11L,
                                     area_name1 == "Southern" ~ 12L)
  ) %>%
  select(-area_id) %>%
  pivot_longer(c(area_id1, area_id2, area_id3, area_id4), values_to = "area_id") %>%
  distinct(area_id, spectrum_region_code)
  
areas_region <- area_merged %>%
  select(-spectrum_region_code) %>%
  left_join(reg_code, by = "area_id") %>%
  select(names(area_merged))
                                     
write_sf(areas_region, "../../inst/extdata/demo-subnational-pjnz/demo_areas_region-pjnz.geojson")


#' Create zone-level versions of ANC testing, ART, and population datasets
#'

anc_testing_zone <- anc_testing %>%
  inner_join(
    areas_wide %>%
    st_drop_geometry() %>%
    select(area_id, area_id2, area_name2),
    by = "area_id"
  ) %>%
  group_by(area_id = area_id2, area_name = area_name2, age_group, year) %>%
  summarise(across(c(starts_with("anc"), "births_facility"), sum), .groups = "drop")


art_number_zone <- art_number %>%
  inner_join(
    areas_wide %>%
    st_drop_geometry() %>%
    select(area_id, area_id2, area_name2),
    by = "area_id"
  ) %>%
  group_by(area_id = area_id2, area_name = area_name2, sex, age_group, year, calendar_quarter) %>%
  summarise(across(c(starts_with("art"), starts_with("vl")), sum), .groups = "drop")

population_zone <- pop_agesex %>%
  inner_join(
    areas_wide %>%
    st_drop_geometry() %>%
    select(area_id, area_id2, area_name2),
    by = "area_id"
  ) %>%
  group_by(area_id = area_id2, area_name = area_name2, source, calendar_quarter, sex, age_group) %>%
  summarise(
    asfr = weighted.mean(asfr, population),
    population = sum(population),
    .groups = "drop"
  ) %>%
  select(-asfr, everything(), asfr)

write_csv(anc_testing_zone, "../../inst/extdata/demo-subnational-pjnz/demo_anc_testing_zone.csv")
write_csv(art_number_zone, "../../inst/extdata/demo-subnational-pjnz/demo_art_number_zone.csv")
write_csv(population_zone, "../../inst/extdata/demo-subnational-pjnz/demo_population_zone.csv", na = "")
