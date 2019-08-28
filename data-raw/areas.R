#' Prepare areas dataset for Malawi
#'

library(tidyverse)
library(sf)
library(here)

devtools::load_all()

#' Construct 32 district shape file from GADM admin2 (https://gadm.org)
tmp <- tempfile()
download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_MWI_2_sf.rds", tmp)
sh2 <- readRDS(tmp)

#' ## Malawi (MWI)
#' Source: GADM Level 2 (https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_MWI_2_sf.rds)
#' Levels:
#'   * 1: Region
#'   * 2: Zone
#'   * 3: District
#'   * 4: District + 4 cities
#' Spectrum: National
#' EPP: Region (level 1)
#' EPP Urban/Rural: No
#' PEPFPAR PSNU: District (level 3)


mwi_wide <- sh2 %>%
  filter(TYPE_2 != "Water body" | NAME_1 == "Likoma") %>%
  mutate(district = NAME_1,
         district32 = if_else(TYPE_2 == "City", NAME_2, NAME_1),
         zone = district %>% fct_collapse(
           "Northern" = c("Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi"),
           "Central-East" = c("Dowa", "Kasungu", "Nkhotakota", "Ntchisi", "Salima"),
           "Central-West" = c("Dedza", "Lilongwe", "Mchinji", "Ntcheu"),
           "South-East" = c("Balaka", "Machinga", "Mangochi", "Mulanje", "Phalombe", "Zomba"),
           "South-West" = c("Blantyre", "Chikwawa", "Chiradzulu", "Mwanza", "Neno", "Nsanje", "Thyolo")
         ) %>%
           fct_relevel("Northern", "Central-East", "Central-West", "South-East", "South-West"),
         region = zone %>% fct_collapse("Central" = c("Central-East", "Central-West"), "Southern" = c("South-East", "South-West")) %>%
           fct_relevel("Northern", "Central", "Southern")
  ) %>%
  group_by(region, zone, district, district32) %>%
  summarise() %>%
  ungroup %>%
  arrange(region, zone, -map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  transmute(
    name0 = "Malawi",
    id0 = "MWI",
    name1 = region,
    id1 = paste0(id0, ".", as_factor(name1) %>% as.integer),
    name2 = zone,
    id2 = paste0(id1, ".", as_factor(name2) %>% as.integer),
    name3 = district,
    id3 = paste0(id2, ".", as_factor(name3) %>% as.integer),
    name4 = district32,
    id4 = paste0(id3, ".", as_factor(name4) %>% as.integer)
  )

mwi_simple <- mwi_wide %>% rmapshaper::ms_simplify(0.1)

check_boundaries(mwi_wide, mwi_simple)
pryr::object_size(mwi_wide %>% select)
pryr::object_size(mwi_simple %>% select)

mwi <- gather_areas(mwi_simple) %>%
  mutate(area_sort_order = row_number(),
         area_level = as.integer(area_level))

areas <- mwi %>% as.data.frame %>% select(-geometry)

area_boundaries <- mwi %>%
  select(iso3, area_id) %>%
  left_join(
    {.} %>% st_point_on_surface() %>%
      rename(center = geometry) %>%
      as.data.frame
  ) %>%
  mutate(center_adj = center)

area_geom <- area_boundaries %>%
  rename(boundary = geometry) %>%
  as_tibble() %>%
  gather(type, geometry, boundary, center, center_adj) %>%
  st_as_sf %>%
  `st_crs<-`(4326)

area_meta <- areas %>%
  count(iso3, area_level, name = "n_areas") %>%
  arrange(area_level) %>%
  mutate(area_level_label = area_level %>% recode(`0` = "Country", `1` = "Region", `2` = "Zone", `3` = "District", `4` = "District + Metro"),
         display = TRUE,
         spectrum_level = if_else(area_level == 0, TRUE, FALSE),
         epp_level = if_else(area_level == 1, TRUE, FALSE),
         epp_urban_rural = FALSE,
         pepfar_psnu_level = if_else(area_level == 2, TRUE, FALSE))

#' Plot illustrating joining of area datasets

areas %>%
  left_join(area_boundaries) %>%
  st_as_sf %>%
  left_join(area_meta %>% select(iso3, area_level, area_level_label)) %>%
  mutate(area_level_label = area_level_label %>% fct_reorder(area_level)) %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = area_sort_order), data = . %>% st_set_geometry("center_adj")) +
  facet_wrap(~area_level_label, nrow = 1) +
  th_map()

#' Using areas_geom

areas %>%
  left_join(area_geom) %>%
  st_as_sf %>%
  left_join(area_meta %>% select(iso3, area_level, area_level_label)) %>%
  mutate(area_level_label = area_level_label %>% fct_reorder(area_level)) %>%
  ggplot() +
  geom_sf(data = . %>% filter(type == "boundary")) +
  geom_sf_label(aes(label = area_sort_order), data = . %>% filter(type == "center_adj")) +
  facet_wrap(~area_level_label, nrow = 1) +
  th_map()


#' ## Save datasets

dir.create(here("inst/extdata/areas"))


saveRDS(areas, here("inst/extdata/areas/areas.rds"))
saveRDS(area_meta, here("inst/extdata/areas/area_meta.rds"))
saveRDS(area_boundaries, here("inst/extdata/areas/area_boundaries.rds"))
saveRDS(area_geom, here("inst/extdata/areas/area_geom.rds"))

write_csv(areas, here("inst/extdata/areas/areas.csv"))
write_csv(area_meta, here("inst/extdata/areas/area_meta.csv"))

st_write(area_boundaries, here("inst/extdata/areas/area_boundaries.geojson"))
st_write(area_boundaries %>% as_tibble %>% select(-geometry, -center), here("inst/extdata/areas/area_centers.geojson"))
st_write(area_geom, here("inst/extdata/areas/area_geom.geojson"))

#' Save as zipped ESRI shape


tmp <- file.path(tempdir(), "area_boundaries")
dir.create(tmp)
st_write(area_boundaries, file.path(tmp, "area_boundaries.shp"), delete_layer = TRUE)
withr::with_dir(tmp, zip(here("inst/extdata/areas/area_boundaries.zip"), list.files()))


#' Note: ESRI shp doesn't like mixed point/polygon long format
tmp <- file.path(tempdir(), "area_geom")
dir.create(tmp)
st_write(area_geom, file.path(tmp, "area_geom.shp"), delete_layer = TRUE)
withr::with_dir(tmp, zip(here("inst/extdata/areas/area_geom.zip"), list.files()))
