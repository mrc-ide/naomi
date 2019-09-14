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
         district32 = case_when(TYPE_2 == "City" ~ NAME_2,
                                NAME_1 %in% c("Blantyre", "Lilongwe", "Zomba", "Mzimba") ~ paste(NAME_1, "Rural"),
                                TRUE ~ NAME_1),
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

#' Rename area ID if area is the same at multiple levels

mwi_simple <- mwi_simple %>%
  mutate_if(is.factor, as.character) %>%
  mutate(id2 = if_else(name2 == name1, id1, id2),
         id3 = if_else(name3 == name2, id2, id3),
         id4 = if_else(name4 == name3, id3, id4))

mwi <- gather_areas(mwi_simple) %>%
  mutate(area_sort_order = row_number(),
         layer_sort_order = layer_id,
         parent_layer_id = if_else(layer_id == 0, NA_character_,
                                   paste0(iso3, "_", layer_id - 1)),
         layer_id = paste0(iso3, "_", layer_id))

area_hierarchy <- mwi %>%
  select(area_id, area_name, parent_area_id, geometry) %>% 
  group_by(area_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(center = st_point_on_surface(geometry),
         center_x = st_coordinates(center)[,1],
         center_y = st_coordinates(center)[,2],
         center = NULL)

area_layers <- mwi %>%
  as.data.frame %>%
  select(layer_id, area_id, area_sort_order)


area_layer_meta <- mwi %>%
  as.data.frame() %>%
  count(
    layer_id,
    parent_layer_id,
    layer_sort_order,
    name = "n_areas"
  ) %>%
  arrange(layer_id) %>%
  mutate(
    layer_label = layer_id %>%
      recode("MWI_0" = "Country",
             "MWI_1" = "Region",
             "MWI_2" = "Zone",
             "MWI_3" = "District",
             "MWI_4" = "District + Metro"),
    display = TRUE,
    spectrum_layer = if_else(layer_id == "MWI_0", TRUE, FALSE),
    epp_layer = if_else(layer_id == "MWI_1", TRUE, FALSE),
    epp_urban_rural = FALSE,
    naomi_layer = if_else(layer_id == "MWI_4", TRUE, FALSE),
    pepfar_psnu_layer = if_else(layer_id == "MWI_3", TRUE, FALSE)
  ) %>%
  select(
    layer_id,
    layer_label,
    n_areas,
    layer_sort_order,
    parent_layer_id,
    everything()
  )

#' Plot illustrating joining of area datasets

area_layer_meta %>%
  select(layer_id, layer_label, layer_sort_order) %>%
  left_join(area_layers) %>%
  left_join(area_hierarchy) %>%
  st_as_sf() %>%
  mutate(area_layer = layer_label %>% fct_reorder(layer_sort_order)) %>%
  ggplot() +
  geom_sf() +
  geom_label(aes(center_x, center_y, label = area_sort_order)) +
  facet_wrap(~area_layer, nrow = 1) +
  th_map()

#' ## Save datasets

mwi_area_layer_meta <- area_layer_meta
mwi_area_layers <- area_layers
mwi_area_hierarchy <- area_hierarchy

usethis::use_data(
           mwi_area_layer_meta,
           mwi_area_hierarchy,
           mwi_area_layers
         )

dir.create(here("inst/extdata/areas"))

write_csv(area_layer_meta, here("inst/extdata/areas/area_layer_meta.csv"), na = "")
write_csv(area_layers, here("inst/extdata/areas/area_layers.csv"), na = "")
st_write(area_hierarchy, here("inst/extdata/areas/area_hierarchy.geojson"), delete_dsn = TRUE)


#' ## Table schema

library(tableschema.r)

schema_area_meta <- system.file("extdata/areas/area_meta.csv", package = "naomi") %>%
  tableschema.r::infer()

schema_area_hierarchy <- system.file("extdata/areas/area_hierarchy.csv", package = "naomi") %>%
  tableschema.r::infer() %>%
  jsonlite::toJSON(pretty = TRUE)

schema_area_names <- system.file("extdata/areas/area_names.csv", package = "naomi") %>%
  tableschema.r::infer() %>%
  jsonlite::toJSON(pretty = TRUE)

schema_area_names <- system.file("extdata/areas/area_hierarchy.csv", package = "naomi") %>%
  tableschema.::infer() %>%
  jsonlite::toJSON(pretty = TRUE)

#' I cant figure out how to get tableschema.r to parse geojson...

schema_area_booundaries <-
  '{
    "fields": [
      {
        "name": ["iso3"],
        "type": ["string"],
        "format": ["default"]
      },
      {
        "name": ["area_id"],
        "type": ["string"],
      "format": ["default"]
    },
    {
      "name": ["geometry"],
      "type": ["geojson"],
      "format": ["default"]
    }
  ],
  "missingValues": [
    [""]
  ]
}
'

schema_area_centers <-
'{
    "fields": [
      {
        "name": ["iso3"],
        "type": ["string"],
        "format": ["default"]
      },
      {
        "name": ["area_id"],
        "type": ["string"],
      "format": ["default"]
    },
    {
      "name": ["geometry"],
      "type": ["geopoint"],
      "format": ["default"]
    }
  ],
  "missingValues": [
    [""]
  ]
}
'

#' Still working on this...
