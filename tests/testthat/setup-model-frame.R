INLA:::inla.dynload.workaround()

area_merged <- sf::read_sf(system.file("extdata/areas/area_merged.geojson", package = "naomi"))
spec <- extract_pjnz_naomi(system.file("extdata/mwi2019.PJNZ", package = "naomi"))


naomi_mf <- naomi_model_frame(area_merged,
                              mwi_population_agesex,
                              spec,
                              scope = "MWI",
                              level = 4,
                              calendar_quarter1 = "CY2016Q1",
                              calendar_quarter2 = "CY2018Q3")
