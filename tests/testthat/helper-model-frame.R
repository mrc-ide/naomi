INLA:::inla.dynload.workaround()

data(mwi_population_agesex)

areas <- create_areas(mwi_area_levels, mwi_area_hierarchy, mwi_area_boundaries)
spec <- extract_pjnz_naomi(system.file("extdata/mwi2019.PJNZ", package = "naomi"))

naomi_mf <- naomi_model_frame(areas,
                              mwi_population_agesex,
                              spec,
                              scope = "MWI",
                              level = 4,
                              quarter_id1 = convert_quarter_id(1, 2016),
                              quarter_id2 = convert_quarter_id(3, 2018))


