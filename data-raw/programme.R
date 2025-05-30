
library(tidyverse)
library(here)

devtools::load_all()
data(demo_area_hierarchy)

#' ## Malawi HIV Programme data
#'
#' Data sources from Integrated Quarterly Reporting spreadsheets available via:
#' https://www.hiv.health.gov.mw/index.php/our-documents
#' 
#' Data are provided by the Ministry of Health, Malawi with the following disclaimer:
#'
#' 1. The data in this file are owned by the Ministry of Health (MOH), Malawi.
#'
#' 2. These data are shared in order to support the National HIV Program.
#'
#' 3. Data in this file may not be used by Third Parties for further analysis and
#'    dissemination without prior written approval of the Director of the Department
#'    for HIV and AIDS, MOH.
#'
#' 4. Patient-level data are routinely collected by health facility staff using
#'    standard monitoring tools. This file contains facility-level aggregates that
#'    have been generated by facility staff and that were verified from primary
#'    records during quarterly National HIV Program Supervision (coordinated of the
#'    Department of HIV and AIDS). While every effort is made to ensure high data
#'    quality, individual records may not be complete and accurate.
#'
#' 5. The official interpretation of these data is presented in Quarterly Integrated
#'    HIV Program Reports and any divergent interpretation may be misleading and is
#'    not supported by the MOH. Analysis and interpretation of the data requires
#'    detailed understsanding of the methods and constraints of the Malawi's National
#'    M&E System.
#'
#' Written approval for inclusion in naomi R package provided by Thoko Kalua via email
#' on 4 September 2019.
#'
#' Dataset updated through Q4 2023 on 16 November 2024.

demo_anc_testing <- read_csv(here("data-raw/programme/mwi_dha_ancrt.csv"))

demo_anc_testing <- demo_anc_testing %>%
  rename(area_name = district32) %>%
  filter(year <= 2023) %>%
  left_join(
    demo_area_hierarchy %>%
    filter(area_level == 4) %>%
    select(area_name, area_id),
    by = "area_name"
  ) %>%
  mutate(age_group = "Y015_049") %>%
  group_by(area_id, area_name, age_group, year) %>%
  summarise_at(vars(anc_clients, anc_known_pos, anc_already_art, anc_tested, anc_tested_pos, anc_known_neg, births_facility), sum) %>%
  ungroup()

demo_art_number <- read_csv(here("data-raw/programme/mwi_dha_arttot.csv"))

demo_art_number <- demo_art_number %>%
  rename(area_name = district32) %>%
  filter(quarter == 4) %>%
  left_join(
    demo_area_hierarchy %>% filter(area_level == 4) %>% select(area_name, area_id),
    by = "area_name"
  ) %>%
  mutate(quarter = NULL) %>%
  select(area_id, area_name, year, everything())

#' Approximate the number on ART age 15+ as 94% of all
#' Based on Spectrum file outputs, which were triangulated 

demo_art_number <- demo_art_number %>%
  crossing(age_group_label = c("0-14", "15+")) %>%
  mutate(art_prop = case_when(age_group_label == "0-14" ~ 0.06,
                              age_group_label == "15+" ~ 0.94),
         art_current = round(art_tot * art_prop),
         art_new = round(art_new * art_prop),
         art_prop = NULL,
         art_tot = NULL) %>%
  left_join(
    get_age_groups() %>%
    select(age_group_label, age_group),
    by = "age_group_label"
  ) %>%
  mutate(sex = "both",
         calendar_quarter = paste0("CY", year, "Q4"),
         age_group_label = NULL) %>%
  select(area_id, area_name, sex, age_group, year, calendar_quarter, art_current, art_new)


#' Add _simulated_ VLS data

vls <- read_csv(here("data-raw/programme/mwi-simulated-vls.csv"))

demo_art_number <- demo_art_number %>%
  left_join(
    vls %>%
    mutate(
      year = as.integer(substr(calendar_quarter, 3, 6))-1,
      quarter = substr(calendar_quarter, 8, 8),
      calendar_quarter = paste0("CY", year, "Q", quarter)
    ) %>%
    group_by(district, calendar_quarter, sex = "both", age_group) %>%
    summarise(across(c(vl_tested_12mos = vls_tested, vl_suppressed_12mos = vls_suppressed), sum)),
    by = c("area_name" = "district", "age_group", "sex", "calendar_quarter")
  )

usethis::use_data(
           demo_anc_testing,
           demo_art_number,
           overwrite = TRUE
         )

write_csv(demo_anc_testing, here("inst/extdata/demo_anc_testing.csv"), na = "")
write_csv(demo_art_number, here("inst/extdata/demo_art_number.csv"), na = "")
