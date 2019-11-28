library(tidyverse)
library(here)

devtools::load_all()

data(mwi_area_hierarchy)


#' ## Data sources
#'
#' Malawi 2018 Census population tables
#' Accessed 30 August 2019
#'
#' 
## dir.create(here("data-raw/population"))
## download.file("http://www.nsomalawi.mw/images/stories/data_on_line/demography/census_2018/2018%20MPHC%20Published%20Tables/Series%20A.%20Population%20Tables.xlsx",
##               here("data-raw/population", "Series A. Population Tables.xlsx"))

readxl::excel_sheets(here("data-raw/population", "Series A. Population Tables.xlsx"))


#' Table A3: Population by sex, district, residence
#' - Only read total (both urban/rural) population by sex.
#' - Possibly opportunity to refine with urban/rural data later

a3 <- readxl::read_excel(here("data-raw/population", "Series A. Population Tables.xlsx"),
                         sheet = "A3", skip = 2) %>%
  select(1, 3:4) %>%
  rename(name = 1, male = 2, female = 3) %>%
  filter(!is.na(name)) %>%
  mutate(
    area_level = case_when(name == "Malawi" ~ 0,
                           name %in% c("Northern", "Central", "Southern") ~ 1,
                           TRUE ~ 4),
    area_name = name
  ) %>%
  left_join(mwi_area_hierarchy %>% filter(area_level %in% c(0, 1, 4))) %>%
  gather(sex, pop_a3, male, female)

a3 %>% count(sex, area_level, wt = pop_a3)


#' Table A5: Population by sex, single-year age, and region
#' - Lowest geographic stratification for population by age and sex

a5 <- Map(readxl::read_excel,
          path = here("data-raw/population", "Series A. Population Tables.xlsx"),
          sheet = "A5",
          skip = 3 + 0:3 * 101,
          n_max = 98) %>%
  lapply(select, c(1, 3:4)) %>%
  lapply(rename, age = 1, male = 2, female = 3) %>%
  Map(mutate, ., area_name = list("Malawi", "Northern", "Central", "Southern")) %>%
  bind_rows %>%
  gather(sex, pop_a5, male, female) %>%
  left_join(mwi_area_hierarchy %>% filter(area_level %in% 0:1))

a5aggr <- a5 %>%
  filter(age != "Total") %>%
  mutate(age = sub("\\+", "", age) %>% type.convert,
         age_group = cut(age, c(0, 1, 1:18*5, Inf), c("Less than 1 Year", "1-4", paste0(1:17*5, "-", 2:18*5 - 1), "90+"), TRUE, FALSE)) %>%
  count(area_id, area_level, area_name, sex, age_group, wt = pop_a5, name = "pop_a5")


#' Table A6: Population by age and district (not sex)

a6 <- Map(readxl::read_excel,
          path = here("data-raw/population", "Series A. Population Tables.xlsx"),
          sheet = "A6",
          skip = c(2, 27, 51),
          n_max = 22) %>%
  lapply(rename, age_group = 1) %>%
  lapply(gather, name, pop_a6, -age_group) %>%
  bind_rows() %>%
  mutate(area_name = sub(" Total", "", name) %>%
           recode("Nklhotakota" = "Nkhotakota")) %>%
  left_join(mwi_area_hierarchy %>% filter(area_level %in% c(1, 4))) 


#' Initial population: district by age disaggregated by region sex ratio by age
cens18 <- spread_areas(mwi_area_hierarchy) %>%
  left_join(
    a6 %>%
    filter(area_level == 4, age_group != "Total") %>%
    select(area_id, age_group, pop_a6)
   ,
    by = c("area_id4" = "area_id")
  ) %>%
  left_join(
    a3 %>%
    filter(area_level == 4) %>%
    select(area_id, sex, pop_a3)
   ,
    by = c("area_id4" = "area_id")
  ) %>%
  left_join(
    a5aggr %>%
    filter(area_level == 1) %>%
    select(area_id, sex, age_group, pop_a5)
   ,
    by = c("area_id1" = "area_id", "sex", "age_group")
  ) %>%
  mutate(population = pop_a6)
  

#' Use inverse proportional fitting to adjust district population to match:
#' - Region population by age/sex (A5)
#' - District population by sex (A3)
#' - District population by age (A6)

for(i in 1:5) {
  cens18 <- cens18 %>%
    group_by(area_id1, sex, age_group) %>%
    mutate(ratio_a5 = pop_a5 / sum(population),
           population = population * ratio_a5) %>%
    group_by(area_id4, sex) %>%
    mutate(ratio_a3 = pop_a3 / sum(population),
           population = population * ratio_a3) %>%
    group_by(area_id4, age_group) %>%
    mutate(ratio_a6 = pop_a6 / sum(population),
           population = population * ratio_a6) %>%
    ungroup
}

cens18 %>%
  summarise(a3 = max(abs(log(ratio_a3))),
            a5 = max(abs(log(ratio_a5))),
            a6 = max(abs(log(ratio_a6))))

cens18 <- cens18 %>%
  select(area_id, sex, age_group, population)
            

#' Malawi NSO Population Projections 2008-2030
#' Source: http://www.nsomalawi.mw/images/stories/data_on_line/demography/census_2008/Main%20Report/ThematicReports/Population%20Projections%20Malawi.pdf
#' Accessed: 30 August 2019
#' Excel sheets transcription obtained from Malawi DHA

nso <- here::here("data-raw", "population", "Pop projections  2008-2030 Master file_transformed.xlsx") %>%
  readxl::read_excel("dataset") %>%
  gather(key, value, -year, -agegr) %>%
  rename(age_group_label = agegr) %>%
  mutate(area_name = sub("(.*) - (.*)", "\\1", key),
         sex = tolower(sub("(.*) - (.*)", "\\2", key))) %>%
  filter(area_name != "Total",
         age_group_label != "Total",
         sex != "total") %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
  left_join(
    mwi_area_hierarchy %>%
    filter(area_level == 4) %>%
    select(area_id, area_name)
  ) %>%
  count(area_id, year, sex, age_group, wt = value, name = "population")


count(nso, area_id) %>% print(n = Inf)
count(nso, sex)
count(nso, age_group)


#' ## Adjust projection to 2018 census population
#' - Log-linear interpoloation of adjustment ratio between 2008 to 2018
#' - Assume 2018 ratio for future years.
#' - Assume 1.0 ratio for before 2008

cens18adj <- nso %>%
  filter(year == 2018) %>%
  left_join(
      cens18 %>%
      mutate(age_group_label = age_group %>%           
               recode("Less than 1 Year" = "0-4",
                      "1-4" = "0-4",
                      "80-84" = "80+",
                      "85-89" = "80+",
                      "90+" = "80+"),
             age_group = NULL) %>%
      left_join(get_age_groups() %>% select(age_group_label, age_group)) %>%
      count(area_id,sex, age_group, wt = population, name = "cens18adj")
  ) %>%
  mutate(cens18adj = cens18adj / population,
         year = NULL,
         population = NULL)

#' Review the adjustments
#' - Urban population for adjult 15-64 MUCH smaller than projections, especially
#'   for men.
#' - Age 0-4 population much smaller: probably a combination of lower than
#'   projected fertility and undercount of U5 population.
cens18adj %>%  
  left_join(mwi_area_hierarchy %>% select(area_id, area_name, area_sort_order)) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label, age_group_sort_order)) %>%
  mutate(area = fct_reorder(area_name, area_sort_order),
         age_group = fct_reorder(age_group_label, age_group_sort_order)) %>%
  ggplot(aes(age_group, cens18adj, color = sex, group = sex)) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  geom_step() +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.6, 2.0)) + 
  facet_wrap(~area, ncol = 8) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(hjust = 1.0, angle = 90)) +
  ggtitle("Ratio of 2018 Census to 2008 population projections")

ggsave("~/Downloads/mwi_census2018_projections2008_comparison.pdf", h=8.5, w=16)


population_agesex <- nso %>%
  filter(year %in% 2008:2025) %>%
  left_join(cens18adj) %>%
  mutate(
    source = "Census 2018",
    population = population * exp(log(cens18adj) * pmax((year - 2008) / (2018 - 2008), 0.0)),
    cens18adj = NULL,
    calendar_quarter = convert_calendar_quarter(year, 2),
    year = NULL
  ) %>%
  select(area_id, source, calendar_quarter, sex, age_group, population)

#' ## Add ASFR column

asfr <- read_csv(here("data-raw/population/mwi-asfr.csv"))

population_agesex  <- population_agesex %>%
  left_join(
    mwi_area_hierarchy %>%
    select(area_id, area_name)
  ) %>%
  left_join(
    asfr %>%
    filter(area_level == 4) %>%
    mutate(area_name = recode(area_name, "Nkhatabay" = "Nkhata Bay"),
           sex = "female",
           asfr = median,
           median = NULL,
           lower = NULL,
           upper = NULL)
  ) %>%
  mutate(area_name = NULL,
         area_level = NULL)


#' ## Save datasets

mwi_population_agesex <- population_agesex

usethis::use_data(mwi_population_agesex, overwrite=TRUE)

dir.create(here("inst/extdata/population"))
write_csv(population_agesex, here("inst/extdata/population/population_agesex.csv"), na = "")
