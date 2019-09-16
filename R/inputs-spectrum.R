#' Extract Model Inputs from Spectrum PJNZ
#'
#' @param pjnz Filepath to Spectrum PJNZ file.
#'
#' @return A `data.frame` with Spectrum indicators.
#'
#' @examples
#' pjnz <- system.file("extdata/mwi2019.pjnz", package = "naomi")
#' spec <- extract_pjnz_naomi(pjnz)
#'
#' @export
extract_pjnz_naomi <- function(pjnz) {
  
  totpop <- specio::read_total_pop(pjnz, TRUE) %>%
    mutate(sex = as.character(sex))
  hivpop <- specio::read_hiv_pop(pjnz, TRUE) %>%
    mutate(sex = as.character(sex))
  artpop <- specio::read_art_pop(pjnz, TRUE) %>%
    mutate(sex = as.character(sex))
  
  demp <- eppasm::read_specdp_demog_param(pjnz)
  specres <- eppasm::read_hivproj_output(pjnz)
  
  infections <- specres$infections %>%
    as.data.frame.table(responseName = "infections",
                        stringsAsFactors = FALSE) %>%
    type.convert(as.is = TRUE)
  
  asfr <- demp$asfr %>%
    as.data.frame.table(responseName = "asfr",
                        stringsAsFactors = FALSE) %>%
    type.convert(as.is = TRUE)
  
  spec <- totpop %>%
    dplyr::left_join(hivpop, by = c("age", "sex", "year")) %>%
    dplyr::left_join(artpop, by = c("age", "sex", "year")) %>%
    dplyr::rename(
      totpop = total_pop,
      hivpop = hiv_pop,
      artpop = art_pop
    ) %>%
    dplyr::left_join(
             dplyr::mutate(., susc_previous_year = totpop - hivpop,
                           year = year + 1) %>%
             dplyr::select(age:year, susc_previous_year),
             by = c("age", "sex", "year")
           ) %>%
    dplyr::left_join(infections, by = c("age", "sex", "year")) %>%
    dplyr::left_join(asfr %>% dplyr::mutate(sex = "female"),
                     by = c("age", "sex", "year")) %>%
    dplyr::mutate(births = dplyr::if_else(is.na(asfr), 0, asfr * totpop)) %>%
    dplyr::mutate(age_group_label = cut(age, c(0:16*5, Inf), c(paste0(0:15*5, "-", 0:15*5+4), "80+"), TRUE, FALSE),
                  age_group_label = as.character(age_group_label)) %>%
    dplyr::group_by(year, sex, age_group_label) %>%
    dplyr::summarise_at(
             dplyr::vars(totpop, hivpop, artpop, susc_previous_year, infections, births), sum) %>%
    ungroup() %>%
    dplyr::mutate(prevalence = hivpop / totpop,
                  art_coverage = artpop / hivpop,
                  incidence = infections / susc_previous_year,
                  asfr = births / totpop) %>%
    dplyr::left_join(
             get_age_groups() %>% dplyr::select(age_group_id, age_group_label),
             by = "age_group_label"
           ) %>%
    dplyr::mutate(quarter_id = convert_quarter_id(2L, year)) %>%
    dplyr::select(year, quarter_id, everything())

  spec
}
