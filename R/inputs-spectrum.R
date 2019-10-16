#' Extract Model Inputs from Spectrum PJNZ
#'
#' @param pjnz_list Vector of filepaths to Spectrum PJNZ file.
#' @param aggregate TRUE/FALSE whether to aggregate 
#'
#' @return A `data.frame` with Spectrum indicators.
#'
#' @examples
#' pjnz <- system.file("extdata/mwi2019.PJNZ", package = "naomi")
#' spec <- extract_pjnz_naomi(pjnz)
#'
#' @export
extract_pjnz_naomi <- function(pjnz_list, aggregate = TRUE) {

  extract_pjnz_one <- function(pjnz, aggregate) {
    
    totpop <- specio::read_total_pop(pjnz, TRUE) %>%
      dplyr::mutate(sex = as.character(sex))
    hivpop <- specio::read_hiv_pop(pjnz, TRUE) %>%
      dplyr::mutate(sex = as.character(sex))
    artpop <- specio::read_art_pop(pjnz, TRUE) %>%
      dplyr::mutate(sex = as.character(sex))

    demp <- eppasm::read_specdp_demog_param(pjnz)
    specres <- eppasm::read_hivproj_output(pjnz)
    
    infections <- specres$infections %>%
      as.data.frame.table(responseName = "infections",
                          stringsAsFactors = FALSE) %>%
      utils::type.convert(as.is = TRUE)

    asfr <- demp$asfr %>%
      as.data.frame.table(responseName = "asfr",
                        stringsAsFactors = FALSE) %>%
      utils::type.convert(as.is = TRUE)
    
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
                    age_group_label = as.character(age_group_label))


    if(aggregate) {
      spec$spectrum_region_code <- 0L
    } else {
      pjn <- eppasm::read_pjn(pjnz)
      region_code <- pjn[which(pjn[, 1] == "<Projection Parameters - Subnational Region Name2>") + 3, 4]
      spec$spectrum_region_code <- as.integer(region_code)
    }

    spec
  }

  spec <- lapply(pjnz_list, extract_pjnz_one, aggregate) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(spectrum_region_code, year, sex, age_group_label) %>%
    dplyr::summarise_at(
             dplyr::vars(totpop, hivpop, artpop, susc_previous_year, infections, births), sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prevalence = hivpop / totpop,
                  art_coverage = artpop / hivpop,
                  incidence = infections / susc_previous_year,
                  asfr = births / totpop) %>%
    dplyr::left_join(
             get_age_groups() %>% dplyr::select(age_group_id, age_group_label),
             by = "age_group_label"
           ) %>%
    dplyr::mutate(quarter_id = convert_quarter_id(year, 2L)) %>%
    dplyr::select(spectrum_region_code, year, quarter_id, dplyr::everything())

  spec
}
