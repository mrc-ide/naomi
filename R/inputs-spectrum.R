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
      dplyr::mutate(births = dplyr::if_else(is.na(asfr), 0, asfr * totpop))

    if(aggregate) {
      spec$spectrum_region_code <- 0L
    } else {
      spec$spectrum_region_code <- read_spectrum_region_code(pjnz)
    }

    spec
  }

  spec <- lapply(pjnz_list, extract_pjnz_one, aggregate) %>%
    dplyr::bind_rows() 

  spec
}

#' Read Subnational Region Code from Spectrum PJNZ
#'
#' @param pjnz file path to Spectrum PJNZ file.
#'
#' @return Spectrum subnational region code as an integer.
#'
#' @details
#' The region code is 0 if a national Spectrum file.
#'
#' @examples
#' pjnz <- system.file("extdata/mwi2019.PJNZ", package = "naomi")
#' read_spectrum_region_code(pjnz)
#' 
#' @export
read_spectrum_region_code <- function(pjnz) {
  pjn <- eppasm::read_pjn(pjnz)
  region_code <- pjn[which(pjn[, 1] == "<Projection Parameters - Subnational Region Name2>") + 3, 4]
  as.integer(region_code)
}


calc_spec_age_group_aggregate <- function(spec) {

  spec <- spec %>%
    dplyr::mutate(age_group_label = cut(age, c(0:16*5, Inf), c(paste0(0:15*5, "-", 0:15*5+4), "80+"), TRUE, FALSE),
                  age_group_label = as.character(age_group_label)) %>%
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

}




age_quarter_to_age_group <- function(age_quarter) {
  cut(age_quarter, breaks = c(0:16*5*4, Inf),
      labels = c(sprintf("%02d-%02d", 0:15*5, 0:15*5+4), "80+"),
      include.lowest = TRUE, right = FALSE)
}

age_quarter_to_age_group_id <- function(age_quarter) {
  age_group <- as.character(age_quarter_to_age_group(age_quarter))
  df <- dplyr::left_join(
                 dplyr::tibble(age_group),
                 dplyr::select(get_age_groups(), age_group, age_group_id),
                 by = "age_group"
               )
  df$age_group_id
}


create_Lproj <- function(spec, mf_model, quarter_id1, quarter_id2) {

  ## Graduate HIV population to quarter-year age
  ## For now, simply divide single-year age by 4
  ## 80+ age group not handled, but will be re-aggreated later
  hivpop <- spec %>%
    dplyr::mutate(quarter_id = convert_quarter_id(year, 2L),
           year = NULL) %>%
    dplyr::filter(dplyr::between(quarter_id, quarter_id1 - 4, quarter_id2 + 4)) %>%
    tidyr::crossing(age_quarter = 0:3) %>%
    dplyr::mutate(age_quarter = pmin(4 * age + age_quarter, max(age)*4),
           age = NULL,
           hivpop = hivpop / 4) %>%
    dplyr::count(spectrum_region_code, sex, age_quarter, quarter_id, wt = hivpop, name = "hivpop")


  ## Log-linear interpolate mid-year population to T1 and T2
  hivpop <- tidyr::expand(hivpop,
                          tidyr::nesting(spectrum_region_code, sex, age_quarter),
                          quarter_id = c(quarter_id1, quarter_id2)) %>%
    dplyr::full_join(hivpop, by = names(.)) %>%
    dplyr::group_by(spectrum_region_code, sex, age_quarter) %>%
    dplyr::mutate(hivpop = exp(zoo::na.approx(log(hivpop), quarter_id, na.rm = FALSE)),
                  hivpop = tidyr::replace_na(hivpop, 0)) %>%
    dplyr::filter(quarter_id %in% c(quarter_id1, quarter_id2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cohort_quarter = quarter_id - age_quarter,
           age_group_id1 = age_quarter_to_age_group_id(quarter_id1 - cohort_quarter),
           age_group_id2 = age_quarter_to_age_group_id(quarter_id2 - cohort_quarter))

  hivpop_t1 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(spectrum_region_code, sex, age_group_id1,
                 wt = hivpop, name = "hivpop1")

  hivpop_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group_id1, age_group_id2,
                 wt = hivpop, name = "hivpop2")

  hivpopLproj <- dplyr::inner_join(hivpop_t1, hivpop_t2,
                                   by = c("spectrum_region_code", "sex", "age_group_id1")) %>%
    dplyr::mutate(L = hivpop2 / hivpop1,
                  hivpop1 = NULL,
                  hivpop2 = NULL) %>%
    dplyr::left_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group_id1 = age_group_id, area_id, idx1 = idx),
             by = c("spectrum_region_code", "sex", "age_group_id1")
           ) %>%
    dplyr::left_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group_id2 = age_group_id, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex", "age_group_id2", "area_id")
           )

  Lproj <- Matrix::sparseMatrix(i = hivpopLproj$idx2,
                                j = hivpopLproj$idx1,
                                x = hivpopLproj$L,
                                dims = rep(nrow(mf_model), 2))

  Lproj
}
