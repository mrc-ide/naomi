#' Extract Model Inputs from Spectrum PJNZ
#'
#' @param pjnz_list Vector of filepaths to Spectrum PJNZ file.
#'
#' @return A `data.frame` with Spectrum indicators.
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' spec <- extract_pjnz_naomi(pjnz)
#'
#' @export
extract_pjnz_naomi <- function(pjnz_list) {

  pjnz_list <- unroll_pjnz(pjnz_list)    
  
  spec <- lapply(pjnz_list, extract_pjnz_one) %>%
    dplyr::bind_rows() %>%
    dplyr::select(spectrum_region_code, dplyr::everything())
  
  spec
}

unroll_pjnz <-  function(pjnz_list) {

  ## If meets conditions, treat as zipped list of PJNZ.
  ## * Single file
  ## * Does not contain a .DP or .PJN file
  if(length(pjnz_list) == 1) {
    file_names <- utils::unzip(pjnz_list, list = TRUE)$Name
    exts <- tolower(tools::file_ext(file_names))
    is_pjnz <- any("dp" %in% exts) || any("pjn" %in% exts)

    if(!is_pjnz) {
      pjnzdir <- tempfile()
      utils::unzip(pjnz_list, exdir = pjnzdir)
      pjnz_list <- list.files(pjnzdir, full.names = TRUE)
    }
  }

  pjnz_list
}


extract_pjnz_one <- function(pjnz) {

  ## Code extracted from eppasm:::create_specfp()
  demp <- eppasm::read_specdp_demog_param(pjnz)
  projp <- eppasm::read_hivproj_param(pjnz)
  specres <- eppasm::read_hivproj_output(pjnz)

  specfp <- eppasm:::create_spectrum_fixpar(projp, demp)
  specfp$eppmod <- "directincid"
  specfp$incidinput <- eppasm::incid(specres)
  specfp$incidpopage <- 0L  ## age 15-49

  mod <- eppasm::simmod(specfp)

  
  totpop <- as.data.frame.table(specres$totpop, responseName = "totpop",
                                stringsAsFactors = FALSE)
  hivpop <- as.data.frame.table(specres$hivpop, responseName = "hivpop",
                                stringsAsFactors = FALSE)
  artpop <- as.data.frame.table(specres$artpop, responseName = "artpop",
                                stringsAsFactors = FALSE)
  infections <- as.data.frame.table(specres$infections, responseName = "infections",
                                    stringsAsFactors = FALSE)
  asfr <- as.data.frame.table(demp$asfr, responseName = "asfr",
                              stringsAsFactors = FALSE)
  asfr$sex <- "female"
  
  spec <- totpop %>%
    dplyr::left_join(hivpop, by = c("age", "sex", "year")) %>%
    dplyr::left_join(artpop, by = c("age", "sex", "year")) %>%
    dplyr::left_join(infections, by = c("age", "sex", "year")) %>%
    dplyr::left_join(asfr, by = c("age", "sex", "year")) %>%
    utils::type.convert(as.is = TRUE)

  pregprev <- extract_eppasm_pregprev(mod, specfp)
  spec <- dplyr::left_join(spec, pregprev, by = c("age", "sex", "year"))

  spec <- add_shiny90_unaware(spec, pjnz)

  spec$spectrum_region_code <- read_spectrum_region_code(pjnz)
  
  spec
}

add_shiny90_unaware <- function(spec, pjnz) {

  if (assert_pjnz_shiny90(pjnz)) {
    shiny90_dir <- tempfile()
    on.exit(unlink(shiny90_dir, recursive = TRUE))

    shiny90_name <- get_pjnz_shiny90_filename(pjnz)
    utils::unzip(pjnz, shiny90_name, exdir = shiny90_dir)
    
    df <- extract_shiny90_age_sex(file.path(shiny90_dir, shiny90_name),
                                  years = unique(spec$year))
    df$unaware_untreated_prop <- (df$plhiv - df$aware) / (df$plhiv - df$artnum)
    df <- df[c("year", "sex", "agegr", "unaware_untreated_prop")]

    ## NOTE: Shiny90 age groups hard coded here.
    ##   15-19, 20-24, ..., 45-49, 50-99
    age_cut <- c(seq(0, 50, 5), 100)
    age_lab <- paste0(age_cut[-length(age_cut)], "-", age_cut[-1] - 1)
    spec$agegr <- cut(spec$age, age_cut, age_lab, right = FALSE)

    spec <- dplyr::left_join(spec, df, by = c("year", "sex", "agegr"))

    ## For children, assume all untreated are unaware. This is the current
    ## assumption in Spectrum.
    spec$unaware_untreated_prop[is.na(spec$unaware_untreated_prop)] <- 1.0

    spec$unaware <- (spec$hivpop - spec$artpop) * spec$unaware_untreated_prop
    spec$agegr <- NULL
    spec$unaware_untreated_prop <- NULL

  } else {

    ## If no .shiny90 estimates available, assume all untreated PLHIV are
    ## unaware
    spec$unaware <- spec$hivpop - spec$artpop
  }

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
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' read_spectrum_region_code(pjnz)
#' 
#' @export
read_spectrum_region_code <- function(pjnz) {
  pjn <- eppasm::read_pjn(pjnz)
  region_code <- pjn[which(pjn[, 1] == "<Projection Parameters - Subnational Region Name2>") + 3, 4]
  as.integer(region_code)
}

#' Read Spectrum Projection Name from Spectrum PJNZ
#'
#' @param pjnz file path to Spectrum PJNZ file.
#'
#' @return Spectrum projection name as character string.
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' read_spectrum_projection_name(pjnz)
#' 
#' @export
read_spectrum_projection_name <- function(pjnz) {
  pjn <- eppasm::read_pjn(pjnz)
  projname <- pjn[which(pjn[, 1] == "<Projection Name>") + 2, 4]
  trimws(projname)
}

get_pjnz_shiny90_filename <- function(pjnz) {
  files <- utils::unzip(pjnz, list = TRUE)$Name
  grep("\\.shiny90$", files, ignore.case = TRUE, value = TRUE)
}

#' Check whether PJNZ contains .shiny90 file
#'
#' @param pjnz file path to PJNZ
#'
#' @return Logical whether PJNZ file contains a .shiny90 file
#'
assert_pjnz_shiny90 <- function(pjnz) {
  as.logical(length(get_pjnz_shiny90_filename(pjnz)))
}

#' Cut Five Year Age Groups
#'
#' Wrapper for `[cut()]` to return five year age groups with 
#'
#' @param age a vector of ages.
#'
#' @return a vector of strings with five year age groups.
#'
#' @seealso
#' get_age_groups
#' 
#' @export
cut_naomi_age_group <- function(age) {
  labs <- c(sprintf("Y%03.0f_%03.0f", 0:15*5, 0:15*5 + 4), "Y080_999")
  age_group <- cut(x = age, breaks = c(0:16*5, Inf), labels = labs,
                   include.lowest = TRUE, right = FALSE)
  as.character(age_group)
}
  
age_quarter_to_age_group <- function(age_quarter) {
  f <- cut(age_quarter, breaks = c(0:16*5*4, Inf),
           labels = c(sprintf("Y%03d_%03d", 0:15*5, 0:15*5+4), "Y080_999"),
           include.lowest = TRUE, right = FALSE)
  as.character(f)
}

create_Lproj <- function(spec, mf_model, quarter_id1, quarter_id2, quarter_id3) {

  ## Graduate HIV population to quarter-year age
  ## For now, simply divide single-year age by 4
  ## 80+ age group not handled, but will be re-aggreated later
  spec_quarter <- spec %>%
    dplyr::mutate(quarter_id = convert_quarter_id(year, 2L),
                  year = NULL) %>%
    dplyr::filter(dplyr::between(quarter_id, quarter_id1 - 4, quarter_id3 + 8)) %>%
    tidyr::crossing(age_quarter = 0:3) %>%
    dplyr::mutate(age_quarter = pmin(4 * age + age_quarter, max(age)*4),
                  age = NULL,
                  totpop = totpop / 4,
                  hivpop = hivpop / 4,
                  artpop = artpop / 4,
                  infections = infections / 4) %>%
    dplyr::group_by(spectrum_region_code, sex, age_quarter, quarter_id) %>%
    dplyr::summarise_at(dplyr::vars(totpop, hivpop, artpop, infections), sum) %>%
    dplyr::ungroup()


  ## Log-linear interpolate mid-year population to T1, T2, and T3
  hivpop <- tidyr::expand(spec_quarter,
                          tidyr::nesting(spectrum_region_code, sex, age_quarter),
                          quarter_id = c(quarter_id1, quarter_id2, quarter_id3)) %>%
    dplyr::full_join(spec_quarter, by = names(.)) %>%
    dplyr::group_by(spectrum_region_code, sex, age_quarter) %>%
    dplyr::mutate(hivpop = exp(zoo::na.approx(log(hivpop), quarter_id, na.rm = FALSE)),
                  hivpop = tidyr::replace_na(hivpop, 0)) %>%
    dplyr::filter(quarter_id %in% c(quarter_id1, quarter_id2, quarter_id3)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cohort_quarter = quarter_id - age_quarter,
                  age_group1 = age_quarter_to_age_group(quarter_id1 - cohort_quarter),
                  age_group2 = age_quarter_to_age_group(quarter_id2 - cohort_quarter),
                  age_group3 = age_quarter_to_age_group(quarter_id3 - cohort_quarter))

  infections_cohort <- spec_quarter %>%
    ## Subtract 4 quarters to move infections from end year to forthcoming year
    dplyr::mutate(cohort_quarter = quarter_id - age_quarter,
                  age_quarter = NULL,
                  quarter_id = quarter_id - 4) %>%
    tidyr::crossing(quarter = 0:3) %>%
    dplyr::mutate(quarter_id = quarter_id + quarter,
                  quarter = NULL,
                  infections = infections / 4,
                  age_quarter = quarter_id - cohort_quarter)

  ## Put half the new infections in the next age group
  infections_cohort <- infections_cohort %>%
    dplyr::mutate(infections = infections / 2) %>%
    dplyr::bind_rows(
             dplyr::mutate(., age_quarter = age_quarter + 1)
           ) %>%
    dplyr::filter(quarter_id >= quarter_id1,
                  quarter_id < quarter_id3 + 4) %>%
    dplyr::mutate(age_group1 = age_quarter_to_age_group(quarter_id1 - cohort_quarter),
                  age_group2 = age_quarter_to_age_group(quarter_id2 - cohort_quarter),
                  age_group3 = age_quarter_to_age_group(quarter_id3 - cohort_quarter),
                  age_group_infection = age_quarter_to_age_group(age_quarter)) %>%
    dplyr::count(spectrum_region_code, sex, quarter_id, age_group1, age_group2, age_group3,
                 age_group_infection,
                 wt = infections, name = "infections")

  
  ## Construct Lproj for t1 to t2

  hivpop_t1 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(spectrum_region_code, sex, age_group1,
                 wt = hivpop, name = "hivpop1")
  
  hivpop_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group1, age_group2,
                 wt = hivpop, name = "hivpop2")

  infections_t1t2 <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group1, age_group2,
                 wt = infections, name = "infections_t1t2")
  
  hivpop_t1t2 <- dplyr::right_join(hivpop_t1, hivpop_t2,
                                   by = c("spectrum_region_code", "sex", "age_group1")) %>%
    dplyr::left_join(infections_t1t2,
                     by = c("spectrum_region_code", "sex", "age_group1", "age_group2")) %>%
    dplyr::mutate(L_hivpop = (hivpop2 - infections_t1t2) / hivpop1)

  hivpopLproj <- hivpop_t1t2 %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group1 = age_group, area_id, idx1 = idx),
             by = c("spectrum_region_code", "sex", "age_group1")
           ) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex", "age_group2", "area_id")
           )
  
  Lproj_hivpop <- Matrix::sparseMatrix(i = hivpopLproj$idx2,
                                       j = hivpopLproj$idx1,
                                       x = hivpopLproj$L_hivpop,
                                       dims = rep(nrow(mf_model), 2))

  infections_age_t2 <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection, age_group2,
                 wt = infections, name = "infections_age_t2")

  infections_age <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection,
                 wt = infections, name = "infections_age")

  incidLproj <- dplyr::left_join(infections_age_t2, infections_age,
                                 by = c("spectrum_region_code", "sex", "age_group_infection")) %>%
    dplyr::mutate(L_incid = dplyr::if_else(infections_age == 0, 0, infections_age_t2 / infections_age)) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group_infection = age_group, area_id, idx1 = idx),
             by = c("spectrum_region_code", "sex", "age_group_infection")
           ) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex", "age_group2", "area_id")
           )

  Lproj_incid <- Matrix::sparseMatrix(i = incidLproj$idx2,
                                      j = incidLproj$idx1,
                                      x = incidLproj$L_incid,
                                      dims = rep(nrow(mf_model), 2))

  ## Paediatric entrants and survivors between time 1 and time 2

  paedLproj <- hivpop_t1t2 %>%
    dplyr::filter(is.na(age_group1)) %>%
    dplyr::select(spectrum_region_code, sex2 = sex, age_group2, hivpop2) %>%
    dplyr::left_join(
             hivpop_t1 %>%
             dplyr::left_join(get_age_groups(),
                              by = c("age_group1" = "age_group")) %>%
             dplyr::filter(
                      sex == "female",
                      age_group_start >= 15,
                      (age_group_start + age_group_span) < 50
                    ) %>%
             dplyr::select(spectrum_region_code, sex1 = sex, age_group1, hivpop1),
             by = "spectrum_region_code"
           ) %>%
    dplyr::group_by(spectrum_region_code, sex2, age_group2) %>%
    dplyr::mutate(L_paed = hivpop2 / sum(hivpop1))

  paedLproj <- paedLproj %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex1 = sex,
                           age_group1 = age_group, area_id, idx1 = idx),
             by = c("spectrum_region_code", "sex1", "age_group1")
         ) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex2 = sex,
                           age_group2 = age_group, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex2", "age_group2", "area_id")
           )

  Lproj_paed <- Matrix::sparseMatrix(i = paedLproj$idx2,
                                     j = paedLproj$idx1,
                                     x = paedLproj$L_paed,
                                     dims = rep(nrow(mf_model), 2))

  ## Construct Lproj t2 to t3

  hivpop_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group2,
                 wt = hivpop, name = "hivpop2")
  
  hivpop_t3 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group2, age_group3,
                 wt = hivpop, name = "hivpop3")

  infections_t2t3 <- infections_cohort %>%
    dplyr::filter(quarter_id >= quarter_id2,
                  quarter_id < quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group2, age_group3,
                 wt = infections, name = "infections_t2t3")
  
  hivpop_t2t3 <- dplyr::right_join(hivpop_t2, hivpop_t3,
                                   by = c("spectrum_region_code", "sex", "age_group2")) %>%
    dplyr::left_join(infections_t2t3,
                     by = c("spectrum_region_code", "sex", "age_group2", "age_group3")) %>%
    dplyr::mutate(L_hivpop = (hivpop3 - infections_t2t3) / hivpop2)

  hivpopLproj <- hivpop_t2t3 %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex", "age_group2")
           ) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group3 = age_group, area_id, idx3 = idx),
             by = c("spectrum_region_code", "sex", "age_group3", "area_id")
           )
  
  Lproj_hivpop_t2t3 <- Matrix::sparseMatrix(i = hivpopLproj$idx3,
                                            j = hivpopLproj$idx2,
                                            x = hivpopLproj$L_hivpop,
                                            dims = rep(nrow(mf_model), 2))
  
  infections_age_t3 <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection, age_group3,
                 wt = infections, name = "infections_age_t3")

  infections_age <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection,
                 wt = infections, name = "infections_age")

  incidLproj <- dplyr::left_join(infections_age_t3, infections_age,
                                 by = c("spectrum_region_code", "sex", "age_group_infection")) %>%
    dplyr::mutate(L_incid = dplyr::if_else(infections_age == 0, 0, infections_age_t3 / infections_age)) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group_infection = age_group, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex", "age_group_infection")
           ) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex, age_group3 = age_group, area_id, idx3 = idx),
             by = c("spectrum_region_code", "sex", "age_group3", "area_id")
           )

  Lproj_incid_t2t3 <- Matrix::sparseMatrix(i = incidLproj$idx3,
                                      j = incidLproj$idx2,
                                      x = incidLproj$L_incid,
                                      dims = rep(nrow(mf_model), 2))

  ## Paediatric entrants and survivors between time 2 and time 3

  paedLproj <- hivpop_t2t3 %>%
    dplyr::filter(is.na(age_group2)) %>%
    dplyr::select(spectrum_region_code, sex3 = sex, age_group3, hivpop3) %>%
    dplyr::left_join(
             hivpop_t2 %>%
             dplyr::left_join(get_age_groups(),
                              by = c("age_group2" = "age_group")) %>%
             dplyr::filter(
                      sex == "female",
                      age_group_start >= 15,
                      (age_group_start + age_group_span) < 50
                    ) %>%
             dplyr::select(spectrum_region_code, sex2 = sex, age_group2, hivpop2),
             by = "spectrum_region_code"
           ) %>%
    dplyr::group_by(spectrum_region_code, sex3, age_group3) %>%
    dplyr::mutate(L_paed = hivpop3 / sum(hivpop2))

  paedLproj <- paedLproj %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex2 = sex,
                           age_group2 = age_group, area_id, idx2 = idx),
             by = c("spectrum_region_code", "sex2", "age_group2")
         ) %>%
    dplyr::inner_join(
             dplyr::select(mf_model, spectrum_region_code, sex3 = sex,
                           age_group3 = age_group, area_id, idx3 = idx),
             by = c("spectrum_region_code", "sex3", "age_group3", "area_id")
           )

  Lproj_paed_t2t3 <- Matrix::sparseMatrix(i = paedLproj$idx3,
                                     j = paedLproj$idx2,
                                     x = paedLproj$L_paed,
                                     dims = rep(nrow(mf_model), 2))

  
  list(Lproj_hivpop = Lproj_hivpop,
       Lproj_incid = Lproj_incid,
       Lproj_paed = Lproj_paed,
       Lproj_hivpop_t2t3 = Lproj_hivpop_t2t3,
       Lproj_incid_t2t3 = Lproj_incid_t2t3,
       Lproj_paed_t2t3 = Lproj_paed_t2t3)
}

#' Interpolate Spectrum to quarter_id
#'
#' @param spec_aggr a data from of 5-year age group aggregate Spectrum estimates
#' @param calendar_quarter_out calendar quarter for desired output time point
#' 
#' 
get_spec_aggr_interpolation <- function(spec_aggr, calendar_quarter_out) {

  quarter_id_out <- calendar_quarter_to_quarter_id(calendar_quarter_out)

  val <- spec_aggr %>%
    dplyr::mutate(quarter_id = convert_quarter_id(year, 2L)) %>%
    dplyr::group_by(spectrum_region_code, sex, age_group) %>%
    dplyr::summarise(
             calendar_quarter = calendar_quarter_out,
             population_spectrum = log_lin_approx(quarter_id, totpop, quarter_id_out),
             plhiv_spectrum = log_lin_approx(quarter_id, hivpop, quarter_id_out),
             art_current_spectrum = log_lin_approx(quarter_id, artpop, quarter_id_out),
             infections_spectrum = log_lin_approx(quarter_id, infections, quarter_id_out),
             susc_previous_year_spectrum = log_lin_approx(quarter_id, susc_previous_year, quarter_id_out),
             unaware_spectrum = log_lin_approx(quarter_id, unaware, quarter_id_out),
             births_spectrum = log_lin_approx(quarter_id, births, quarter_id_out),
             births_hivpop_spectrum = log_lin_approx(quarter_id, births_hivpop, quarter_id_out),
             births_artpop_spectrum = log_lin_approx(quarter_id, births_artpop, quarter_id_out)
           ) %>%
    dplyr::ungroup()

  
  dplyr::select(val,
                spectrum_region_code, sex, age_group, calendar_quarter,
                population_spectrum,
                plhiv_spectrum,
                art_current_spectrum,
                infections_spectrum,
                susc_previous_year_spectrum,
                unaware_spectrum,
                births_spectrum,
                births_hivpop_spectrum,
                births_artpop_spectrum)
}

log_lin_approx <- function(x, y, xout, replace_na_value = 0){
  v <- exp(stats::approx(x, log(pmax(y, 0)), xout)$y)
  v <- tidyr::replace_na(v, replace_na_value)
  v
}



#' Number of births by HIV status from EPP-ASM model
#'
#' @param mod EPP-ASM model simulation, returned by [`eppasm::simmod()`].
#' @param fp  EPP-ASM fixed parameters corresponding to `mod`.
#' @param years Years for which to calculate number of births.
#'
#' @return Data frame of HIV prevalence and ART coverage among pregnant women
#'   by single-year of age.
#' 
#' @details
#'
#' This is a wrapper for [`eppasm::agepregprev()`] and [`eppasm::agepregartcov()`].
#' The code in these two functions is almost entirely duplicated, so this function
#' could be roughly reduced by half by refactoring this to one function call.
#'
#' @noRd
extract_eppasm_pregprev <- function(mod, fp, years = NULL) {

  proj_years <- fp$ss$proj_start + seq_len(fp$ss$PROJ_YEARS) - 1L
  
  if (is.null(years)) {
    years <- proj_years
  }
  
  if (!all(years %in% proj_years)) {
    stop("Ouput years not contained in shiny90 projection: ",
         paste0(setdiff(years, proj_years), collapse = ", "))
  }

  df <- expand.grid(age = fp$ss$AGE_START + fp$ss$p.fert.idx - 1L,
                    sex = "female",
                    year = years)

  idx <- df
  idx$aidx  <- idx$age - fp$ss$AGE_START+1L
  idx$agspan <- 1
  idx$yidx <- idx$year - fp$ss$proj_start + 1

  df$pregprev <- eppasm::agepregprev(mod, fp, idx$aidx, idx$yidx, idx$agspan)
  df$pregartcov <- eppasm::agepregartcov(mod, fp, idx$aidx, idx$yidx, idx$agspan)
  
  df
}
