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
    dplyr::select(spectrum_region_code, spectrum_region_name, dplyr::everything())

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

  spec <- add_dec31_art(spec, pjnz)
  
  spec <- add_shiny90_unaware(spec, pjnz)


  spectrum_region_code <- read_spectrum_region_code(pjnz)

  if (spectrum_region_code == 0) {
    spectrum_region_name <- eppasm::read_country(pjnz)
  } else {
    spectrum_region_name <- read_spectrum_region_name(pjnz)
  }

  spec$spectrum_region_code <- spectrum_region_code
  spec$spectrum_region_name <- spectrum_region_name

  spec
}

#'
#' Read number on ART at Dec 31 from PJNZ
#'
#' Reads the number on ART at December 31 from Spectrum PJNZ for children (0-14),
#' adult (15+) males, and adult (15+) females.
#'
#' @param pjnz path to PJNZ file
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' read_pjnz_art_dec31(pjnz)
#'
read_pjnz_art_dec31 <- function(pjnz) {

  dpfile <- grep(".DP$", unzip(pjnz, list = TRUE)$Name, value = TRUE)
  dp <- read.csv(unz(pjnz, dpfile), as.is = TRUE)

  exists_dptag <- function(tag, tagcol = 1) {
    tag %in% dp[, tagcol]
  }
  dpsub <- function(tag, rows, cols, tagcol = 1) {
    dp[which(dp[, tagcol] == tag) + rows, cols]
  }
  yr_start <- as.integer(dpsub("<FirstYear MV2>", 2, 4))
  yr_end <- as.integer(dpsub("<FinalYear MV2>", 2, 4))
  proj.years <- yr_start:yr_end
  timedat.idx <- 4 + 1:length(proj.years) - 1

  art15plus_isperc <- sapply(dpsub("<HAARTBySexPerNum MV>", 4:5, timedat.idx), as.integer)
  dimnames(art15plus_isperc) <- list(sex = c("male", "female"), year = proj.years)

  art15plus_num <- sapply(dpsub("<HAARTBySex MV>", 4:5, timedat.idx), as.numeric)
  dimnames(art15plus_num) <- list(sex = c("male", "female"), year = proj.years)

  art15plus_need <- sapply(dpsub("<NeedARTDec31 MV>", 3:4, timedat.idx), as.numeric)
  dimnames(art15plus_need) <- list(sex = c("male", "female"), year = proj.years)

  if (any(art15plus_num[art15plus_isperc == 1] < 1 |
          art15plus_num[art15plus_isperc == 1] > 100)) {
    stop("Invalid percentage on ART entered for adult ART")
  }

  art15plus_num[art15plus_isperc == 1] <- art15plus_need[art15plus_isperc == 1] * art15plus_num[art15plus_isperc == 1] / 100

  art15plus <- as.data.frame.table(art15plus_num,
                                   responseName = "art_dec31",
                                   stringsAsFactors = FALSE)
  art15plus$age_group <- "Y015_999"
  art15plus$year <- type.convert(art15plus$year, as.is = TRUE)


  ## # Child number on ART
  ##
  ## - If age-stratified entered, use sum of three age categories
  ## - Else, if number entered for total children 0-14 on ART, use that
  ## - Else, if percentage entered for children 0-14 on ART, use percentage
  ##   times children needing ART.
  ##
  ## Children needing ART taken from line 9 of <ChildARTCalc MV2>, interpolated
  ## to end of year. I am not entirely sure what this calculation is, but
  ## averaging between two years in the internal values matches the values
  ## reported in the Spectrum ART results table for "Children needing ART (0-14)".
  ##
  ## The Spectrum output for number of children on ART in the table can be quite
  ## different from the internal number on ART in the projection period. I am
  ## not sure what explains the difference; it should be minimal impact here.

  child_art_raw<- dpsub("<ChildTreatInputs MV3>", 3:6, timedat.idx)
  child_art_raw <- sapply(child_art_raw, as.numeric)
  child_art_raw[child_art_raw == -9999] <- NA_real_
  colnames(child_art_raw) <- proj.years

  child_art_0to14 <- child_art_raw[1,]
  child_art_aggr <- colSums(child_art_raw[2:4,])

  ## Note: these errors should never occur; don't need to be translated.
  if (any(is.na(child_art_0to14) & is.na(child_art_aggr))) {
    stop(paste0("No child ART input found for year: ",
                paste0(names(which(is.na(child_art_0to14) & is.na(child_art_aggr))), collapse = ", "),
                ". Check PJNZ file inputs"))
  }

  if (any(!is.na(child_art_0to14) & !is.na(child_art_aggr))) {
    stop(paste0("Both aggregated and age-stratified child ART input found for year: ",
                paste0(names(which(!is.na(child_art_0to14) & !is.na(child_art_aggr))), collapse = ", "),
                ". Check PJNZ file inputs"))
  }

  child_art_isperc <- dpsub("<ChildARTByAgeGroupPerNum MV2>", 3, timedat.idx)
  child_art_isperc <- as.integer(child_art_isperc)
  names(child_art_isperc) <- proj.years

  child_art_need<- dpsub("<ChildARTCalc MV2>", 9, timedat.idx)
  child_art_need <- as.numeric(child_art_need)
  child_art_need <- approx(proj.years, child_art_need, proj.years + 0.5, rule = 2)$y
  names(child_art_need) <- proj.years

  child_art <- dplyr::case_when(child_art_isperc == 0  & !is.na(child_art_aggr) ~ child_art_aggr,
                                child_art_isperc == 0  & !is.na(child_art_0to14) ~ child_art_0to14,
                                child_art_isperc == 1 ~ child_art_need * child_art_0to14 / 100)
  names(child_art) <- proj.years

  if (any(is.na(child_art))) {
    stop("Something has gone wrong extracting child ART inputs; please seek troubleshooting.")
  }

  child_art <- data.frame(sex = "both",
                          age_group = "Y000_014",
                          year = proj.years,
                          art_dec31 = child_art)

  art_dec31 <- rbind(child_art, art15plus)

  art_dec31
}

#' Disaggregate the number on ART Dec 31 to single age
#' 
add_dec31_art <- function(spec, pjnz) {

  art_dec31 <- read_pjnz_art_dec31(pjnz)

  spec <- spec %>%
    dplyr::mutate(age_group_coarse = dplyr::if_else(age <= 14, "Y000_014", "Y015_999"),
                  sex_join = dplyr::if_else(age_group_coarse == "Y000_014", "both", sex)) %>%
    dplyr::group_by(sex_join, age_group_coarse, year) %>%
    dplyr::mutate(art_prop = dplyr::if_else(artpop == 0, 0.0, artpop / sum(artpop))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(art_dec31, by = c("sex_join" = "sex", "age_group_coarse" = "age_group", "year")) %>%
    dplyr::mutate(
      artpop_dec31 = dplyr::if_else(is.na(art_prop), 0.0, art_prop * art_dec31),
      age_group_coarse = NULL,
      sex_join = NULL,
      art_prop = NULL,
      art_dec31 = NULL
    )

  stopifnot(sum(spec$artpop_dec31) == sum(art_dec31$art_dec31))

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

#' Read Subnational Region Name from Spectrum PJNZ
#'
#' @param pjnz file path to Spectrum PJNZ file.
#'
#' @return Spectrum subnational region name as a string. Returns NA
#'   if no subnational region.
#'
#' @details
#' Value NA corresponds to region code 0 for a national Spectrum file.
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' read_spectrum_region_name(pjnz)
#'
#' @export
read_spectrum_region_name <- function(pjnz) {
  pjn <- eppasm::read_pjn(pjnz)
  region_name <- pjn[which(pjn[, 1] == "<Projection Parameters - Subnational Region Name2>") + 2, 4]
  if (region_name == "") {
    region_name <- NA_character_
  }
  region_name
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

create_Lproj <- function(spec, mf_model, quarter_id1, quarter_id2, quarter_id3,
                         adjust_area_growth = TRUE) {

  ## Graduate HIV population to quarter-year age using Hyman monotonic
  ## spline interpolation. 80+ open-ended age group is extended to age 100.
  graduate_mono <- function(y, x, xout) {
    xout <- c(xout, 2*xout[length(xout)] - xout[length(xout)-1])
    diff(splinefun(x, c(0, cumsum(y)), method = "hyman")(xout))
  }

  spec_quarter <- spec %>%
    dplyr::mutate(quarter_id = convert_quarter_id(year, 2L),
                  year = NULL) %>%
    dplyr::filter(dplyr::between(quarter_id, quarter_id1 - 4, quarter_id3 + 8)) %>%
    dplyr::group_by(spectrum_region_code, quarter_id, sex) %>%
    dplyr::summarise(
      age_quarter = 0:(400-1),
      dplyr::across(c(totpop, hivpop, artpop, infections), graduate_mono, c(age, 100), age_quarter/4),
      .groups = "drop"
    )

  ## Log-linear interpolate mid-year population to T1, T2, and T3
  hivpop <- tidyr::expand(spec_quarter,
                          tidyr::nesting(spectrum_region_code, sex, age_quarter),
                          quarter_id = c(quarter_id1, quarter_id2, quarter_id3)) %>%
    dplyr::full_join(spec_quarter, by = names(.)) %>%
    dplyr::group_by(spectrum_region_code, sex, age_quarter) %>%
    dplyr::mutate(
      hivpop = exp(zoo::na.approx(log(hivpop), quarter_id, na.rm = FALSE)),
      hivpop = tidyr::replace_na(hivpop, 0),
      totpop = exp(zoo::na.approx(log(totpop), quarter_id, na.rm = FALSE)),
      totpop = tidyr::replace_na(totpop, 0)
    ) %>%
    dplyr::filter(quarter_id %in% c(quarter_id1, quarter_id2, quarter_id3)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cohort_quarter = quarter_id - age_quarter,
                  age_group = age_quarter_to_age_group(age_quarter),
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


  ## Expand area-level populations to long format
  totpop_area <- mf_model %>%
    dplyr::select(area_id, spectrum_region_code, sex, age_group,
                  population_t1, population_t2, population_t3) %>%
    tidyr::pivot_longer(c(population_t1, population_t2, population_t3),
                        values_to = "population") %>%
    dplyr::mutate(
      quarter_id = dplyr::case_when(name == "population_t1" ~ quarter_id1,
                                    name == "population_t2" ~ quarter_id2,
                                    name == "population_t3" ~ quarter_id3),
      name = NULL
    )


  ## Graduate 5-year age group population to quarter-year age group population
  ## use hyman smoothing on cumulative population.

  totpop_area <- totpop_area %>%
    dplyr::left_join(
      dplyr::select(get_age_groups(), age_group, age_group_start),
      by = "age_group"
    ) %>%
    dplyr::group_by(area_id, spectrum_region_code, sex, quarter_id) %>%
    dplyr::summarise(
      age_quarter = 0:(400-1),
      population = graduate_mono(population, c(age_group_start, 100), age_quarter/4),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      ## Merge age groups
      dplyr::select(hivpop, spectrum_region_code, sex, quarter_id,
                    age_quarter, age_group, age_group1, age_group2, age_group3),
      by = c("spectrum_region_code", "sex", "quarter_id", "age_quarter")
    )


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


  ## # Calculate net_growth_ratio: ratio of district population change vs. Spectrum
  ##   region population change by cohort.

  totpop_area_t1 <- totpop_area %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(area_id, spectrum_region_code, sex, age_group1,
                 wt = population, name = "population1")

  totpop_area_t2 <- totpop_area %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(area_id, spectrum_region_code, sex, age_group1, age_group2,
                 wt = population, name = "population2")

  totpop_area_t1t2 <- totpop_area_t1 %>%
    dplyr::right_join(totpop_area_t2,
                      by = c("area_id", "spectrum_region_code", "sex", "age_group1")) %>%
    dplyr::mutate(
      totpop_ratio_area = population2 / population1,
      population1 = NULL,
      population2 = NULL
    )


  totpop_spec_t1 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(spectrum_region_code, sex, age_group1,
                 wt = totpop, name = "totpop1")

  totpop_spec_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group1, age_group2,
                 wt = totpop, name = "totpop2")

  totpop_spec_t1t2 <- totpop_spec_t1 %>%
    dplyr::right_join(totpop_spec_t2,
                      by = c("spectrum_region_code", "sex", "age_group1")) %>%
    dplyr::mutate(
      totpop_ratio_spec = totpop2 / totpop1,
      totpop2 = NULL,
      totpop1 = NULL
    )

  net_growth_ratio_t1t2 <- totpop_area_t1t2 %>%
    dplyr::right_join(totpop_spec_t1t2,
                      by = c("spectrum_region_code", "sex", "age_group1", "age_group2")) %>%
    dplyr::mutate(net_growth_ratio = totpop_ratio_area / totpop_ratio_spec) %>%
    dplyr::select(area_id, sex, age_group1, age_group2, totpop_ratio_area, totpop_ratio_spec, net_growth_ratio)

  net_growth_ratio_t1t2_aggr <- net_growth_ratio_t1t2 %>%
    dplyr::group_by(area_id, sex, age_group1) %>%
    dplyr::summarise(dplyr::across(c(totpop_ratio_area, totpop_ratio_spec), sum), .groups = "drop") %>%
    dplyr::mutate(net_growth_ratio = totpop_ratio_area / totpop_ratio_spec)

  if (!adjust_area_growth) {
    net_growth_ratio_t1t2$net_growth_ratio <- 1.0
    net_growth_ratio_t1t2_aggr$net_growth_ratio <- 1.0
  }

  hivpopLproj <- hivpop_t1t2 %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group1 = age_group, area_id, idx1 = idx),
      by = c("spectrum_region_code", "sex", "age_group1")
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
      by = c("spectrum_region_code", "sex", "age_group2", "area_id")
    )

  hivpopLproj <- hivpopLproj %>%
    dplyr::left_join(
      net_growth_ratio_t1t2,
      by = c("area_id", "sex", "age_group1", "age_group2")
    ) %>%
    dplyr::mutate(
      L_hivpop = L_hivpop * net_growth_ratio
    )

  Lproj_hivpop <- Matrix::sparseMatrix(i = hivpopLproj$idx2,
                                       j = hivpopLproj$idx1,
                                       x = hivpopLproj$L_hivpop,
                                       dims = rep(nrow(mf_model), 2))

  Lproj_netgrow_t1t2 <- Matrix::sparseMatrix(i = hivpopLproj$idx2,
                                             j = hivpopLproj$idx1,
                                             x = hivpopLproj$net_growth_ratio,
                                             dims = rep(nrow(mf_model), 2))


  infections_age_t2 <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection, age_group2,
                 wt = infections, name = "infections_age_t2")

  infections_age <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection,
                 wt = infections, name = "infections_age")

  incidLproj <- infections_age_t2 %>%
    dplyr::left_join(infections_age,
                     by = c("spectrum_region_code", "sex", "age_group_infection")) %>%
    dplyr::mutate(
      L_incid = dplyr::if_else(infections_age == 0, 0, infections_age_t2 / infections_age)
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group_infection = age_group, area_id, idx1 = idx),
      by = c("spectrum_region_code", "sex", "age_group_infection")
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
      by = c("spectrum_region_code", "sex", "age_group2", "area_id")
    )

  incidLproj <- incidLproj %>%
    dplyr::left_join(
      net_growth_ratio_t1t2,
      by = c("area_id", "sex", "age_group_infection" = "age_group1", "age_group2")
    ) %>%
    dplyr::mutate(
      L_incid = L_incid * (net_growth_ratio ^ 0.5)
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
    dplyr::mutate(L_paed = hivpop2 / sum(hivpop1)) %>%
    dplyr::ungroup()

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

  paedLproj <- paedLproj %>%
    dplyr::left_join(
      dplyr::filter(net_growth_ratio_t1t2_aggr, sex == "female"),
      by = c("area_id", "age_group1")
    ) %>%
    dplyr::mutate(
      L_paed = L_paed * (net_growth_ratio ^ 0.5)
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


  ## Calculate net_growth_ratio: ratio of district population change vs. Spectrum
  ## region population change by cohort.

  totpop_area_t2 <- totpop_area %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(area_id, spectrum_region_code, sex, age_group2,
                 wt = population, name = "population2")

  totpop_area_t3 <- totpop_area %>%
    dplyr::filter(quarter_id == quarter_id3) %>%
    dplyr::count(area_id, spectrum_region_code, sex, age_group2, age_group3,
                 wt = population, name = "population3")

  totpop_area_t2t3 <- totpop_area_t2 %>%
    dplyr::right_join(totpop_area_t3,
                      by = c("area_id", "spectrum_region_code", "sex", "age_group2")) %>%
    dplyr::mutate(
      totpop_ratio_area = population3 / population2,
      population1 = NULL,
      population2 = NULL
    )

  totpop_spec_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group2,
                 wt = totpop, name = "totpop2")

  totpop_spec_t3 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group2, age_group3,
                 wt = totpop, name = "totpop3")

  totpop_spec_t2t3 <- totpop_spec_t2 %>%
    dplyr::right_join(totpop_spec_t3,
                      by = c("spectrum_region_code", "sex", "age_group2")) %>%
    dplyr::mutate(
      totpop_ratio_spec = totpop3 / totpop2,
      totpop2 = NULL,
      totpop1 = NULL
    )

  net_growth_ratio_t2t3 <- totpop_area_t2t3 %>%
    dplyr::right_join(totpop_spec_t2t3,
                      by = c("spectrum_region_code", "sex", "age_group2", "age_group3")) %>%
    dplyr::mutate(net_growth_ratio = totpop_ratio_area / totpop_ratio_spec) %>%
    dplyr::select(area_id, sex, age_group2, age_group3, totpop_ratio_area, totpop_ratio_spec, net_growth_ratio)

  net_growth_ratio_t2t3_aggr <- net_growth_ratio_t2t3 %>%
    dplyr::group_by(area_id, sex, age_group2) %>%
    dplyr::summarise(dplyr::across(c(totpop_ratio_area, totpop_ratio_spec), sum), .groups = "drop") %>%
    dplyr::mutate(net_growth_ratio = totpop_ratio_area / totpop_ratio_spec)

  if (!adjust_area_growth) {
    net_growth_ratio_t2t3$net_growth_ratio <- 1.0
    net_growth_ratio_t2t3$net_growth_ratio <- 1.0
  }


  hivpopLproj <- hivpop_t2t3 %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
      by = c("spectrum_region_code", "sex", "age_group2")
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group3 = age_group, area_id, idx3 = idx),
      by = c("spectrum_region_code", "sex", "age_group3", "area_id")
    )

  hivpopLproj <- hivpopLproj %>%
    dplyr::left_join(
      net_growth_ratio_t2t3,
      by = c("area_id", "sex", "age_group2", "age_group3")
    ) %>%
    dplyr::mutate(
      L_hivpop = L_hivpop * net_growth_ratio
    )

  Lproj_hivpop_t2t3 <- Matrix::sparseMatrix(i = hivpopLproj$idx3,
                                            j = hivpopLproj$idx2,
                                            x = hivpopLproj$L_hivpop,
                                            dims = rep(nrow(mf_model), 2))

  Lproj_netgrow_t2t3 <- Matrix::sparseMatrix(i = hivpopLproj$idx3,
                                             j = hivpopLproj$idx2,
                                             x = hivpopLproj$net_growth_ratio,
                                             dims = rep(nrow(mf_model), 2))

  infections_age_t3 <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection, age_group3,
                 wt = infections, name = "infections_age_t3")

  infections_age <- infections_cohort %>%
    dplyr::filter(quarter_id < quarter_id3) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection,
                 wt = infections, name = "infections_age")

  incidLproj <- infections_age_t3 %>%
    dplyr::left_join(infections_age,
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

  incidLproj <- incidLproj %>%
    dplyr::left_join(
      net_growth_ratio_t2t3,
      by = c("area_id", "sex", "age_group_infection" = "age_group2", "age_group3")
    ) %>%
    dplyr::mutate(
      L_incid = L_incid * (net_growth_ratio ^ 0.5)
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

  paedLproj <- paedLproj %>%
    dplyr::left_join(
      dplyr::filter(net_growth_ratio_t2t3_aggr, sex == "female"),
      by = c("area_id", "age_group2")
    ) %>%
    dplyr::mutate(
      L_paed = L_paed * (net_growth_ratio ^ 0.5)
    )

  Lproj_paed_t2t3 <- Matrix::sparseMatrix(i = paedLproj$idx3,
                                          j = paedLproj$idx2,
                                          x = paedLproj$L_paed,
                                          dims = rep(nrow(mf_model), 2))


  list(Lproj_hivpop = Lproj_hivpop,
       Lproj_incid = Lproj_incid,
       Lproj_paed = Lproj_paed,
       Lproj_netgrow_t1t2= Lproj_netgrow_t1t2,
       Lproj_hivpop_t2t3 = Lproj_hivpop_t2t3,
       Lproj_incid_t2t3 = Lproj_incid_t2t3,
       Lproj_paed_t2t3 = Lproj_paed_t2t3,
       Lproj_netgrow_t2t3 = Lproj_netgrow_t2t3)
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
    dplyr::group_by(spectrum_region_code, spectrum_region_name, sex, age_group) %>%
    dplyr::summarise(
      calendar_quarter = calendar_quarter_out,
      population_spectrum = log_lin_approx(quarter_id, totpop, quarter_id_out),
      plhiv_spectrum = log_lin_approx(quarter_id, hivpop, quarter_id_out),
      ##
      ## Note: art_current interpolates mid-year and end-year ART values
      art_current_spectrum = log_lin_approx(c(quarter_id, quarter_id+2),
                                            c(artpop, artpop_dec31), quarter_id_out),
      ##
      infections_spectrum = log_lin_approx(quarter_id, infections, quarter_id_out),
      susc_previous_year_spectrum = log_lin_approx(quarter_id, susc_previous_year, quarter_id_out),
      unaware_spectrum = log_lin_approx(quarter_id, unaware, quarter_id_out),
      births_spectrum = log_lin_approx(quarter_id, births, quarter_id_out),
      births_hivpop_spectrum = log_lin_approx(quarter_id, births_hivpop, quarter_id_out),
      births_artpop_spectrum = log_lin_approx(quarter_id, births_artpop, quarter_id_out)
    ) %>%
    dplyr::ungroup()

  ## Include 31 Dec values in the art_current interpolation creates an opportunity
  ## for inconsistent results for PLHIV, on ART, and unaware. Cap values to ensure
  ## proportions < 1.0.
  val <- val %>%
    dplyr::mutate(
      art_current_spectrum = pmin(plhiv_spectrum, art_current_spectrum),
      unaware_spectrum = pmin(plhiv_spectrum - art_current_spectrum, unaware_spectrum)
    )
  
  dplyr::select(val,
                spectrum_region_code, spectrum_region_name, sex, age_group, calendar_quarter,
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
