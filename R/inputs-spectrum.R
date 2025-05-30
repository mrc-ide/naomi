#' Extract Model Inputs from Spectrum PJNZ
#'
#' @param pjnz_list Vector of filepaths to Spectrum PJNZ file.
#' @param extract_shiny90 Logical; whether to attempt to extract .shiny90 zip
#'
#' @return A `data.frame` with Spectrum indicators.
#'
#' @description
#' If the .shiny90 file does not exist within the .PJNZ, the function will
#' silently not return values, even if `extract_shiny90 = TRUE`.
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' spec <- extract_pjnz_naomi(pjnz)
#'
#' @export
extract_pjnz_naomi <- function(pjnz_list, extract_shiny90 = TRUE) {

  pjnz_list <- unroll_pjnz(pjnz_list)

  spec <- lapply(pjnz_list, extract_pjnz_one, extract_shiny90 = extract_shiny90) %>%
    dplyr::bind_rows() %>%
    dplyr::select(iso3, spectrum_country, spectrum_region_code, spectrum_region_name, year, quarter,
                  dplyr::everything())

  spec
}

unroll_pjnz <-  function(path) {
  if (!is_pjnz(path)) {
    unzip_dir <- tempfile("pjnz_unzip")
    dir.create(unzip_dir)
    zip::unzip(path, exdir = unzip_dir)
    pjnz_paths <- list.files(unzip_dir, full.names = TRUE)
    are_pjnz <- vlapply(pjnz_paths, is_pjnz)
    if (!any(are_pjnz)) {
      stop(t_("PJNZ_INVALID_ZIP"))
    }
    pjnz_paths <- pjnz_paths[are_pjnz]
  } else {
    pjnz_paths <- path
  }
  pjnz_paths
}

is_pjnz <- function(path) {
  tryCatch({
    files <- zip::zip_list(path)
    any(grepl("*.DP", files$filename))
  },
  error = function(e) {
    return(FALSE)
  })
}


extract_pjnz_one <- function(pjnz, extract_shiny90) {

  ## Code extracted from eppasm:::create_specfp()
  demp <- eppasm::read_specdp_demog_param(pjnz)
  projp <- eppasm::read_hivproj_param(pjnz)
  specres <- eppasm::read_hivproj_output(pjnz)

  specfp <- eppasm::create_spectrum_fixpar(projp, demp)
  specfp$eppmod <- "directincid_hts"
  specfp$incidinput <- eppasm::incid15to49_eppinput_specres(specres)
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

  spec <- add_shiny90_unaware(spec, pjnz, extract_shiny90)



  spectrum_region_code <- read_spectrum_region_code(pjnz)
  if (spectrum_region_code == 0) {
    spectrum_region_name <- eppasm::read_country(pjnz)
  } else {
    spectrum_region_name <- read_spectrum_region_name(pjnz)
  }
  spec$spectrum_region_code <- spectrum_region_code
  spec$spectrum_region_name <- spectrum_region_name
  spec$spectrum_country <- eppasm::read_country(pjnz)
  spec$iso3 <- eppasm::read_iso3(pjnz)

  spec$quarter <- c(2, 4)[match(specfp$projection_period, c("midyear", "calendar"))]

  spec
}

#' Extract ART and ANC testing program data inputs from Spectrum PJNZ
#'
#' @param pjnz_list Vector of filepaths to Spectrum PJNZ file.
#'
#' @return A list with a two `data.frame`s of ANC testing data and
#'   number on ART, respectively.
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' spec <- extract_pjnz_program_data(pjnz)
#'
#' @export
#'
extract_pjnz_program_data <- function(pjnz_list) {
  pjnz_list <- unroll_pjnz(pjnz_list)

  region_code <- lapply(pjnz_list, read_spectrum_region_code)

  dp_list <- lapply(pjnz_list, read_dp)

  art_dec31 <- lapply(dp_list, read_dp_art_dec31) %>%
    Map(dplyr::mutate, ., spectrum_region_code = region_code) %>%
    dplyr::bind_rows() %>%
    dplyr::select(spectrum_region_code, dplyr::everything())

  anc_testing <- lapply(dp_list, read_dp_anc_testing) %>%
    Map(dplyr::mutate, ., spectrum_region_code = region_code) %>%
    dplyr::bind_rows() %>%
    dplyr::select(spectrum_region_code, dplyr::everything())
  val <- list(art_dec31 = art_dec31, anc_testing = anc_testing)
  class(val) <- "spec_program_data"

  val
}


#'
#' Read number on ART at Dec 31 from PJNZ
#'
#' Reads the number on ART at December 31 from Spectrum PJNZ for children (0-14),
#' adult (15+) males, and adult (15+) females.
#'
#' @param dp dp data from PJNZ file
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' dp <- read_dp(pjnz)
#' read_dp_art_dec31(dp)
#'
#' @noRd
#'
read_dp_art_dec31 <- function(dp) {

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

  ## In Spectrum 2023, "<NeedARTDec31 MV>" was updated to include children in the totals
  ## -> now need to sum over 5-year age groups for age 15+ to get the adult ART need

  male_15plus_needart <- dpsub("<NeedARTDec31 MV>", 4:17*3 + 3, timedat.idx)
  male_15plus_needart <- vapply(lapply(male_15plus_needart, as.numeric), sum, numeric(1))

  female_15plus_needart <- dpsub("<NeedARTDec31 MV>", 4:17*3 + 4, timedat.idx)
  female_15plus_needart <- vapply(lapply(female_15plus_needart, as.numeric), sum, numeric(1))

  art15plus_need <- rbind(male_15plus_needart, female_15plus_needart)
  dimnames(art15plus_need) <- list(sex = c("male", "female"), year = proj.years)

  if (any(art15plus_num[art15plus_isperc == 1] < 0 |
          art15plus_num[art15plus_isperc == 1] > 100)) {
    stop("Invalid percentage on ART entered for adult ART")
  }

  ## # Adult on ART adjustment factor
  ##
  ## * Implemented from around Spectrum 6.2 (a few versions before)
  ## * Allows user to specify scalar to reduce number on ART in each year ("<AdultARTAdjFactor>")
  ## * Enabled / disabled by checkbox flag ("<AdultARTAdjFactorFlag>")
  ## * Scaling factor only applies to number inputs, not percentages (John Stover email, 20 Feb 2023)
  ##   -> Even if scaling factor specified in a year with percentage input, ignore it.
  ## ** UPDATE Spectrum 6.37 beta 18 **
  ##
  ## Two changes to the adult ART adjustment were implemented in Spectrum 6.37 beta 18:
  ##
  ## * ART adjustments were moved the main Spectrum editor and the flag variable
  ##   "<AdultARTAdjFactorFlag>" was removed from the .DP file.
  ## * New tag "<AdultPatsAllocToFromOtherRegion>" was added allowing for input
  ##   of absolute count adjustment
  ##
  ## New logic to account for these changes:
  ## * Initialise values to defaults 1.0 for relative adjustment and 0.0
  ##   for absolute adjustment.
  ## * Only check flag variable if it exists. If adjustment variable exists
  ##   but flag variable does not exist, use the adjustment.

  ## Initialise
  adult_artadj_factor <- array(1.0, dim(art15plus_num))
  dimnames(adult_artadj_factor) <- list(sex = c("male", "female"), year = proj.years)

  adult_artadj_absolute <- array(0.0, dim(art15plus_num))
  dimnames(adult_artadj_absolute) <- list(sex = c("male", "female"), year = proj.years)

  ## Flag to use adjustment
  use_artadj <- exists_dptag("<AdultARTAdjFactor>") &&
    (!exists_dptag("<AdultARTAdjFactorFlag>") ||
       (exists_dptag("<AdultARTAdjFactorFlag>") &&
          dpsub("<AdultARTAdjFactorFlag>", 2, 4) == 1))

  if (use_artadj) {

    adult_artadj_factor <- sapply(dpsub("<AdultARTAdjFactor>", 3:4, timedat.idx), as.numeric)

    if(exists_dptag("<AdultPatsAllocToFromOtherRegion>")) {
      adult_artadj_absolute <- sapply(dpsub("<AdultPatsAllocToFromOtherRegion>", 3:4, timedat.idx), as.numeric)
    }

    ## Only apply if is number (! is percentage)
    adult_artadj_factor <- adult_artadj_factor ^ as.numeric(!art15plus_isperc)
    adult_artadj_absolute <- adult_artadj_absolute * as.numeric(!art15plus_isperc)
  }

  ## First add absolute adjustment, then apply scalar adjustment (Spectrum procedure)
  art15plus_attend <- art15plus_num + adult_artadj_absolute
  art15plus_attend <- art15plus_attend * adult_artadj_factor
  art15plus_reside <- art15plus_attend + adult_artadj_absolute

  # Covert percentage coverage to absolute numbers on ART
  art15plus_num[art15plus_isperc == 1] <- art15plus_need[art15plus_isperc == 1] * art15plus_num[art15plus_isperc == 1] / 100
  art15plus_attend[art15plus_isperc == 1] <- art15plus_need[art15plus_isperc == 1] * art15plus_attend[art15plus_isperc == 1] / 100
  art15plus_reside[art15plus_isperc == 1] <- art15plus_need[art15plus_isperc == 1] * art15plus_reside[art15plus_isperc == 1] / 100

  # Reported number on ART
  art_dec31_reported <- as.data.frame.table(art15plus_num,
                                   responseName = "art_dec31_reported",
                                   stringsAsFactors = FALSE)

  # Adjusted number on ART (attending)
  art_dec31_attend <- as.data.frame.table(art15plus_attend,
                                   responseName = "art_dec31_attend",
                                   stringsAsFactors = FALSE)

  # Adjusted number on ART (residing)
  art_dec31_reside <- as.data.frame.table(art15plus_reside,
                                             responseName = "art_dec31_reside",
                                             stringsAsFactors = FALSE)

  art15plus <- purrr::reduce(list(art_dec31_reported,
                                  art_dec31_attend,
                                  art_dec31_reside), dplyr::left_join,
                             by = dplyr::join_by(sex, year))

  art15plus$age_group <- "Y015_999"
  art15plus$year <- utils::type.convert(art15plus$year, as.is = TRUE)

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

  child_art_need <- dpsub("<ChildARTCalc MV2>", 9, timedat.idx)
  child_art_need <- as.numeric(child_art_need)
  child_art_need <- stats::approx(proj.years, child_art_need, proj.years + 0.5, rule = 2)$y
  names(child_art_need) <- proj.years

  child_art <- dplyr::case_when(child_art_isperc == 0  & !is.na(child_art_aggr) ~ child_art_aggr,
                                child_art_isperc == 0  & !is.na(child_art_0to14) ~ child_art_0to14,
                                child_art_isperc == 1 ~ child_art_need * child_art_0to14 / 100)
  names(child_art) <- proj.years

  if (any(is.na(child_art))) {
    stop("Something has gone wrong extracting child ART inputs; please seek troubleshooting.")
  }


  ## # Child on ART adjustment factor
  ##
  ## * Implemented same as adult adjustment factor above

  ## Initialise
  child_artadj_factor <- rep(1.0, length(child_art))
  child_artadj_absolute <- rep(0.0, length(child_art))

  ## Flag to use adjustment
  use_child_artadj <- exists_dptag("<ChildARTAdjFactor MV>") &&
    (!exists_dptag("<ChildARTAdjFactorFlag>") ||
       (exists_dptag("<ChildARTAdjFactorFlag>") &&
          dpsub("<ChildARTAdjFactorFlag>", 2, 4) == 1))

  if (use_child_artadj) {

    child_artadj_factor <- as.numeric(dpsub("<ChildARTAdjFactor MV>", 2, timedat.idx))

    if(exists_dptag("<ChildPatsAllocToFromOtherRegion MV>")) {
      child_artadj_absolute <- as.numeric(dpsub("<ChildPatsAllocToFromOtherRegion MV>", 2, timedat.idx))
    }

    ## Only apply if is number (! is percentage)
    child_artadj_factor <- child_artadj_factor ^ !child_art_isperc
    child_artadj_absolute <- child_artadj_absolute ^ !child_art_isperc
  }

  ## First add absolute adjustment, then apply scalar adjustment (Spectrum procedure)
  child_art_attend <- child_art + child_artadj_absolute
  child_art_attend <- child_art_attend * child_artadj_factor
  child_art_reside <- child_art_attend + child_artadj_absolute

  child_art <- data.frame(sex = "both",
                          age_group = "Y000_014",
                          year = proj.years,
                          art_dec31_reported = child_art,
                          art_dec31_attend = child_art_attend,
                          art_dec31_reside = child_art_reside)

  art_dec31 <- rbind(child_art, art15plus) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 0)),
                  art_dec31 = art_dec31_attend)

  art_dec31
}

#' Disaggregate the number on ART Dec 31 to single age
#'
#' @noRd
#'
add_dec31_art <- function(spec, pjnz) {

  dp <- read_dp(pjnz)
  art_dec31 <- read_dp_art_dec31(dp)

  ## Distribute coarse (<15/15+) ART data to 5-year age groups
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

  stopifnot(all.equal(sum(spec$artpop_dec31), sum(art_dec31$art_dec31)))

  spec
}


#' Read ANC testing inputs from PJNZ
#'
#' Reads ANC testing cascade inputs from Spectrum PJNZ.
#'
#' @param pjnz path to PJNZ file
#'
#' @examples
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' read_dp_anc_testing(pjnz)
#'
#' @noRd
#'
read_dp_anc_testing <- function(dp) {

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

  anc_indicators <- c("anc_clients", "anc_tested", "anc_tested_pos", "anc_known_pos", "anc_known_neg")
  if (exists_dptag("<ANCTestingValues MV>")) {
    anc_testing <- dpsub("<ANCTestingValues MV>", 2:5, timedat.idx)
    anc_testing <- sapply(anc_testing, as.integer)
    dimnames(anc_testing) <- list(indicator = anc_indicators[1:4], year = proj.years)
  } else if (exists_dptag("<ANCTestingValues MV2>")) {
    anc_testing <- dpsub("<ANCTestingValues MV2>", 2:5, timedat.idx)
    anc_testing <- sapply(anc_testing, as.integer)
    dimnames(anc_testing) <- list(indicator = anc_indicators[1:4], year = proj.years)
  } else if (exists_dptag("<ANCTestingValues MV4>")) {
    anc_testing <- dpsub("<ANCTestingValues MV4>", c(2:5, 10), timedat.idx)
    anc_testing <- sapply(anc_testing, as.integer)
    dimnames(anc_testing) <- list(indicator = anc_indicators, year = proj.years)
  } else {
    stop("ANC testing inputs not found in .DP file")
  }
  anc_testing[anc_testing == -9999] <- NA_real_

  ## Note: these values start 1 column later than other arrays in the .DP file
  ## If value is 0, interpret as not entered (NA)
  if (exists_dptag("<ARVRegimen MV2>")) {
    anc_already_art <- dpsub("<ARVRegimen MV2>", 11, timedat.idx+1)
  } else if (exists_dptag("<ARVRegimen MV3>")) {
    anc_already_art <- dpsub("<ARVRegimen MV3>", 11, timedat.idx+1)
  }
  anc_already_art <- sapply(anc_already_art, as.integer)
  anc_already_art[anc_already_art == 0] <- NA

  anc_testing <- rbind(anc_testing, "anc_already_art" = anc_already_art)
  names(dimnames(anc_testing)) <- c("indicator", "year")

  anc_testing <- as.data.frame.table(anc_testing,
                                     responseName = "value",
                                     stringsAsFactors = FALSE)
  anc_testing$year <- utils::type.convert(anc_testing$year, as.is = TRUE)
  anc_testing <- dplyr::filter(anc_testing, !is.na(value))

  anc_testing
}

add_shiny90_unaware <- function(spec, pjnz, extract_shiny90) {

  if (extract_shiny90 && assert_pjnz_shiny90(pjnz)) {
    shiny90_dir <- tempfile()
    on.exit(unlink(shiny90_dir, recursive = TRUE))

    shiny90_name <- get_pjnz_shiny90_filename(pjnz)
    utils::unzip(pjnz, shiny90_name, exdir = shiny90_dir)

    df <- extract_shiny90_age_sex(shiny90_path = file.path(shiny90_dir, shiny90_name),
                                  pjnz_path = pjnz,
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
  if (is.null(region_name) || !nzchar(region_name)) {
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
  shiny90file <- grep("\\.shiny90$", files, ignore.case = TRUE, value = TRUE)

  if (length(shiny90file) > 1) {
    msg <- paste0("Multiple .shiny90 files found: ",
                  paste0(shiny90file, collapse = ", "))

    ## If multiple files found, choose file with shortest name
    shiny90file <- shiny90file[which.min(nchar(shiny90file))]

    msg <- paste0(msg, "\nUsing file: ", shiny90file)

    ## No need to translate this. It shouldn't happen
    naomi_warning(msg, c("model_options", "model_fit"))
  }

  shiny90file
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

#' Interpolate Spectrum to quarter_id
#'
#' @param spec_aggr a data from of 5-year age group aggregate Spectrum estimates
#' @param calendar_quarter_out calendar quarter for desired output time point
#'
#'
get_spec_aggr_interpolation <- function(spec_aggr, calendar_quarter_out) {

  quarter_id_out <- calendar_quarter_to_quarter_id(calendar_quarter_out)

  spec_aggr <- spec_aggr %>%
    dplyr::mutate(
      quarter_id = convert_quarter_id(year, quarter),
      quarter_id_dec31 = convert_quarter_id(year, 4L)
    )

  ## If projected values `artpop` are Q4 (Dec 31), use the Dec 31 specified
  ## values for interpolation rather than the the internal model values.
  ## Achieve this by setting the `artpop` values to missing.
  spec_aggr <- spec_aggr %>%
    dplyr::mutate(
      artpop_external = dplyr::if_else(quarter_id == quarter_id_dec31 & !is.na(artpop_dec31), NA_real_, artpop)
    )

  val <- spec_aggr %>%
    dplyr::group_by(spectrum_region_code, spectrum_region_name, sex, age_group) %>%
    dplyr::summarise(
      calendar_quarter = calendar_quarter_out,
      population_spectrum = log_lin_approx(quarter_id, totpop, quarter_id_out),
      plhiv_spectrum = log_lin_approx(quarter_id, hivpop, quarter_id_out),
      ##
      ## Note: art_current interpolates mid-year and end-year ART values
      art_current_spectrum = log_lin_approx(c(quarter_id, quarter_id_dec31),
                                            c(artpop_external, artpop_dec31), quarter_id_out),
      ##
      art_current_internal_spectrum = log_lin_approx(quarter_id, artpop, quarter_id_out),
      infections_spectrum = log_lin_approx(quarter_id, infections, quarter_id_out),
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
                art_current_internal_spectrum,
                infections_spectrum,
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

read_dp <- function(pjnz) {
  dpfile <- grep(".DP$", utils::unzip(pjnz, list = TRUE)$Name, value = TRUE)
  as.data.frame(
    readr::read_csv(unz(pjnz, dpfile),
                    name_repair = "minimal",
                    col_types = readr::cols(.default = "c")))
}
