##' Compare aggregated district ART inputs + spectrum totals

##' Compare aggregated subnational ART inputs + spectrum totals for comparison table
##'
##' @param art Path to file containing ART data or ART data object
##' @param shape Path to file containing geojson areas data or areas data object
##' @param pjnz Path to zip file containing spectrum pjnz file/s
##' @keywords internal
prepare_art_spectrum_comparison <- function(art, shape, pjnz) {

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    shape <- read_area_merged(shape) }

  ## Check if art is object or file path
  if(!inherits(art, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    art <- read_art_number(art, all_columns = TRUE)}

  ## PJNZ either object or file path
  if (!inherits(pjnz, "spec_program_data")) {
    pjnz <- extract_pjnz_program_data(pjnz) }

  ## Aggregate ART data
  art_agreggated <- art |>
    dplyr::left_join(shape,  by = "area_id") |>
    dplyr::count(spectrum_region_code, calendar_quarter, sex, age_group,
                 wt = art_current, name = "value_naomi")

  if(identical(unique(art$sex), c("both"))) {
    # If no sex aggregated data present in ART data, aggregate Spectrum by age
    spec_aggreagted <- pjnz$art_dec31 |>
      dplyr::mutate(calendar_quarter = paste0("CY", year, "Q4")) %>%
      dplyr::count(spectrum_region_code, calendar_quarter, age_group,
                   wt = art_dec31, name = "value_spectrum") |>
      dplyr::mutate(sex = "both")

  } else {
    # If sex aggregated data present in ART data, aggregate Spectrum by age and sex
    spec_aggreagted <- pjnz$art_dec31 |>
      dplyr::mutate(calendar_quarter = paste0("CY", year, "Q4")) %>%
      dplyr::count(spectrum_region_code, calendar_quarter, sex, age_group,
                   wt = art_dec31, name = "value_spectrum")
  }

  # Get spectrum level to select correct area names
  spectrum_region_code <- unique(shape$spectrum_region_code)

  spectrum_level <- as.integer(length(spectrum_region_code) > 1)

  dat  <- dplyr::left_join(art_agreggated, spec_aggreagted,
                           by = c("spectrum_region_code", "calendar_quarter",
                                  "sex", "age_group")) |>
    dplyr::left_join(shape |>
                       dplyr::filter(area_level == spectrum_level) |>
                       dplyr::select(area_name, spectrum_region_code),
                     by = "spectrum_region_code")

  #  Return data formatted for comparison table
  dat |>
    dplyr::mutate(
      indicator = "number_on_art",
      year = naomi::calendar_quarter_to_year(calendar_quarter),
      group = dplyr::if_else(age_group == "Y000_014",
                             "art_children", paste0("art_adult_", sex)),
      difference = value_spectrum - value_naomi) |>
    dplyr::select(indicator, area_name, year, group,
                  value_spectrum, value_naomi, difference)
}

##' Compare aggregated subnational ART inputs + spectrum totals for comparison table
##'
##' @param art Path to file containing ART data or ART data object
##' @param shape Path to file containing geojson areas data or areas data object
##' @param pjnz Path to zip file containing spectrum pjnz file/s
##' @keywords internal
prepare_anc_spectrum_comparison <- function(anc, shape, pjnz) {

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    shape <- read_area_merged(shape) }

  ## Check if anc is object or file path
  if(!inherits(anc, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    anc <- read_anc_testing(anc)
  }

  ## PJNZ either object or file path
  if (!inherits(pjnz, "spec_program_data")) {
    pjnz <- extract_pjnz_program_data(pjnz) }

  ## Aggregate ART data
  anc_agreggated <- anc |>
    dplyr::left_join(shape,  by = "area_id") |>
    tidyr::pivot_longer(dplyr::starts_with("anc"),
                        names_to = "indicator",
                        values_to = "value_naomi") |>
    dplyr::count(spectrum_region_code, age_group, year, indicator,
                 wt = value_naomi, name = "value_naomi")

  ## Aggregate Spectrum data
  spec_aggregated <- pjnz$anc_testing |>
    dplyr::rename("value_spectrum" = "value")

  # Get spectrum level to select correct area names
  spectrum_region_code <- unique(shape$spectrum_region_code)

  if(length(spectrum_region_code) > 1){spectrum_level <- 1}else{spectrum_level <- 0}

  dat  <- dplyr::left_join(anc_agreggated, spec_aggregated,
                           by = c("spectrum_region_code", "year", "indicator")) |>
    dplyr::left_join(shape |>
                       dplyr::filter(area_level == spectrum_level) |>
                       dplyr::select(area_name, spectrum_region_code),
                     by = "spectrum_region_code") |>
    dplyr::filter(indicator %in% unique(pjnz$anc_testing$indicator))

  #  Return data formatted for comparison table
  dat |>
    dplyr::mutate(
      sex = "female", age_group = "Y015_049",
      group = "anc_adult_female",
      difference = value_spectrum - value_naomi) |>
    dplyr::select(indicator, area_name, year, group,
                  value_spectrum, value_naomi, difference)

}

##' Compare aggregated subnational Naomi + spectrum totals for comparison table
##'
##' @param art Path to file containing ART data or ART data object
##' @param anc Path to file containing ART data or ART data object
##' @param shape Path to file containing geojson areas data or areas data object
##' @param pjnz Path to zip file containing spectrum pjnz file/s
##' @export
prepare_spectrum_naomi_comparison <- function(art, anc, shape, pjnz){

  null_df <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
    c("indicator", "area_name", "year", "group","value_spectrum", "value_naomi", "difference"))

  if(is.null(art) & is.null(anc) ){

    # Empty data frame if no programme data
    comparison_df <- null_df

  } else {

    ## Check if shape is object or file path
    if(!inherits(shape, "sf")) {
      shape <- read_area_merged(shape) }

    ## PJNZ either object or file path
    if (!inherits(pjnz, "spec_program_data")) {
      pjnz <- extract_pjnz_program_data(pjnz) }

    # Create ART comparison or empty data frame if no ART supplied
    if (!is.null(art)) {
      art_comparison <- prepare_art_spectrum_comparison(art, shape, pjnz)
    } else {
      art_comparison <- null_df
    }

    # Create ANC comparison or empty data frame if no ART supplied
    if (!is.null(anc)) {
      anc_comparison <- prepare_anc_spectrum_comparison(anc, shape, pjnz)
    } else {
      anc_comparison <- null_df
    }

    comparison_df <- rbind(art_comparison, anc_comparison)
  }

  comparison_df
}
