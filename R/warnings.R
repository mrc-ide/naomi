naomi_warning <- function(text, locations) {
  ## If you want to add a new location here then let one of the developers know
  ## we will need to make sure the front end knows where to display the
  ## new location
  match_values(locations, c("review_inputs", "model_options", "model_fit",
                            "model_calibrate", "review_output",
                            "download_results"))
  warn <- list(
    text = text,
    locations = locations
  )
  signalCondition(structure(
    warn,
    class = c("naomi_warning", "condition")))
}

handle_naomi_warnings <- function(expr) {
  naomi_warnings <- new.env(parent = emptyenv())
  naomi_warnings$warnings <- list()
  out <- withCallingHandlers(
    force(expr),
    naomi_warning = function(e) {
      naomi_warnings$warnings <- c(naomi_warnings$warnings, list(e))
    }
  )
  out$warnings <- naomi_warnings$warnings
  out
}

##' Generate naomi warning for specific strata of simulated outputs
##'
##'
##' @param naomi_output
##' @param ind Naomi output indicator
##' @param threshold Numerical threshold to trigger warning
##' @param locations Location where warning should be displayed in front end

output_naomi_warning <- function(naomi_output, ind, threshold, locations) {


  val <- naomi_output$indicators$mean[naomi_output$indicators$indicator == ind]

  if(max(val) > threshold) {

    to_upper_first <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }

    v <- naomi_output$indicators %>%
      dplyr::filter(indicator == ind, mean > threshold) %>%
      dplyr::left_join(naomi_output$meta_age_group, by = "age_group") %>%
      dplyr::left_join(naomi_output$meta_area %>% sf::st_drop_geometry(), by = "area_id") %>%
      dplyr::mutate(calendar_quarter = calendar_quarter_labels(calendar_quarter),
                    sex = to_upper_first(sex)) %>%
      dplyr::mutate(disag = paste(calendar_quarter, area_name, sex, age_group_label, sep = ", "))

    if (ind == "prevalence") {
      key <- "WARNING_OUTPUTS_PREV_EXCEEDS_THRESHOLD"
    } else if (ind == "art_coverage") {
      key <- "WARNING_OUTPUTS_ARTCOV_EXCEEDS_THRESHOLD"
    } else {
      stop("Invalid indicator, can only return warning for prevalence or art_coverage") ## or some generic warning here?
    }

    msg <- t_(key, list(rows = paste0(v$disag, collapse = "; ")))

    naomi_warning(msg, locations)
  }

}

##' Compare aggregated district ART inputs + spectrum totals
##'
##' Generate warning if aggregated district art totals do not match spectrum totals
##'
##' @param art Path to file containing ART data or ART data object
##' @param shape Path to file containing geojson areas data or areas data object
##' @param pjnz Path to zip file containing spectrum pjnz file/s
##' @export
art_spectrum_warning <- function(art, shape, pjnz) {

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()
  } else {
    areas <- shape %>% sf::st_drop_geometry()
  }

  ## Check if art is object or file path
  if(!inherits(art, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    art_number <- read_art_number(art, all_columns = TRUE)
  }

  ## Read in spectrum programme data
  spec_program_data <- extract_pjnz_program_data(pjnz)

  ## Check if art totals match spectrum totals
  art_merged <- art_number %>%
      dplyr::left_join(
        dplyr::select(areas, area_id, area_name, spectrum_region_code),
        by = "area_id"
      ) %>%
      dplyr::count(spectrum_region_code, sex, age_group, area_name, calendar_quarter,
                   wt = art_current, name = "value_naomi") %>%
      dplyr::inner_join(
        spec_program_data$art_dec31 %>%
          dplyr::mutate(
            calendar_quarter = paste0("CY", year, "Q4")
          ),
        by = c("spectrum_region_code", "sex", "age_group", "calendar_quarter")
      ) %>%
    dplyr::rename(value_spectrum = art_dec31)

  format_spectrum_total_warning(data_merged = art_merged,
                                key = "WARNING_ART_NOT_EQUAL_TO_SPECTRUM",
                                location = "review_inputs",
                                age_disag = TRUE)

  }

##' Compare aggregated district ANC inputs + spectrum totals
##'
##' Generate warning if aggregated district art totals do not match spectrum totals
##'
##' @param anc Path to file containing ANC data or ANC data object
##' @param shape Path to file containing geojson areas data or areas data object
##' @param pjnz Path to zip file containing spectrum pjnz file/s
##' @export
anc_spectrum_warning <- function(anc, shape, pjnz) {
  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()
  } else {
    areas <- shape %>% sf::st_drop_geometry()
  }

  ## Check if anc is object or file path
  if(!inherits(anc, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    anc_testing <- read_anc_testing(anc)
  }

  ## Read in spectrum programme data
  spec_program_data <- extract_pjnz_program_data(pjnz)

  ## Check if art totals match spectrum totals
  anc_merged <- anc_testing %>%
    dplyr::left_join(
      dplyr::select(areas, area_id,area_name, spectrum_region_code),
      by = "area_id"
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("anc"),
                        names_to = "indicator",
                        values_to = "value_naomi") %>%
    dplyr::count(spectrum_region_code, age_group, area_name, year, indicator,
                 wt = value_naomi, name = "value_naomi") %>%
    dplyr::inner_join(
      spec_program_data$anc_testing %>%
        dplyr::rename("value_spectrum" = "value"),
      by = c("spectrum_region_code", "indicator", "year")
    ) %>%
    dplyr::mutate( sex = "female")

  anc_tested <- anc_merged[anc_merged$indicator == "anc_tested",]

  anc_tested_pos <- anc_merged[anc_merged$indicator == "anc_tested_pos",]

  # Generate warning if totals are not aligned
  format_spectrum_total_warning(data_merged = anc_tested,
                                key = "WARNING_ANC_TEST_NOT_EQUAL_TO_SPECTRUM",
                                location = "review_inputs")

  format_spectrum_total_warning(
    data_merged = anc_tested_pos,
    key = "WARNING_ANC_TEST_POS_NOT_EQUAL_TO_SPECTRUM",
    location = "review_inputs")
}

##' Format spectrum total warnings
##'
##' Format warnings with translated key to be displayed when spectrum totals
##' do not match aggregated district level programme data
##'
##' @param data_merged Aggregated program data merged with spectrum totals
##' @param key Translation key for warnings
##' @param location Location where warning should be displayed
##' @param age_disag Logical if age should be included in warning labels. Default is FALSE.
##' @keywords internal
format_spectrum_total_warning <- function(data_merged, key, location,
                                          age_disag = FALSE) {

  df <- data_merged %>%
    dplyr::filter(!(value_naomi == value_spectrum)) %>%
    dplyr::arrange(dplyr::desc(year))
  if(nrow(df) > 0) {
    if (age_disag) {
      df$label <- paste(df$year, df$age_group, df$area_name,
            "naomi:", df$value_naomi,
            "spectrum:", df$value_spectrum,
            sep = " ")
    } else {
      df$label <- paste(df$year, df$area_name,
            "naomi:", df$value_naomi,
            "spectrum:", df$value_spectrum,
            sep = " ")
    }

    n_regions <- nrow(df)
    if (n_regions > 3) {
      warning <- paste0(df[1:3, "label"], collapse = "; ")
      warning <- paste(warning,
                       t_("WARNING_ADDITIONAL", list(count = n_regions - 3)))
    } else {
      warning <- paste0(df$label, collapse = "; ")
    }
    msg <- paste0(t_(key), list(warnings = warning))

    naomi_warning(msg, location)
  }
}




