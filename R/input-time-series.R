##' Aggregate ART data according to area hierarchy
##'
##' Take ART and shape file paths or files and aggregate
##' art_current according to area hierarchy provided
##'
##' @param art Path to file containing ART data or ART data object
##' @param shape Path to file containing geojson areas data or areas data object
##'
##' @return Aggregated ART data containing columns area_id, area_name,
##' area_level, area_level_label, parent_area_id, sex, age_group, time_period,
##'  year, quarter,calendar_quarter and art_current
##'
##' @export

aggregate_art <- function(art, shape, pjnz = NULL) {


  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()
  } else {
    areas <- shape %>% sf::st_drop_geometry()
  }

  ## Check if art is object or file path
  if(!inherits(art, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    art <- read_art_number(art, all_column = TRUE)
  }

  ## Check if art totals match spectrum totals
  if(!is.null(pjnz)) {

    art_merged <- art %>%
      dplyr::left_join(
        dplyr::select(shape, area_id, spectrum_region_code),
        by = "area_id"
      ) %>%
      dplyr::count(spectrum_region_code, sex, age_group, calendar_quarter,
                   wt = art_current, name = "art_current_naomi") %>%
      dplyr::inner_join(
        spec_program_data$art_dec31 %>%
          dplyr::mutate(
            calendar_quarter = paste0("CY", year, "Q4"),
            year = NULL
          ),
        by = c("spectrum_region_code", "sex", "age_group", "calendar_quarter")
      )

    art_number_spectrum_aligned <- all(art_merged$art_current_naomi == art_merged$art_dec31)

    if(!(art_number_spectrum_aligned)) {
        v <- art_merged %>%
        filter(!(art_current_naomi %in% unique(art_dec31))) %>%
          mutate(id = paste(calendar_quarter,
                            "naomi:", art_current_naomi,
                            "spectrum:",art_dec31,
                            sep = " "))

        key <- "WARNING_ART_INPUTS_NOT_EQUAL_TO_SPECTRUM"

        msg <- t_(key, list(rows = paste0(v$id, collapse = "; ")))

        naomi_warning(msg, "model_fit")

    }

  }

  art_number <- art %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_level), by = "area_id") %>%
    dplyr::mutate(year = year_labels(calendar_quarter_to_quarter_id(calendar_quarter)),
                  quarter = stringr::str_sub(calendar_quarter,-2,-1),
                  time_period = paste0(year, " ", quarter))

  ## Recursively aggregate ART data up from lowest level of programme data provided
  # Levels to aggregate up from
  art_level <- max(art_number$area_level)
  sex_level <- unique(art_number$sex)
  age_level <- unique(art_number$age_group)
  # Join ART data to hierarchy
  art_number_wide <- spread_areas(areas %>% dplyr::filter(area_level <= art_level)) %>%
    dplyr::left_join(art_number, by = "area_id")


  # Aggregate based on what columns exist in dataset
  cols_list <- c("art_current", "art_new", "vl_tested_12mos", "vl_suppressed_12mos")
  cols_keep <- intersect(cols_list, colnames(art))

  # Function to aggregate based on area_id[0-9]$ columns in hierarchy
  aggregate_data_art <- function(col_name) {
    df <- art_number_wide %>%
      dplyr::group_by(eval(as.name(col_name)), sex, age_group, time_period,
                      year, quarter, calendar_quarter) %>%
      dplyr::summarise_at(dplyr::vars(cols_keep), ~sum(.)) %>%
      dplyr::rename(area_id = `eval(as.name(col_name))`)
  }

  # Aggregated data frame
  art_long <- grep("^area_id*\\s*[0-9]$", colnames(art_number_wide), value = TRUE) %>%
    lapply(function(x) aggregate_data_art(x))  %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_name, area_level,
                                             area_level_label, parent_area_id,
                                             area_sort_order),
                     by = "area_id" ) %>%
    dplyr::select(area_id, area_name, area_level, area_level_label,parent_area_id,
                  area_sort_order, sex, age_group,time_period, year, quarter,
                  calendar_quarter, dplyr::everything())

  return(art_long)
}


##' Prepare data for ART input time series plots
##'
##' Take uploaded ART and shape file paths and format as data which
##' can be used to draw input time series graphs.
##'
##' @param art Path to file containing ART data or ART data object
##' @param shape Path to file containing geojson areas data or area data object
##'
##' @return Data formatted for plotting input time series containing columns
##' area_id, area_name, area_level, area_level_label, time_period, year,
##' quarter, plot and value
##'
##' @export
prepare_input_time_series_art <- function(art, shape) {

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()
  } else {
    areas <- shape %>% sf::st_drop_geometry()
  }

  ## Check if art is object or file path
  if(!inherits(art, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    art <- read_art_number(art, all_column = TRUE)
  }


  ## Recursively aggregate ART data up from lowest level of programme data provided
  # Levels to aggregate up from
  art_long <- aggregate_art(art, shape)
  sex_level <- unique(art_long$sex)
  age_level <- unique(art_long$age_group)

  ## Shape data for plot
  art_plot_data <- art_long %>%
    dplyr::group_by(area_id, area_name, area_level, area_level_label,parent_area_id,
                    area_sort_order,time_period, year, quarter, calendar_quarter) %>%
    dplyr::summarise(
      art_total = sum(art_current, na.rm = TRUE),
      art_adult = sum(art_current * as.integer(age_group == "Y015_999"), na.rm = TRUE),
      art_adult_f = sum(art_current * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
      art_adult_m = sum(art_current * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
      art_child = sum(art_current * as.integer(age_group == "Y000_014"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      art_adult_sex_ratio = art_adult_f / art_adult_m,
      art_adult_child_ratio = art_adult / art_child,
      art_prop_u15 = round(art_child/(art_adult+art_child),3),
    )

  # if art_new column exists in art data, calculate variables
  if(any(grep("art_new", colnames(art_long)))) {

    art_new_data <- art_long %>%
      dplyr::group_by(area_id, area_name, area_level, area_level_label,parent_area_id,
                      area_sort_order,time_period, year, quarter, calendar_quarter) %>%
      dplyr::summarise(
        art_new_total = sum(art_new, na.rm = TRUE),
        art_new_adult = sum(art_new * as.integer(age_group == "Y015_999"), na.rm = TRUE),
        art_new_adult_f = sum(art_new * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
        art_new_adult_m = sum(art_new * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
        art_new_child = sum(art_new * as.integer(age_group == "Y000_014"), na.rm = TRUE),
        .groups = "drop"
      )

    art_plot_data <- dplyr::left_join(art_plot_data, art_new_data,
                               by = c("area_id", "area_name", "area_level",
                                      "area_level_label", "parent_area_id",
                                      "area_sort_order", "time_period",
                                      "year", "quarter", "calendar_quarter"))
  }

  # if VL columns exist in art data, calculate variables
  if(any(grep("vl", colnames(art_long)))) {

    vl_data <- art_long %>%
      dplyr::group_by(area_id, area_name, area_level, area_level_label,parent_area_id,
                      area_sort_order,time_period, year, quarter, calendar_quarter) %>%
      dplyr::summarise(
        vl_tested_12mos_total = sum(vl_tested_12mos, na.rm = TRUE),
        vl_tested_12mos_adult = sum(vl_tested_12mos * as.integer(age_group == "Y015_999"), na.rm = TRUE),
        vl_tested_12mos_adult_f = sum(vl_tested_12mos * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
        vl_tested_12mos_adult_m = sum(vl_tested_12mos * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
        vl_tested_12mos_child = sum(vl_tested_12mos * as.integer(age_group == "Y000_014"), na.rm = TRUE),
        vl_suppressed_12mos_total = sum(vl_suppressed_12mos, na.rm = TRUE),
        vl_suppressed_12mos_adult = sum(vl_suppressed_12mos * as.integer(age_group == "Y015_999"), na.rm = TRUE),
        vl_suppressed_12mos_adult_f = sum(vl_suppressed_12mos * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
        vl_suppressed_12mos_adult_m = sum(vl_suppressed_12mos * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
        vl_suppressed_12mos_child = sum(vl_suppressed_12mos * as.integer(age_group == "Y000_014"), na.rm = TRUE),
        .groups = "drop")

    art_plot_data <- dplyr::left_join(art_plot_data, vl_data,
                               by = c("area_id", "area_name", "area_level",
                                      "area_level_label", "parent_area_id",
                                      "area_sort_order", "time_period",
                                      "year", "quarter", "calendar_quarter")) %>%
      dplyr::mutate(
        vl_coverage_total = vl_tested_12mos_total/ art_total,
        vl_coverage_adult = vl_tested_12mos_adult / art_adult,
        vl_coverage_adult_f = vl_tested_12mos_adult_f/ art_adult_f,
        vl_coverage_adult_m = vl_tested_12mos_adult_m/ art_adult_m,
        vl_coverage_child = vl_tested_12mos_child/ art_child,

        vl_prop_suppressed_total = vl_suppressed_12mos_total/ vl_tested_12mos_total,
        vl_prop_suppressed_adult = vl_suppressed_12mos_adult/ vl_tested_12mos_adult,
        vl_prop_suppressed_adult_f = vl_suppressed_12mos_adult_f/ vl_tested_12mos_adult_f,
        vl_prop_suppressed_adult_m = vl_suppressed_12mos_adult_m/ vl_tested_12mos_adult_m,
        vl_prop_suppressed_child = vl_suppressed_12mos_child/ vl_tested_12mos_child)

  }

  art_plot_data_long <- art_plot_data %>%
    tidyr::pivot_longer(cols = !c(area_id, area_name, area_level, area_level_label,
                                  parent_area_id, area_sort_order, time_period,
                                  year, quarter, calendar_quarter),
                        names_to = "plot",
                        values_to = "value") %>%
    dplyr::mutate_at(dplyr::vars(value), ~replace(., is.nan(.), 0))

# Remove sex disaggregated variables if only sex aggregated data is present
  if(all(!c("male", "female") %in% sex_level)) {
    art_plot_data_long <-  dplyr::filter(art_plot_data_long,
                                    !(plot %in% c("art_adult_f","art_adult_m", "art_adult_sex_ratio",
                                                  "art_new_adult_f", "art_new_adult_m",
                                                  "vl_tested_12mos_adult_f", "vl_tested_12mos_adult_m",
                                                  "vl_suppressed_12mos_adult_f", "vl_suppressed_12mos_adult_m",
                                                  "vl_coverage_adult_f", "vl_coverage_adult_m",
                                                  "vl_prop_suppressed_adult_f", "vl_prop_suppressed_adult_m")))
  }

  # Remove age disaggregated variables if paeds data is not present
  if(!("Y000_014" %in% age_level)) {
    art_plot_data_long <-  dplyr::filter(art_plot_data_long,
                                    !(plot %in% c("art_child","art_adult_child_ratio", "art_prop_u15",
                                                  "art_new_child","vl_tested_12mos_child",
                                                  "vl_suppressed_12mos_child","vl_coverage_child",
                                                  "vl_prop_suppressed_child")))
  }
  art_plot_data_long <- art_plot_data_long %>%
    dplyr::arrange(area_sort_order, calendar_quarter)
  return(art_plot_data_long)
}


##' Aggregate ANC data according to area hierarchy
##'
##' Take ANC and shape file paths or data files and aggregate
##' art_current according to area hierarchy provided
##'
##' @param anc Path to file containing ANC data or ANC data object
##' @param shape Path to file containing geojson areas data or areas data object
##'
##' @return Aggregated ANC data containing columns area_id, area_name, area_level,
##' area_level_label, sex,age_group, time_period, year, quarter, calendar_quarter,
##' anc_clients, anc_known_pos, anc_already_art, anc_tested and anc_tested_pos

##' @export

aggregate_anc <- function(anc, shape) {

  ## Recursively aggregate ANC data up from lowest level of programm data provided
  # Level to aggregate from


  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()
  }

  ## Check if anc is object or file path
  if(!inherits(anc, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    anc <- read_anc_testing(anc)
  }

  anc_testing <- anc %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_level), by = "area_id") %>%
    dplyr::mutate(time_period = as.character(year),
                  quarter = "Q4",
                  calendar_quarter = paste0("CY", time_period, quarter),
                  sex = "female")

  ## Recursively aggregate ART data up from lowest level of programme data provided
  # Level to aggregate from
  anc_level <- levels(as.factor(anc_testing$area_level))
  # Join ART data to hierarchy
  anc_testing_wide <- dplyr::left_join(
    anc_testing,
    spread_areas(areas %>% dplyr::filter(area_level <= anc_level)),
    by = "area_id")

  # Function to aggregate based on area_id[0-9]$ columns in hierarchy
  aggregate_data_anc <- function(col_name) {
    df <- anc_testing_wide %>%
      dplyr::group_by(eval(as.name(col_name)), sex, age_group, time_period,
                      year, quarter, calendar_quarter) %>%
      dplyr::summarise(anc_clients = sum(anc_clients, na.rm = TRUE),
                       anc_known_pos = sum(anc_known_pos, na.rm = TRUE),
                       anc_already_art = sum(anc_already_art, na.rm = TRUE),
                       anc_tested = sum(anc_tested, na.rm = TRUE),
                       anc_tested_pos = sum(anc_tested_pos, na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::rename(area_id = `eval(as.name(col_name))`)
  }

  # Aggregated data frame
  anc_long <- grep("^area_id*\\s*[0-9]$", colnames(anc_testing_wide), value = TRUE) %>%
    lapply(function(x) aggregate_data_anc(x))  %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(areas %>% dplyr::select(area_id,area_name, area_level,
                                             area_level_label, parent_area_id,
                                             area_sort_order), by = "area_id") %>%
    dplyr::select(area_id, area_name, area_level, area_level_label,parent_area_id,
                  area_sort_order, sex, age_group, time_period, year, quarter,
                  calendar_quarter, anc_clients, anc_known_pos, anc_already_art,
                  anc_tested,anc_tested_pos)

}


##' Prepare data for ANC input time series plots
##'
##' Take uploaded ANC and shape file paths or objects and format as data which
##' can be used to draw input time series graphs.
##'
##' @param anc Path to file containing ANC data or ANC data object
##' @param shape Path to file containing geojson areas data or shape sf object
##'
##' @return Data formatted for plotting input ANC time series containing columns
##' area_id, area_name, area_level, area_level_label, age_group, time_period,
##' time_step, plot and value
##' @export
prepare_input_time_series_anc <- function(anc, shape) {


  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()
  }

  if(inherits(shape, "sf")) {
    areas <- shape %>% sf::st_drop_geometry()
  }

  ## Check if art is object or file path
  if(!inherits(anc, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    anc <- read_anc_testing(anc)
  }

  anc_long <- aggregate_anc(anc, shape)## Shape data for plot
  anc_plot_data <- anc_long %>%
    dplyr::mutate(
      anc_total_pos = anc_known_pos + anc_tested_pos,
      anc_status = anc_known_pos + anc_tested,
      anc_prevalence = anc_total_pos / anc_status,
      anc_art_among_known = anc_already_art / anc_known_pos,
      anc_art_coverage = anc_already_art / anc_total_pos
    ) %>%
    dplyr::select(area_id, area_name, area_level, area_level_label, parent_area_id,
                  area_sort_order, age_group,  time_period, year, quarter,
                  calendar_quarter, anc_clients, anc_tested, anc_tested_pos,
                  anc_prevalence, anc_known_pos, anc_art_coverage) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("anc"),
                        names_to = "plot",
                        values_to = "value") %>%
    dplyr::arrange(area_sort_order, calendar_quarter)
  anc_plot_data
}

##' Return the translated label & description for a set of plot types
##'
##' @param plot_type Plot type ids
##'
##' @return For each plot type the label and description as a list of lists
##'   containing id, label and description
##' @export
get_plot_type_column_metadata <- function(plot_type) {
  meta <- naomi_read_csv(
    system_file("metadata", "time_series_plot_metadata.csv"),
    col_types = readr::cols(.default = "c"))
  meta <- meta[meta$id %in% plot_type, ]

  meta$label <- traduire::translator()$replace(meta$label)
  meta$description <- traduire::translator()$replace(meta$description)

  ## Remove a single leading or trailing "
  ## We quote to avoid excel changing these to e.g. to replace 0.0 with 0
  meta$format <- sub("^\"", "", meta$format)
  meta$format <- sub("\"$", "", meta$format)

  ## Convert numeric columns to numbers
  meta$accuracy <- as.numeric(meta$accuracy)
  lapply(seq_len(nrow(meta)), function(row_number) {
    row <- meta[row_number, ]
    list(
      id = row$id,
      label = row$label,
      description = row$description,
      format = row$format,
      accuracy = row$accuracy
    )
  })
}
