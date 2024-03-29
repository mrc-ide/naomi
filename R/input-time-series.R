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
aggregate_art <- function(art, shape) {

  ## Check if shape is object or file path
  if (!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) |> sf::st_drop_geometry()
  } else {
    areas <- shape |> sf::st_drop_geometry()
  }

  ## Check if art is object or file path
  if (!inherits(art, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    art <- read_art_number(art, all_columns = TRUE)
  }

  # Aggregate based on what columns exist in dataset
  cols_list <- c("art_current", "art_new", "vl_tested_12mos", "vl_suppressed_12mos")
  cols_keep <- intersect(cols_list, colnames(art))

  art <- art |>
    dplyr::select(area_id, sex, age_group, calendar_quarter, dplyr::any_of(cols_list))

  art_number <- art |>
    dplyr::left_join(areas |> dplyr::select(area_id, area_level), by = "area_id")

  aggregate_art_by_level <- function(art_number, level) {

    ## Recursively aggregate ART data up from lowest level of programme data provided
    # Levels to aggregate up from
    max_dat <- dplyr::filter(art_number, area_level == level)
    max_shape <- dplyr::filter(areas, area_level == level)

    # Ensure entries exist for all programme data age/sex/quarter combinations X
    # shape file area_ids at finest stratification

    age_sex_df <- max_dat |>
      dplyr::group_by(sex, age_group, calendar_quarter) |>
      dplyr::summarise(.groups = "drop")

    art_full <- tidyr::crossing(area_id = unique(max_shape$area_id),
                                age_sex_df) |>
      dplyr::left_join(max_dat, by = c("area_id", "sex", "age_group", "calendar_quarter")) |>
      dplyr::mutate(area_level = level)


    art_number_wide <- spread_areas(areas |> dplyr::filter(area_level <= level)) |>
      dplyr::right_join(art_full, by = "area_id", multiple = "all")


    # Function to aggregate based on area_id[0-9]$ columns in hierarchy
    aggregate_data_art <- function(col_name) {

      max_col_name <- paste0("area_id", level)

      if (col_name == max_col_name) {
        # Don't aggregate lowest level of data to retain missing values
        df <- art_number_wide |> dplyr::select(area_id, sex, age_group, calendar_quarter,
                                                all_of(cols_keep))
      } else {

        df <- art_number_wide |>
          dplyr::group_by(!!col_name := .data[[col_name]], sex, age_group, calendar_quarter) |>
          dplyr::summarise_at(dplyr::vars(dplyr::all_of(cols_keep)), ~sum(., na.rm = TRUE),
                              .groups = "drop") |>
          dplyr::rename_with(~gsub("[0-9]$", "", .))

      }

    }


    # Aggregated data frame for area levels > data provided
    aggregate_cols <- grep("^area_id*\\s*[0-9]$", colnames(art_number_wide), value = TRUE)

    aggregate_cols |>
      lapply(function(x) aggregate_data_art(x)) |>
      dplyr::bind_rows() |>
      dplyr::ungroup() |>
      dplyr::bind_rows()
  }

  max_levels <- art_number |> dplyr::summarise(area_level = max(area_level),
                                              .by = "calendar_quarter")

  ## Note we can have the case here that different calendar quarters have
  ## a different max level e.g. Mozambique
  art_long <- lapply(unique(max_levels$area_level),
                     function(level) aggregate_art_by_level(art_number, level)) |>
    dplyr::bind_rows() |>
    dplyr::mutate(year = calendar_quarter_to_year(calendar_quarter),
                  quarter = calendar_quarter_to_quarter(calendar_quarter),
                  time_period = paste0(year, " ", quarter)) |>
    dplyr::left_join(
      areas |>
        dplyr::select(area_id, area_name, area_level,
                      area_level_label, parent_area_id, area_sort_order),
      by = c("area_id")
    ) |>
    dplyr::select(area_id, area_level, area_name, area_level_label,parent_area_id,
                  area_sort_order, sex, age_group,time_period, year, quarter,
                  calendar_quarter, dplyr::everything()) |>
    dplyr::arrange(year, area_sort_order)

  art_long$area_hierarchy <- build_hierarchy_label(art_long)
  art_long

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
##' area_id, area_name, area_level, area_level_label, parent_area_id, area_sort_order,
##' time_period, year, quarter, calendar_quarter, area_hierarchy, plot, value and
##' missing_ids
##'
##' @export
prepare_input_time_series_art <- function(art, shape) {

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) |> sf::st_drop_geometry()
  } else {
    areas <- shape |> sf::st_drop_geometry()
  }

  ## Recursively aggregate ART data up from lowest level of programme data provided
  # Levels to aggregate up from
  art_long <- aggregate_art(art, shape)
  sex_level <- unique(art_long$sex)
  age_level <- unique(art_long$age_group)
  admin_level <- max(art_long$area_level)

  ## Shape data for plot
  art_plot_data <- art_long |>
    dplyr::group_by(area_id, area_name, area_level, area_level_label,parent_area_id,
                    area_sort_order,time_period, year, quarter, calendar_quarter,area_hierarchy) |>
    dplyr::mutate(na_rm = area_level != admin_level) |>
    dplyr::summarise(
      art_total = sum(art_current,  na.rm = na_rm),
      art_adult = sum(art_current * as.integer(age_group == "Y015_999"),  na.rm = na_rm),
      art_adult_f = sum(art_current * as.integer(sex == "female" & age_group == "Y015_999"),  na.rm = na_rm),
      art_adult_m = sum(art_current * as.integer(sex == "male" & age_group == "Y015_999"),  na.rm = na_rm),
      art_child = sum(art_current * as.integer(age_group == "Y000_014"),  na.rm = na_rm),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      art_adult_sex_ratio = art_adult_f / art_adult_m,
      art_child_adult_ratio = art_child / art_adult
    )

  # if art_new column exists in art data, calculate variables
  if(any(grep("art_new", colnames(art_long)))) {

    art_new_data <- art_long |>
      dplyr::mutate(na_rm = area_level != admin_level) |>
      dplyr::group_by(area_id, area_name, area_level, area_level_label,parent_area_id,
                      area_sort_order,time_period, year, quarter, calendar_quarter, area_hierarchy) |>
      dplyr::summarise(
        art_new_total = sum(art_new, na.rm = na_rm),
        art_new_adult = sum(art_new * as.integer(age_group == "Y015_999"), na.rm = na_rm),
        art_new_adult_f = sum(art_new * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = na_rm),
        art_new_adult_m = sum(art_new * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = na_rm),
        art_new_child = sum(art_new * as.integer(age_group == "Y000_014"), na.rm = na_rm),
        .groups = "drop"
      )

    art_plot_data <- dplyr::left_join(art_plot_data, art_new_data,
                                      by = c("area_id", "area_name", "area_level",
                                             "area_level_label", "parent_area_id",
                                             "area_sort_order", "time_period",
                                             "year", "quarter", "calendar_quarter",
                                             "area_hierarchy"))
  }

  # if VL columns exist in art data, calculate variables
  if(any(grep("vl", colnames(art_long)))) {

    vl_data <- art_long |>
      dplyr::mutate(na_rm = area_level != admin_level) |>
      dplyr::group_by(area_id, area_name, area_level, area_level_label,parent_area_id,
                      area_sort_order,time_period, year, quarter, calendar_quarter, area_hierarchy) |>
      dplyr::summarise(
        vl_tested_12mos_total = sum(vl_tested_12mos, na.rm = na_rm),
        vl_tested_12mos_adult = sum(vl_tested_12mos * as.integer(age_group == "Y015_999"), na.rm = na_rm),
        vl_tested_12mos_adult_f = sum(vl_tested_12mos * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = na_rm),
        vl_tested_12mos_adult_m = sum(vl_tested_12mos * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = na_rm),
        vl_tested_12mos_child = sum(vl_tested_12mos * as.integer(age_group == "Y000_014"), na.rm = na_rm),
        vl_suppressed_12mos_total = sum(vl_suppressed_12mos, na.rm = na_rm),
        vl_suppressed_12mos_adult = sum(vl_suppressed_12mos * as.integer(age_group == "Y015_999"), na.rm = na_rm),
        vl_suppressed_12mos_adult_f = sum(vl_suppressed_12mos * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = na_rm),
        vl_suppressed_12mos_adult_m = sum(vl_suppressed_12mos * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = na_rm),
        vl_suppressed_12mos_child = sum(vl_suppressed_12mos * as.integer(age_group == "Y000_014"), na.rm = na_rm),
        .groups = "drop")

    art_plot_data <- dplyr::left_join(art_plot_data, vl_data,
                                      by = c("area_id", "area_name", "area_level",
                                             "area_level_label", "parent_area_id",
                                             "area_sort_order", "time_period",
                                             "year", "quarter", "calendar_quarter",
                                             "area_hierarchy")) |>
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

  art_plot_data_long <- art_plot_data |>
    tidyr::pivot_longer(cols = !c(area_id, area_name, area_level, area_level_label,
                                  parent_area_id, area_sort_order, time_period,
                                  year, quarter, calendar_quarter, area_hierarchy),
                        names_to = "plot",
                        values_to = "value") |>
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
                                         !(plot %in% c("art_child","art_child_adult_ratio",
                                                       "art_new_child","vl_tested_12mos_child",
                                                       "vl_suppressed_12mos_child","vl_coverage_child",
                                                       "vl_prop_suppressed_child")))
  }

  art_plot_data_long <- art_plot_data_long |>
    dplyr::arrange(area_sort_order, calendar_quarter)

  # Tag data with NAs at the lowest admin level
  area_level <- max(art_plot_data_long$area_level)

  hierarchy_wide <- spread_areas(areas |> dplyr::filter(area_level <= admin_level)) |>
    dplyr::select(dplyr::starts_with("area_id"))


  missing_map <- art_plot_data_long |>
    # For edge cases where data is provided at different admin levels for
    # different years (get max area level by year)
    dplyr::select(calendar_quarter, area_level) |>
    dplyr::group_by(calendar_quarter) |>
    dplyr::summarise(area_level = max(area_level)) |>
    # Select lowest admin level for each year
    dplyr::left_join(art_plot_data_long |>
                       dplyr::select(area_id, area_name, area_level, calendar_quarter, plot, value),
                     multiple = "all", by = dplyr::join_by(calendar_quarter, area_level)) |>
    # Joint to wide hierarchy
    dplyr::left_join(hierarchy_wide, by = dplyr::join_by(area_id)) |>
    # Filter for districts with missing value
    dplyr::filter(is.na(value)) |>
    dplyr::select(missing = area_id, dplyr::everything()) |>
    # Get into long format
    tidyr::pivot_longer(cols = dplyr::starts_with("area_id"), values_to = "area_id") |>
    # Aggregate missing observations by area_id at all levels
    dplyr::group_by(area_id, plot, calendar_quarter) |>
    dplyr::summarise(missing_ids = list(missing), .groups = "drop")


  df_final <- art_plot_data_long |>
    dplyr::left_join(missing_map, by = dplyr::join_by(area_id, calendar_quarter, plot)) |>
    dplyr::mutate(value = tidyr::replace_na(value, 0), missing = NULL)

  return(df_final)
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
##' anc_clients, anc_known_neg, anc_already_art, anc_tested and anc_tested_pos,
##' births_clients_ratio

##' @export
aggregate_anc <- function(anc, shape) {

  ## Recursively aggregate ANC data up from lowest level of programm data provided
  # Level to aggregate from

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) |> sf::st_drop_geometry()
  } else {
    areas <- shape |> sf::st_drop_geometry()
  }

  ## Check if anc is object or file path
  if(!inherits(anc, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    anc <- read_anc_testing(anc)
  }

  # Aggregate based on what columns exist in dataset
  cols_list <- c("anc_clients", "anc_known_pos", "anc_already_art",
                 "anc_tested", "anc_tested_pos", "anc_known_neg", "births_facility")
  cols_keep <- intersect(cols_list, colnames(anc))

  anc <- anc |>
    dplyr::select(area_id, age_group, year, dplyr::any_of(cols_list))

  anc_testing <- anc |>
    dplyr::left_join( dplyr::select(areas, area_id, area_level), by = "area_id")

  # Split data by year and aggregate from lowest level available
  anc_dat <- split(anc_testing , f = anc_testing$year)

  aggregate_anc_by_level <- function(anc_testing){

    ## Recursively aggregate ANC data up from lowest level of programme data provided
    # Level to aggregate from
    anc_level <- max(anc_testing$area_level)
    max_dat <- dplyr::filter(anc_testing, area_level == anc_level)
    max_shape <- dplyr::filter(areas, area_level == anc_level)


    # Ensure entries exist for all programme data age/sex/quarter combinations X
    # shape file area_ids at finest stratification
    anc_full <- tidyr::crossing(area_id = unique(max_shape$area_id),
                                age_group = unique(max_dat$age_group),
                                year = unique(max_dat$year)) |>
      dplyr::left_join(max_dat, by = c("area_id", "age_group", "year"))

    # Join ANC data to hierarchy
    anc_testing_wide <- areas |>
      dplyr::filter(area_level <= anc_level) |>
      spread_areas() |>
      dplyr::right_join(anc_full, by = "area_id", multiple = "all")


    # Function to aggregate based on area_id[0-9]$ columns in hierarchy
    aggregate_data_anc <- function(col_name) {

      max_admin <- paste0("area_id", anc_level)

      if(col_name == max_admin) {
        # Don't aggregate lowest level of data to retain missing values
        df <- anc_testing_wide |> dplyr::select(area_id, age_group, year,
                                                 all_of(cols_keep))
      } else {

        df <- anc_testing_wide |>
          dplyr::group_by(!!col_name := .data[[col_name]], age_group, year) |>
          dplyr::summarise_at(dplyr::vars(dplyr::all_of(cols_keep)), ~sum(., na.rm = TRUE),
                              .groups = "drop") |>
          dplyr::rename_with(~gsub("[0-9]$", "", .))
      }
    }

    # Aggregated data frame for area levels
    aggregate_cols <- grep("^area_id*\\s*[0-9]$", colnames(anc_testing_wide), value = TRUE)

    aggregated_anc <- aggregate_cols |>
      lapply(aggregate_data_anc) |>
      dplyr::bind_rows() |>
      dplyr::ungroup() |>
      dplyr::bind_rows()
  }

  anc_long <- lapply(anc_dat, aggregate_anc_by_level) |>
    dplyr::bind_rows() |>
    dplyr::mutate(time_period = as.character(year), quarter = "Q4", sex = "female",
                  calendar_quarter = paste0("CY", time_period, quarter)) |>
    dplyr::left_join(areas |>
                       dplyr::select(area_id, area_name, area_level,area_level_label,
                                     parent_area_id, area_sort_order),
                     by = "area_id") |>
    dplyr::select(area_id, area_name, area_level, area_level_label,parent_area_id,
                  area_sort_order, sex, age_group, time_period, year, quarter,
                  calendar_quarter, dplyr::all_of(cols_keep)) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, area_sort_order)

  anc_long$area_hierarchy <- build_hierarchy_label(anc_long)
  anc_long
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
##' area_id, area_name, area_level, area_level_label, parent_area_id, area_sort_order,
##' time_period, year, quarter, calendar_quarter, area_hierarchy, plot, value and
##' missing_ids
##' @export
prepare_input_time_series_anc <- function(anc, shape) {

  ## Check if shape is object or file path
  if(!inherits(shape, "sf")) {
    areas <- sf::read_sf(shape) |> sf::st_drop_geometry()
  } else {
    areas <- shape |> sf::st_drop_geometry()
  }

  ## Shape data for plot
  anc_long <- aggregate_anc(anc, shape)

  anc_plot_data_long <- anc_long |>
    dplyr::mutate(
      anc_total_pos = anc_known_pos + anc_tested_pos,
      anc_status = anc_known_pos + anc_tested + anc_known_neg,
      anc_prevalence = anc_total_pos / anc_status,
      anc_art_among_known = anc_already_art / anc_known_pos,
      anc_art_coverage = anc_already_art / anc_total_pos,
      births_clients_ratio = births_facility / anc_clients
      ) |>
    dplyr::select(area_id, area_name, area_level, area_level_label, parent_area_id,
                  area_sort_order, age_group,  time_period, year, quarter,
                  calendar_quarter, anc_clients, anc_tested, anc_tested_pos,
                  anc_prevalence, anc_known_pos, anc_known_neg,
                  anc_art_coverage, births_facility, births_clients_ratio,
                  area_hierarchy) |>
    tidyr::pivot_longer(cols = c(dplyr::starts_with("anc"), "births_facility", "births_clients_ratio"),
                        names_to = "plot",
                        values_to = "value") |>
    dplyr::arrange(area_sort_order, calendar_quarter)

  # Tag data with NAs at the lowest admin level
  anc_level <- max(anc_plot_data_long$area_level)
  hierarchy_wide <- spread_areas(areas |> dplyr::filter(area_level <= anc_level))

  missing_map <- anc_plot_data_long |>
    # For edge cases where data is provided at different admin levels for
    # different years (get max area level by year)
    dplyr::select(calendar_quarter, area_level) |>
    dplyr::group_by(calendar_quarter) |>
    dplyr::summarise(area_level = max(area_level)) |>
    # Select lowest admin level for each year
    dplyr::left_join(anc_plot_data_long |>
                       dplyr::select(area_id, area_name, area_level, calendar_quarter, plot, value),
                     multiple = "all", by = dplyr::join_by(calendar_quarter, area_level)) |>
    # Joint to wide hierarchy
    dplyr::left_join(hierarchy_wide, by = dplyr::join_by(area_id)) |>
    # Filter for districts with missing value
    dplyr::filter(is.na(value)) |>
    dplyr::select(missing = area_id, dplyr::everything()) |>
    # Get into long format
    tidyr::pivot_longer(cols = dplyr::starts_with("area_id"), values_to = "area_id") |>
    # Aggregate missing observations by area_id at all levels
    dplyr::group_by(area_id, plot, calendar_quarter) |>
    dplyr::summarise(missing_ids = list(missing), .groups = "drop")

  df_final <- anc_plot_data_long |>
    dplyr::left_join(missing_map, by = dplyr::join_by(area_id, calendar_quarter, plot)) |>
    dplyr::mutate(value = tidyr::replace_na(value, 0), missing = NULL)

  return(df_final)
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


##' Return the translated label & description for a set of plot types
##'
##' @param meta_areas dataframe containing
##'
##' @return For each plot type the label and description as a list of lists
##'   containing id, label and description
##' @export
build_hierarchy_label <- function(meta_areas) {

  area_ids <- dplyr::select(meta_areas, area_id, parent_area_id, area_name) |>
    dplyr::distinct() |>
    as.data.frame()

  seen <- new.env(parent = emptyenv())

  get_label <- function(area_id) {
    if (!is.null(seen[[area_id]])) {
      return(seen[[area_id]])
    }

    sub <- area_ids[area_ids$area_id == area_id,]
    parent_id <- unique(sub$parent_area_id)
    if (is.na(parent_id)) {
      return(NA_character_)
    }
    parent_label <- get_label(parent_id)

    if (!is.na(parent_label)) {
      parent_label <- paste0(parent_label, "/")
      label <- paste0(parent_label, sub$area_name)
    } else{
      label <- sub$area_name
    }
    seen[[area_id]] <- label
    label
  }

  labels <- vapply(meta_areas$area_id, get_label, character(1),
                   USE.NAMES = FALSE)

  ## We do not include top level country name in the hierarchy for brevity,
  ## but we want to include it where it is the country level region instead
  ## of just an empty label
  root_name <- area_ids[is.na(area_ids$parent_area_id), "area_name"]
  labels[is.na(labels)] <- root_name
  labels
}
