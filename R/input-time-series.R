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
aggregate_art <- function(art, shape, drop_geometry = TRUE) {

  ## Check if shape is object or file path
  if (drop_geometry) {
    if(!inherits(shape, "sf")) {
      areas <- sf::read_sf(shape) |> sf::st_drop_geometry()
    } else {
      areas <- shape |> sf::st_drop_geometry()
    }
  } else {
    areas <- shape
  }

  ## Check if art is object or file path
  if (!inherits(art, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    art <- read_art_number(art, all_columns = TRUE)
  }

  # Aggregate based on what columns exist in dataset
  cols_list <- c("art_current", "art_new", "vl_tested_12mos", "vl_suppressed_12mos")
  cols_keep <- intersect(cols_list, colnames(art))

  clean_art <- art |>
    dplyr::select(area_id, sex, age_group, calendar_quarter, dplyr::any_of(cols_list))

  all_strat_art <- clean_art |>
    dplyr::group_by(calendar_quarter, age_group) |>
    dplyr::reframe(
      tidyr::expand_grid(
        age_group = dplyr::first(age_group),
        sex = unique(dplyr::cur_data()$sex)
      )
    ) |>
    dplyr::ungroup()

  quarter_by_area_level <- clean_art |>
    dplyr::left_join(dplyr::select(areas, area_id, area_level), by = "area_id") |>
    dplyr::group_by(calendar_quarter) |>
    dplyr::summarise(area_level = max(area_level), .groups = "drop")

  agg_art <- all_strat_art |>
    dplyr::left_join(quarter_by_area_level, by = "calendar_quarter") |>
    dplyr::left_join(dplyr::select(areas, area_id, area_level), by = "area_level") |>
    dplyr::left_join(clean_art, by = c("area_id", "calendar_quarter", "age_group", "sex"))

  area_with_parent_ids <- areas |>
    dplyr::select(area_id, parent_area_id)

  max_area_level <- max(agg_art$area_level)

  for (level in max_area_level:1) {
    agg_art <- agg_art |>
      dplyr::filter(area_level == level) |>
      dplyr::left_join(area_with_parent_ids, by = "area_id") |>
      dplyr::group_by(parent_area_id, calendar_quarter, sex, age_group) |>
      dplyr::summarise(
        calendar_quarter = dplyr::first(calendar_quarter),
        age_group = dplyr::first(age_group),
        sex = dplyr::first(sex),
        area_level = dplyr::first(area_level) - 1,
        dplyr::across(dplyr::any_of(cols_list), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::rename(area_id = parent_area_id) |>
      dplyr::bind_rows(agg_art)
  }  

  art_long <- agg_art |>
    dplyr::left_join(
      areas |> dplyr::select(
        area_id, area_name, area_level_label, parent_area_id, area_sort_order
      ), by = "area_id"
    ) |>
    dplyr::mutate(year = calendar_quarter_to_year(calendar_quarter),
                  quarter = calendar_quarter_to_quarter(calendar_quarter),
                  time_period = paste0(year, " ", quarter)) |>
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
  art_long <- aggregate_art(art, areas, drop_geometry = FALSE)
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
  art_level <- max(art_plot_data_long$area_level)

  # initialise missing_map with values that are missing, these will only show up
  # at the max admin level per calendar_quarter
  missing_map <- art_plot_data_long |>
    dplyr::select(area_id, calendar_quarter, value, plot, area_level) |>
    # find NAs, also check it isn't a NaN because these can appear in some
    # derived columns where we divide by 0
    dplyr::filter(is.na(value) & !is.nan(value)) |>
    dplyr::select(-value) |>
    dplyr::mutate(missing_ids = as.list(area_id))
  
  area_with_parent_ids <- areas |>
    dplyr::select(area_id, parent_area_id)

  # same idea as in aggregate_art, every iteration of the loop aggregates
  # up one admin level
  for (level in art_level:1) {
    missing_map <- missing_map |>
      dplyr::filter(area_level == level) |>
      dplyr::left_join(area_with_parent_ids, by = "area_id") |>
      dplyr::group_by(parent_area_id, calendar_quarter, plot) |>
      # > missing_ids merge the two lists together
      # > area_level decrease area_level by one because we aggregate
      #   up to the parent
      dplyr::summarise(
        missing_ids = list(unlist(missing_ids, FALSE, FALSE)),
        area_level = dplyr::first(area_level) - 1,
        .groups = "drop"
      ) |>
      dplyr::rename(area_id = parent_area_id) |>
      dplyr::bind_rows(missing_map)
  }

  df_final <- art_plot_data_long |>
    dplyr::left_join(missing_map, by = dplyr::join_by(area_id, calendar_quarter, plot, area_level)) |>
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
##' @param drop_geometry Setting this to FALSE will skip dropping geometry on
##'   the shape file (default TRUE)
##'
##' @return Aggregated ANC data containing columns area_id, area_name, area_level,
##' area_level_label, sex,age_group, time_period, year, quarter, calendar_quarter,
##' anc_clients, anc_known_neg, anc_already_art, anc_tested and anc_tested_pos,
##' births_clients_ratio

##' @export
aggregate_anc <- function(anc, shape, drop_geometry = TRUE) {

  ## Recursively aggregate ANC data up from lowest level of programm data provided
  # Level to aggregate from

  ## Check if shape is object or file path
  if (drop_geometry) {
    if(!inherits(shape, "sf")) {
      areas <- sf::read_sf(shape) |> sf::st_drop_geometry()
    } else {
      areas <- shape |> sf::st_drop_geometry()
    }
  } else {
    areas <- shape
  }

  ## Check if anc is object or file path
  if(!inherits(anc, c("spec_tbl_df","tbl_df","tbl","data.frame" ))) {
    anc <- read_anc_testing(anc)
  }

  # Aggregate based on what columns exist in dataset
  cols_list <- c("anc_clients", "anc_known_pos", "anc_already_art",
                 "anc_tested", "anc_tested_pos", "anc_known_neg", "births_facility")
  cols_keep <- intersect(cols_list, colnames(anc))

  # make sure the ANC data is the correct shape, note that all the
  # variables in cols_list are requried expect births_facility
  clean_anc <- anc |>
    dplyr::select(area_id, age_group, year, dplyr::any_of(cols_list))

  # initialise aggregated anc with the max admin level (most fine grained)
  # and fill in and missing rows - if a year has max admin level n (these can be
  # different per year) then we fill in values for any missing areas at admin
  # level n with NAs
  agg_anc <- clean_anc |>
    dplyr::left_join(dplyr::select(areas, area_id, area_level), by = "area_id") |>
    dplyr::group_by(year) |>
    # summarise to table with columns year, area_level (max area level for this year)
    # and age_group
    dplyr::summarize(
      area_level = dplyr::first(area_level),
      age_group = dplyr::first(age_group)
    ) |>
    # expand each year row to multiple rows with all area_ids for that admin level
    # this is the complete list of area_ids that we need
    dplyr::left_join(
      areas |> dplyr::select(area_id, area_level), by = "area_level"
    ) |>
    # left join complete list of area_ids with our potentially missing area_ids in
    # clean_anc to get rows of NAs if an area_id is missing
    dplyr::left_join(clean_anc, by = c("year", "area_id", "age_group")) |>
    dplyr::select(area_id, age_group, year, area_level, dplyr::any_of(cols_list))

  area_with_parent_ids <- areas |>
    dplyr::select(area_id, parent_area_id)

  max_area_level <- max(agg_anc$area_level)

  # every iteration of the loop aggregates the area up one area_level and
  # adds all those rows to agg_anc
  for (level in max_area_level:1) {
    agg_anc <- agg_anc |>
      dplyr::filter(area_level == level) |>
      dplyr::left_join(area_with_parent_ids, by = "area_id") |>
      dplyr::group_by(parent_area_id, year) |>
      # > year and age_group assumed to be the same as we aggregate up so
      #   just take the first value
      # > area_level is one less since we are aggregating to the parent
      # > sum across all the other numeric values ignoring NAs
      dplyr::summarise(
        year = dplyr::first(year),
        age_group = dplyr::first(age_group),
        area_level = dplyr::first(area_level) - 1,
        dplyr::across(dplyr::any_of(cols_list), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::rename(area_id = parent_area_id) |>
      dplyr::bind_rows(agg_anc)
  }

  # add in extra columns and sort
  anc_long <- agg_anc |>
    dplyr::left_join(
      areas |> dplyr::select(
        area_id, area_name, area_level_label, area_sort_order, parent_area_id
      ),
      by = "area_id"
    ) |>
    dplyr::mutate(time_period = as.character(year), quarter = "Q4", sex = "female",
                  calendar_quarter = paste0("CY", time_period, quarter)) |>
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
  anc_long <- aggregate_anc(anc, areas, drop_geometry = FALSE)

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

  # initialise missing_map with values that are missing, these will only show up
  # at the max admin level per year
  missing_map <- anc_plot_data_long |>
    dplyr::select(area_id, year, value, plot, area_level) |>
    # find NAs, also check it isn't a NaN because these can appear in some
    # derived columns where we divide by 0
    dplyr::filter(is.na(value) & !is.nan(value)) |>
    dplyr::select(-value) |>
    dplyr::mutate(missing_ids = as.list(area_id))
  
  area_with_parent_ids <- areas |>
    dplyr::select(area_id, parent_area_id)

  # same idea as in aggregate_anc, every iteration of the loop aggregates
  # up one admin level
  for (level in anc_level:1) {
    missing_map <- missing_map |>
      dplyr::filter(area_level == level) |>
      dplyr::left_join(area_with_parent_ids, by = "area_id") |>
      dplyr::group_by(parent_area_id, year, plot) |>
      # > missing_ids merge the two lists together
      # > area_level decrease area_level by one because we aggregate
      #   up to the parent
      dplyr::summarise(
        missing_ids = list(unlist(missing_ids, FALSE, FALSE)),
        area_level = dplyr::first(area_level) - 1,
        .groups = "drop"
      ) |>
      dplyr::rename(area_id = parent_area_id) |>
      dplyr::bind_rows(missing_map)
  }

  df_final <- anc_plot_data_long |>
    dplyr::left_join(missing_map, by = dplyr::join_by(area_id, year, plot, area_level)) |>
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
