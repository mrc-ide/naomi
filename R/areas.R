#' Create an Areas Object
#'
#' Constructs and validates an areas object as an S3 class.
#'
#' @param levels Data frame of area level metadata.
#' @param hierarchy Data frame defining area hierarchy and area-level data.
#' @param boundaries an `sf` object with boundary geometry for each area_id
#' @param area_merged A merged version of `levels`, `hierarchy`, and `boundaries`.
#' @return An object of class `naomi_areas`
#'
#' @examples
#' data(demo_area_levels)
#' data(demo_area_hierarchy)
#' data(demo_area_boundaries)
#'
#' areas <- create_areas(demo_area_levels, demo_area_hierarchy, demo_area_boundaries)
#' areas
#'
#' @export
  create_areas <- function(levels = NULL, hierarchy = NULL, boundaries = NULL,
                         area_merged = NULL) {

  if(is.null(area_merged) &&
     (is.null(levels) ||
      is.null(hierarchy) ||
      is.null(boundaries))
     ) {
    stop(t_("AREAS_AREA_LEVEL_HIERARCHY_BOUNDARIES_MISSING"))
  }

  if(!is.null(area_merged)) {
    if(!rlang::has_name(area_merged, "center_x"))
      area_merged$center_x <- NA
    if(!rlang::has_name(area_merged, "center_y"))
      area_merged$center_y <- NA

    levels <- area_merged %>%
      as.data.frame() %>%
      dplyr::count(area_level, area_level_label, display, name = "n_areas")
    hierarchy <- area_merged %>%
      as.data.frame() %>%
      dplyr::select(area_id, area_name, area_level, parent_area_id, spectrum_region_code, area_sort_order, center_x, center_y)

    boundaries <- dplyr::select(area_merged, area_id, geometry)
  }

  if(!rlang::has_name(hierarchy, "center_x"))
    hierarchy$center_x <- NA
  if(!rlang::has_name(hierarchy, "center_y"))
    hierarchy$center_y <- NA
  missing_center <- is.na(hierarchy$center_x)
  if(any(missing_center)) {
    sf::st_agr(boundaries) <- "constant"
    px <- withCallingHandlers(
      sf::st_point_on_surface(boundaries),
      warning = function(w) {
        if(grepl("st_point_on_surface may not give correct results for longitude/latitude data", w$message))
          invokeRestart("muffleWarning")
      }
    )
    hierarchy$center_x[missing_center] <- sf::st_coordinates(px)[,1]
    hierarchy$center_y[missing_center] <- sf::st_coordinates(px)[,2]
  }

  ## Validate areas

  ## - Area levels are unique and sequential
  if(any(duplicated(levels$area_level))) {
    stop(
      t_("AREAS_LEVEL_NOT_UNIQUE", list(missing_levels =
             paste(unique(levels$area_level[duplicated(levels$area_level)]),
                   collapse = ",")))
    )
  }
  stopifnot(diff(levels$area_level) == 1)

  ## - Number of areas by level is non-decreasing
  stopifnot(diff(levels$n_areas) >= 0)

  ## - Levels are consistent with meta data
  if(any(!levels$area_level %in% levels$area_level)) {
    stop(
      paste("Metadata area_level not found in levels list:",
            paste(setdiff(levels$area_level, levels$area_level), collapse = ","))
    )
  }
  if(any(!levels$area_level %in% levels$area_level)) {
    stop(
      paste("Levels area_level not found in levels metadata:",
            paste(unique(setdiff(levels$area_level, levels$area_level),
                         collapse = ","))
            )
    )
  }

  ## - Number of areas at each level matches expected
  ## - Levels are consistent between metadata and levels
  stopifnot(
    hierarchy %>%
    dplyr::count(area_level) %>%
    dplyr::full_join(levels, by = "area_level") %>%
    dplyr::mutate(n_match = n == n_areas) %>%
    .$n_match
  )

  ## - Area IDs only appear once in each level
  stopifnot(
    hierarchy %>%
      dplyr::group_by(area_level) %>%
      dplyr::mutate(unique_area_id = !duplicated(area_id)) %>%
      .$unique_area_id
  )

  ## - All areas have a name, boundary, and center
  assertthat::assert_that(assertthat::noNA(hierarchy$area_name))
  assertthat::assert_that(assertthat::noNA(hierarchy$center_x))
  assertthat::assert_that(assertthat::noNA(hierarchy$center_y))
  assertthat::assert_that(assertthat::noNA(boundaries$geometry))


  ## TO DO: boundary checks
  ## - Areas are nested
  ## - Each level covers level above
  ## - Centroids like within geometry (maybe)
  ## ** These checks might be time consuming, perhaps make them optional


  tree <- hierarchy %>%
    dplyr::filter(!is.na(parent_area_id)) %>%
    dplyr::select(area_id, parent_area_id) %>%
    data.tree::FromDataFrameNetwork()

  hierarchy <- hierarchy %>%
    dplyr::left_join(
             data.frame(
               area_id = tree$Get("name", traversal = "level"),
               tree_idx = 1:tree$totalCount,
               stringsAsFactors = FALSE
             ),
             by = "area_id"
           ) %>%
    dplyr::arrange(tree_idx) %>%
    dplyr::left_join(levels, by = "area_level")

  tree$Set(area_id = hierarchy$area_id,
           area_level = hierarchy$area_level,
           area_level_label = hierarchy$area_level_label,
           display_level = hierarchy$display,
           spectrum_region_code = hierarchy$spectrum_region_code,
           area_name = hierarchy$area_name,
           area_sort_order = hierarchy$area_sort_order,
           center_x = hierarchy$center_x,
           center_y = hierarchy$center_y,
           traversal = "level")

  v <- list(tree = tree,
            boundaries = stats::setNames(boundaries$geometry, boundaries$area_id))
  class(v) <- "naomi_areas"

  v
}

print.naomi_areas <- function(areas) {
  print(areas$tree, "area_name")
}


#' Get a collection of areas
#'
#' Get a collection of areas defined by a level and nested within a collection
#' of higher level area(s).
#'
#' @param areas Data frame representing a valid area hierarchy.
#' @param level The level to return area collection
#' @param area_scope `area_id`s to subset return level, possible a vector.
#'
#' @details
#' This function recursively expands the area hierarchy to identify all areas in
#' area_level `level` which are nested within areas defined by `area_scope`.
#'
#' If `level = NULL` areas are returned at the lowest level of the hierarchy.
#' An error is thrown if `level` is not reflected in the hierarchy.
#'
#' If `area_scope = NULL` all areas at area_level `level` are returned. In this case
#' the recursion is shortcut and simply filters the hierarchy to the desired
#' level for efficiency, but it is equivalent to initialising the recursion
#' at the highest level of the hierarchy.
#'
#' If `area_scope` is not null, a column `area_scope` is returned indicating the
#' area in which each returned area is nesteed. `area_scope` can accept a vector
#' of `area_id`s and they do not have to be
#' at the same level. If the level of an area in `area_scope` is higher than
#' area_level `level`, nothing is returned. An error is thrown if any `area_scope`
#' are not recognized.
#'
#' TODO: Should be an example - where is demo_areas, demo_area_geom?
#' data(demo_areas, demo_area_geom)
#'
#' areas <- get_area_collection(demo_areas, level = 3, area_scope = c("MWI.1", "MWI.3.5"))
#' areas %>%
#'   left_join(demo_area_geom %>% filter(type == "boundary")) %>%
#'   sf::st_as_sf() %>%
#'   ggplot() + geom_sf()
#'
#' @export
get_area_collection <- function(areas, level = NULL, area_scope = NULL) {

  if(is.null(level))
    level <- max(areas$area_level)

  stopifnot(level %in% areas$area_level)

  if(is.null(area_scope))
    return(dplyr::filter(areas, area_level == level) %>%
           dplyr::select(area_id, area_level))

  stopifnot(all(area_scope %in% areas$area_id))

  area_pinched <- dplyr::filter(areas, area_level <= level) %>%
    dplyr::select(area_id, area_level, parent_area_id, area_sort_order)

  val <- dplyr::filter(area_pinched, area_id %in% area_scope) %>%
    dplyr::mutate(area_scope = area_id)

  while(any(val$area_level < level)) {
    done <- dplyr::filter(val, area_level == level)
    new <- dplyr::filter(val, area_level < level) %>%
      dplyr::select(area_scope, parent_area_id = area_id) %>%
      dplyr::inner_join(area_pinched, by = "parent_area_id")

    val <- dplyr::bind_rows(done, new)
  }

  dplyr::arrange(val, area_scope, area_sort_order) %>%
    dplyr::select(area_scope, area_id, area_level)
}


#' Long data frame mapping area hierarchy areas to model level areas
#'
#' @param model_area_ids vector of model areas.
#' @param areas naomi_areas object.
#' @param drop_partial_areas Drop areas from output if some children are
#'   missing (default TRUE).
#'
#' @examples
#'
#' area_merged <- read_area_merged(system.file("extdata/demo_areas.geojson", package = "naomi"))
#' areas <- create_areas(area_merged = area_merged)
#' model_area_ids <- area_merged$area_id[area_merged$area_level == 4]
#'
#' create_area_aggregation(model_area_ids, areas)
#'
#' @export
create_area_aggregation <- function(model_area_ids, areas, drop_partial_areas = TRUE) {

  stopifnot(methods::is(areas, "naomi_areas"))
  
  area_id_out <- areas$tree$Get("area_id",
                                traversal = "level")
  area_id_out_leaves <- areas$tree$Get("leaves",
                                       traversal = "level") %>%
    lapply(data.tree::Get, "area_id")

  area_id_out_leaves <- area_id_out_leaves[!duplicated(area_id_out)]
  area_id_out <- area_id_out[!duplicated(area_id_out)]

  stopifnot(model_area_ids %in% unlist(area_id_out_leaves))
  
  leaf_in_model <- lapply(area_id_out_leaves, `%in%`, model_area_ids)
  if(drop_partial_areas) {
    all_leaves_in_model <- vapply(leaf_in_model, all, logical(1))
    area_id_out <- area_id_out[all_leaves_in_model]
    area_id_out_leaves <- area_id_out_leaves[all_leaves_in_model]
  } else {
    area_id_out_leaves <- Map("[", area_id_out_leaves, leaf_in_model)
    area_id_out <- area_id_out[lengths(area_id_out_leaves) > 0]
    area_id_out_leaves <- area_id_out_leaves[lengths(area_id_out_leaves) > 0]
  }

  area_id_join <- Map(data.frame,
                      area_id = area_id_out,
                      model_area_id = area_id_out_leaves,
                      stringsAsFactors = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  area_id_join
}
