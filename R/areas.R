#' Create an Areas Object
#'
#' Constructs and validates an areas object as an S3 class.
#'
#' @param meta Data frame of area metadata.
#' @param hierarchy Data frame of area metadata.
#' @param names Data frame of names for each area
#' @param boundaries an `sf` object with boundary geometry for each area_id
#' @param centers an `sf` object with desired center for each area_id (optional).
#' @return An object of class `naomi_areas`
#'
#' @examples
#' data(mwi_area_meta)
#' data(mwi_area_hierarchy)
#' data(mwi_area_names)
#' data(mwi_area_boundaries)
#'
#' areas <- create_areas(mwi_area_meta, mwi_area_hierarchy, mwi_area_layersn)
#' areas
#'
#' @export
create_areas <- function(meta, hierarchy, layers) {

  missing_center <- is.na(hierarchy$center_x)
  if(any(missing_center)) {
    sf::st_agr(hierarchy) <- "constant"
    ## !!! Need to update to use `withCallingHandlers()`
    px <- suppressWarnings(sf::st_point_on_surface(hierarchy[missing_center,]))
    hierarchy$center_x[missing_center] <- sf::st_coordinates(px)[,1]
    hierarchy$center_y[missing_center] <- sf::st_coordinates(px)[,2]
  }
    
  ## Validate areas

  ## - Area levels are unique
  if(any(duplicated(meta$layer_id))) {
    stop(
      paste0("Area layers not unique.\n",
             "Duplicated layer_id: ",
             paste(unique(meta$layer_id[duplicated(meta$layer_id)]),
                   collapse = ","))
    )
  }

  ## - Layers are consistent with meta data
  if(any(!meta$layer_id %in% layers$layer_id)) {
    stop(
      paste("Metadata layer_id not found in layers list:",
            paste(setdiff(meta$layer_id, layers$layer_id), collapse = ","))
    )
  }
  if(any(!layers$layer_id %in% meta$layer_id)) {
    stop(
      paste("Layers layer_id not found in layers metadata:",
            paste(unique(setdiff(layers$layer_id, meta$layer_id),
                         collapse = ","))
            )
    )
  }

  ## - Number of areas at each level matches expected
  ## - Levels are consistent between metadata and layers
  stopifnot(
    layers %>%
    dplyr::count(layer_id) %>%
    dplyr::full_join(meta, by = "layer_id") %>%
      dplyr::mutate(n_match = n == n_areas) %>%
      .$n_match
  )

  ## - Area IDs only appear once in each layer
  stopifnot(
    layers %>%
      dplyr::group_by(layer_id) %>%
      dplyr::mutate(unique_area_id = !duplicated(area_id)) %>%
      .$unique_area_id
  )

  ## - All areas have a name, boundary, and center
  assertthat::assert_that(assertthat::noNA(hierarchy$area_name))
  assertthat::assert_that(assertthat::noNA(hierarchy$geometry))
  assertthat::assert_that(assertthat::noNA(hierarchy$center_x))
  assertthat::assert_that(assertthat::noNA(hierarchy$center_y))

  ## TO DO: boundary checks
  ## - Areas are nested
  ## - Each level covers level above
  ## - Centroids like within geometry (maybe)
  ## ** These checks might be time consuming, perhaps make them optional

  tree <- hierarchy %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    dplyr::mutate(from = dplyr::if_else(is.na(parent_area_id), "r", parent_area_id)) %>%
    dplyr::select(from, area_id, dplyr::everything()) %>%
    data.tree::FromDataFrameNetwork() %>%
    {.$children[[1]]}

  boundaries <- dplyr::select(hierarchy, area_id, geometry)

  v <- list(meta = meta,
            hierarchy = tree,
            layers = layers,
            boundaries = boundaries)
  class(v) <- "naomi_areas"

  v
}

print.naomi_areas <- function(areas) {
  print(areas$hierarchy, "area_name")
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
#' `area_scope` can accept a vector of `area_id`s and they do not have to be
#' at the same level. If the level of an area in `area_scope` is higher than
#' area_level `level`, nothing is returned. An error is thrown if any `area_scope`
#' are not recognized.
#'
#' @examples
#' data(mwi_areas, mwi_area_geom)
#'
#' areas <- get_area_collection(mwi_areas, level = 3, area_scope = c("MWI.1", "MWI.3.5"))
#' areas %>%
#'   left_join(mwi_area_geom %>% filter(type == "boundary")) %>%
#'   sf::st_as_sf() %>%
#'   ggplot() + geom_sf()
#'
#' @export
get_area_collection <- function(areas, level = NULL, area_scope = NULL) {

  if(is.null(level))
    level <- max(areas$area_level)

  stopifnot(level %in% areas$area_level)

  if(is.null(area_scope))
    return(dplyr::filter(areas, area_level == level))

  stopifnot(all(area_scope %in% areas$area_id))

  area_pinched <- dplyr::filter(areas, area_level <= level)
  val <- dplyr::filter(area_pinched, area_id %in% area_scope)

  while(any(val$area_level < level)) {
    done <- dplyr::filter(val, area_level == level)
    new <- dplyr::semi_join(area_pinched,
                            dplyr::filter(val, area_level < level),
                            by = c("parent_area_id" = "area_id"))
    val <- dplyr::bind_rows(done, new)
  }

  dplyr::arrange(val, area_sort_order)
}
