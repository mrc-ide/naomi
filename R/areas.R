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
#' areas <- create_areas(mwi_area_meta, mwi_area_hierarchy, mwi_area_names, mwi_area_boundaries)
#' areas
#'
#' @export
create_areas <- function(meta, hierarchy, names, boundaries, centers = NULL) {

  if(is.null(centers))
    centers <- suppressWarnings(sf::st_point_on_surface(boundaries))

  meta <- meta[order(meta$area_level), ]

  ## Validate areas

  ## - Area levels are unique and sequential
  stopifnot(!duplicated(meta$area_level))
  stopifnot(diff(meta$area_level) == 1)

  ## - Number of areas by level is non-decreasing
  stopifnot(diff(meta$n_areas) >= 0)

  ## - Levels in hierarchy are consistent with metadata
  stopifnot(meta$area_level %in% hierarchy$area_level)
  stopifnot(hierarchy$area_level %in% meta$area_level)

  ## - Number of areas at each level matches expected
  ## - Levels are consistent between metadata and hierarchy
  stopifnot(
    hierarchy %>%
      dplyr::count(area_level) %>%
      dplyr::full_join(meta, by = "area_level") %>%
      dplyr::mutate(n_match = n == n_areas) %>%
      .$n_match
  )

  ## - Area IDs only appear once in each level of hierarchy
  stopifnot(
    hierarchy %>%
      dplyr::group_by(area_level) %>%
      dplyr::mutate(unique_area_id = !duplicated(area_id)) %>%
      .$unique_area_id
  )

  ## - All areas have a name, boundary, and center
  stopifnot(hierarchy$area_id %in% names$area_id)
  stopifnot(hierarchy$area_id %in% boundaries$area_id)
  stopifnot(hierarchy$area_id %in% centers$area_id)

  stopifnot(!is.na(names$area_name))
  stopifnot(!is.na(boundaries$geometry))
  stopifnot(!is.na(centers$geometry))

  ## TO DO: boundary checks
  ## - Areas are nested
  ## - Each level covers level above
  ## - Centroids like within geometry (maybe)
  ## ** These checks might be time consuming, perhaps make them optional

  tree <- mwi_area_hierarchy %>%
    dplyr::filter(!is.na(parent_area_id)) %>%
    dplyr::mutate(from = paste0(parent_area_id, "_", area_level-1L),
                  to = paste0(area_id, "_", area_level)) %>%
    dplyr::select(from, to, dplyr::everything()) %>%
    data.tree::FromDataFrameNetwork()

  v <- list(meta = meta,
            hierarchy = tree,
            names = names,
            boundaries = boundaries,
            centers = centers)
  class(v) <- "naomi_areas"

  v
}

print.naomi_areas <- function(areas) {
  print(areas$hierarchy)
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
