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
