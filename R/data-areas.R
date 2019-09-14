#' Read shape file from ZIP
#'
#' @export
st_read_zip <- function(zfile, pattern = "shp$") {
  tmpd <- tempfile()
  on.exit(unlink(tmpd))
  unzip(zfile, exdir = tmpd)
  sf::st_read(list.files(tmpd, pattern, recursive = TRUE, full.names = TRUE))
}

#' Convert nested hierarchy from wide to long format
#'
gather_areas <- function(x) {

  val <- x %>%
    group_by(iso3 = id0,
             area_id = id0,
             area_name = name0,
             layer_id = 0,
             parent_area_id = NA) %>%
    summarise()

  for(i in 1:6)
    if(exists(paste0("id", i), x) && !is.na(x[[paste0("id", i)]]))
      val <- val %>%
        rbind(
          x %>%
          mutate(iso3 = id0,
                 layer_id = i) %>%
          rename(area_id = paste0("id", i),
                 area_name = paste0("name", i),
                 parent_area_id = paste0("id", i-1)) %>%
          group_by(iso3, area_id, area_name, layer_id, parent_area_id) %>%
          summarise
        )
  val %>%
    ungroup
}


#' `ggplot2` theme for plotting maps
th_map <- function(){
  ggplot2::theme_minimal() + ggplot2::theme(axis.text = element_blank())
}


#' Compare boundaries of two shapefiles by overlaying them
#'
#' @param sh1 is bottom shapefile with red boundaries
#' @param sh2 is top shapefile with red boundaries
#' @param aggregate whether to aggregate shapefiles
#'
compare_boundaries <- function(sh1, sh2 = NULL, aggregate = FALSE) {

  if(aggregate){
    sh1 <- dplyr::summarise(sh1)
    if(!is.null(sh2))
      sh2 <- dplyr::summarise(sh2)
  }
  p <- ggplot2::ggplot() + th_map()

  p <- p + ggplot2::geom_sf(data = sh1, color = "red")
  if(!is.null(sh2))
    p <- p + ggplot2::geom_sf(data = sh2, color = "blue", fill = NA)

  p
}

#' Check full and aggregated
#'
#' This function is useful for checking level of coarseness
#' of a simplified versus raw shapefile and any slivers
#' in a shapefile.
#'
check_boundaries <- function(sh1, sh2 = NULL){
  gridExtra::grid.arrange(
    compare_boundaries(sh1, sh2, aggregate = TRUE),
    compare_boundaries(sh1, sh2, aggregate = FALSE),
    nrow = 1)
}

#' Spread area hierarchy to wide format
#'
#' @param areas area hierarchy data.frame
#' @param min_level integer specifying the minimum level
#' 
#' @examples
#' areas <- readRDS(system.file("extdata/areas/areas.rds", package = "naomi"))
#' areas_wide <- spread_areas(areas)
#'
#' @export
spread_areas <- function(areas, min_level = min(areas$area_level), max_level = max(areas$area_level)) {

  stopifnot(min_level >= min(areas$area_level))
  stopifnot(max_level <= max(areas$area_level))

  areas_wide <- areas %>%
    dplyr::filter(area_level == min_level) %>%
    dplyr::select(
      iso3,
      !!paste0("area_id", min_level) := area_id,
      !!paste0("area_name", min_level) := area_name
    )

  for(level in (min_level + 1):max_level) {
    
    areas_wide <- areas_wide %>%
      dplyr::left_join(
        areas %>%
        dplyr::filter(area_level == level) %>%
        dplyr::select(
          iso3,
          !!paste0("area_id", level) := area_id,
          !!paste0("area_name", level) := area_name,
          parent_area_id)
       ,
        by = setNames(c("iso3", "parent_area_id"), c("iso3", paste0("area_id", level - 1L)))
      )

  }

  areas_wide$area_id <- areas_wide[[paste0("area_id", max_level)]]

  areas_wide
}
