#' Get plotting metadata for a particular country
#'
#' @param iso3 iso3 code of country to get metadata for or blank for default
#' configuration.
#'
#' @return List of plotting metadata about how to locate data for a specific
#' indicator, data type and plot type. Also returns metadata about colour
#' scheme to use for that country and indicator.
#'
#' @export
#'
#' @examples
#' get_plotting_metadata("MWI")
get_plotting_metadata <- function(iso3) {
  metadata <- get_metadata()
  colour_scale <- get_colour_scale(iso3)
  if (nrow(colour_scale) == 0) {
    message(sprintf(
      "Country with iso3 code %s not in metadata - returning default colour scales.", iso3))
    colour_scale <- get_colour_scale()
  }
  merge(x = metadata, y = colour_scale, by = "indicator", sort = FALSE)
}

#' Get colour scale and ranges for a particular country
#'
#' @param iso3 iso3 code of country to get metadata for or blank for default
#' configuration.
#'
#' @return List of scale information including colour as a d3 scale chromatic
#' function name, whether to invert the scale and a min and max value for the
#' scale.
#'
#' @keywords internal
get_colour_scale <- function(iso3 = "default") {
  scales <- naomi_read_csv(system_file("metadata", "colour_scales.csv"))
  data <- scales[tolower(scales$iso3) == tolower(iso3), ]
  if (nrow(data) == 0 && iso3 == "default") {
    stop(sprintf("Can't retrieve default colour scale. Check configuration."))
  }
  data
}

#' Get indicator metadata as a data frame
#'
#'
#' @return Metadata about indicators as a list
#'
#' @export
#'
#' @examples
#' get_metadata()
get_metadata <- function() {
  data <- naomi_read_csv(system_file("metadata", "metadata.csv"))
  data$name <- traduire::translator()$replace(data$name)
  data
}

#' Get 5 year age groups
#'
#'
#' @return ID for 5 year age groups
#'
#' @export
#'
#' @examples
#' get_five_year_age_groups()
get_five_year_age_groups <- function() {
  age_groups <- get_age_groups()
  age_groups <- age_groups[age_groups$age_group_span == 5 | age_groups$age_group == "80+", ]
  age_groups$age_group
}
