#' Get plotting metadata for a particualr country
#'
#' @param country Country to get metadata for
#'
#' @return List of plotting metadata about how to locate data for a specific
#' indicator, data type and plot type. Also returns metadata about colour
#' scheme to use for that country and indicator.
#'
#' @export
#'
#' @examples
#' get_plotting_metadata("Malawi")
get_plotting_metadata <- function(country) {
  metadata <- get_metadata()
  colour_scale <- get_colour_scale(country)
  merge(x = metadata, y = colour_scale, by = "indicator")
}

#' Get colour scale and ranges for a particular country
#'
#' @param country Country to get scale for
#'
#' @return List of scale information including colour as a d3 scale chromatic
#' function name, whether to invert the scale and a min and max value for the
#' scale.
#'
#' @keywords internal
get_colour_scale <- function(country) {
  scales <- naomi_read_csv(system_file("extdata", "meta", "colour_scales.csv"))
  data <- scales[tolower(scales$country) == tolower(country), ]
  if (nrow(data) == 0) {
    stop(sprintf(
      "Can't retrieve colour scale for country %s. Country not found in configuration.",
      country))
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
  naomi_read_csv(system_file("extdata", "meta", "metadata.csv"))
}

