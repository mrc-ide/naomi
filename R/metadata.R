get_colour_scale <- function(indicator, country) {
  scales <- naomi_read_csv(system.file("extdata", "meta", "colour_scales.csv",
                           package = "naomi"))
  data <- scales[tolower(scales$country) == tolower(country) &
                   scales$indicator == indicator, ]
  if (nrow(data) == 0) {
    stop(sprintf("Can't retrieve colour scale for country %s and indicator %s.
Indicator and country combination not found in configuration.", country, indicator))
  }
  if (nrow(data) > 1) {
    stop(sprintf("Found more than one colour scale for country %s and indicator %s.
Check configuration, each country and indicator combination should have 1 and only 1 entry.",
                 country, indicator))
  }
  list(
    colour = data$colour,
    invert_scale = data$invert_scale,
    min = data$min,
    max = data$max
  )
}
