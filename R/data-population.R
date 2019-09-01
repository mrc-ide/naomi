#' Data frame of age groups
#'
#' Return a data frame consisting of master age groups
#'
#' @return data frame
#' @export
get_age_groups <- function() {
  data.frame(
    age_group_start = c(0:15*5, 80,
                        15, 15, 15, 50, 0, 0, 0, 15, 25, 35, 50, 65),
    age_group_span = c(rep(5, 16), Inf,
                       35, 50, Inf, Inf, Inf, 65, 15, 10, 10, 15, 15, Inf)
  ) %>%
    dplyr::mutate(age_group_id = dplyr::row_number(),
                  age_group_label = paste0(age_group_start, "-", age_group_start + age_group_span - 1) %>%
                    sub("-Inf", "+", .) %>%
                    dplyr::recode("0+" = "all ages")) %>%
    dplyr::select(age_group_id, age_group_label, age_group_start, age_group_span)
}


#' Log-linear interpolation of age/sex stratified population
#'
#' @param pop_agesex a subset of the population_agesex.
#' @param times vector of times to return interpolation.
#'
#' @return
#' A data.frame with same columns as pop_agesex interpolated to `times`.
#'
#' @details
#' `zoo::na.approx()` is used to interpolate log(population).
#'
#' @examples
#' ## Interpolate Malawi population at level 2 (Zone) at two time points 
#' population_agesex <- readRDS(system.file("extdata/population/population_agesex.rds", package = "naomi"))
#' pop_interp <- interpolate_population_agesex(population_agesex, c(2016.25, 2019.75))
#'
#' @export
interpolate_population_agesex <- function(population_agesex, times = seq(2010.5, 2019.5, 0.25)) {

  dfall <- dplyr::distinct(dplyr::select(population_agesex, -time, -population))
  
  df <- dplyr::select(population_agesex, time, area_id, source, sex, age_group_id, population)

  tidyr::expand(df, time = times,
                tidyr::nesting(area_id, source, sex, age_group_id)) %>%
    dplyr::full_join(df, by = names(.)) %>%
    dplyr::group_by(area_id, source, sex, age_group_id) %>%
    dplyr::mutate(population = exp(zoo::na.approx(log(population), time, na.rm = FALSE))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(time %in% times) %>%
    dplyr::left_join(dfall, by = intersect(names(.), names(dfall))) %>%
    dplyr::select(names(population_agesex))

}
