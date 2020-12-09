#' Data frame of age groups
#'
#' Return a data frame consisting of master age groups
#'
#' @return data frame
#' @export
get_age_groups <- function() {

  naomi_read_csv(system_file("metadata", "meta_age_group.csv"))

}

#' Time period indexing
#'
#' Time periods are indexed by integers for efficiency and precision.
#' Quarters are indexed as the number of quarters since the beginning of
#' 1900: $quarter_id = (year - 1900) * 4 + quarter$.
#'
#' @param quarter_id vector of integer quarter IDs.
#' @param year vector of integer years.
#' @param quarter vector of integer quarters (1,2,3,4).
#' @param calendar_quarter Vector of calendar quarters to convert.
#'
#' @details
#' Quarters are labelled as "Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec" instead of
#' "Q1", "Q2", "Q3", "Q4" to avoid confusion between calendar quarters and offset
#' fiscal year quarters.
#'
#' @examples
#' quarter_ids <- convert_quarter_id(c(2009, 2017), c(3, 1))
#' quarter_ids
#' calender_quarters <- convert_calendar_quarter(c(2009, 2017), c(3, 1))
#' quarter_number(quarter_ids)
#' quarter_labels(quarter_ids)
#' year_labels(quarter_ids)
#' quarter_year_labels(quarter_ids)
#' calendar_quarter_labels("CY2015Q2")
#' calendar_quarter_to_year("CY2015Q2")
#'
#' @export
quarter_year_labels <- function(quarter_id) {
  paste(quarter_labels(quarter_id), year_labels(quarter_id))
}

#' @rdname quarter_year_labels
#' @export
quarter_number <- function(quarter_id) {
  quarter_id - 4 * (quarter_id - 1) %/% 4
}

#' @rdname quarter_year_labels
#' @export
quarter_labels <- function(quarter_id) {
  c(t_("MONTH_MARCH"), t_("MONTH_JUNE"), t_("MONTH_SEPTEMBER"), t_("MONTH_DECEMBER"))[quarter_number(quarter_id)]
}

#' @rdname quarter_year_labels
#' @export
calendar_quarter_labels <- function(calendar_quarter) {
  quarter_year_labels(calendar_quarter_to_quarter_id(calendar_quarter))
}

#' @rdname quarter_year_labels
#' @export
year_labels <- function(quarter_id) {
  1900 + (quarter_id - 1) %/% 4
}

#' @rdname quarter_year_labels
#' @export
convert_quarter_id <- function(year, quarter) {

  stopifnot(year %% 1 == 0)
  stopifnot(quarter %in% 1:4)

  as.integer((year - 1900) * 4 + quarter)
}

#' @rdname quarter_year_labels
#' @export
convert_calendar_quarter <- function(year, quarter) {

  stopifnot(year %% 1 == 0)
  stopifnot(quarter %in% 1:4)

  paste0("CY", year, "Q", quarter)
}

#' @rdname quarter_year_labels
#' @export
calendar_quarter_to_quarter_id <- function(calendar_quarter) {

  convert_quarter_id(as.integer(substr(calendar_quarter, 3, 6)),
                     as.integer(substr(calendar_quarter, 8, 8)))
}

#' @rdname quarter_year_labels
#' @export
quarter_id_to_calendar_quarter <- function(quarter_id) {
  quarter <- quarter_id %% 4
  if (quarter == 0) {
    quarter <- 4
  }
  year <- (quarter_id - quarter) / 4 + 1900
  convert_calendar_quarter(year, quarter)
}

#' @rdname quarter_year_labels
#' @export
calendar_quarter_to_year <- function(calendar_quarter) {
  year_labels(calendar_quarter_to_quarter_id(calendar_quarter))
}

#' Log-linear interpolation of age/sex stratified population
#'
#' @param population_agesex a subset of the population_agesex.
#' @param calendar_quarters vector of calendar quarters to return interpolation.
#'
#' @return
#' A data.frame with same columns as pop_agesex interpolated to `times`.
#'
#' @details
#' `zoo::na.approx()` is used to interpolate log(population).
#'
#' @seealso
#' [convert_quarter_id()]
#'
#' @examples
#' ## Interpolate Malawi population at level 2 (Zone) at two time points
#' data(demo_population_agesex)
#' calendar_quarters <- c("CY2016Q1", "CY2018Q3")
#' pop_interp <- interpolate_population_agesex(demo_population_agesex, calendar_quarters)
#'
#' @export
interpolate_population_agesex <- function(population_agesex, calendar_quarters) {

  raw <- dplyr::select(population_agesex, area_id, source,
                       calendar_quarter, sex, age_group, population)

  check <- dplyr::count(raw, area_id, source, calendar_quarter, sex, age_group)

  if(any(check$n > 1))
    stop(paste0("Duplicated population counts for ", sum(check$n > 1),
                " area/source/quarter/sex/age combinatons"))

  quarter_ids <- calendar_quarter_to_quarter_id(calendar_quarters)
  dfall <- dplyr::distinct(dplyr::select(raw, -calendar_quarter, -population))

  df <- dplyr::select(raw, calendar_quarter, area_id, source, sex, age_group, population) %>%
    dplyr::mutate(quarter_id = calendar_quarter_to_quarter_id(calendar_quarter))

  val <- tidyr::expand(
                  df,
                  tidyr::nesting(calendar_quarter = calendar_quarters,
                                 quarter_id = quarter_ids),
                  tidyr::nesting(area_id, source, sex, age_group)
                ) %>%
    dplyr::full_join(df, by = names(.)) %>%
    dplyr::group_by(area_id, source, sex, age_group) %>%
    dplyr::mutate(population = log_linear_interp(population, quarter_id, rule = 2)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(quarter_id %in% quarter_ids) %>%
    dplyr::left_join(dfall, by = intersect(names(.), names(dfall))) %>%
    dplyr::select(names(raw))

  val
}

#' Log-linear interpolation of NA values
#'
#' @param y vector of output values, possibly with NAs
#' @param x vector of points to interpolate (no NAs)
#' @param rule rule for extrapolating outside range (see [approx()])
#' @param replace_na value to replace if interpolation evaluates to NA
#'
#' @examples
#' log_linear_interp(c(100, 105, NA, 110), 1:4)
#' log_linear_interp(c(NA, 105, NA, 110), 1:4)
#' log_linear_interp(c(NA, 105, NA, 110, NA), 1:5, rule = 1)
#' log_linear_interp(c(NA, 105, NA, 110, NA), 1:5, rule = 2)
#' log_linear_interp(c(NA, NA, 37), 1:3, rule = 2)
#'
#' @export
log_linear_interp <- function(y, x, rule = 2, replace_na = 0){
  if(sum(!is.na(y)) == 1 && rule == 2)
    yout <- tidyr::replace_na(y, y[!is.na(y)])
  else
    yout <- exp(zoo::na.approx(log(y), x, na.rm = FALSE, rule = rule))
  yout <- tidyr::replace_na(yout, replace_na)
  yout
}
