#' Calculate prevalence and ART coverage from ANC input data
#'
#' @param data Data frame of input ANC data
#'
#' @return Data frame with prevalence and ART coverage
#' @export
calculate_prevalence_art_coverage <- function(data) {
  data$anc_prevalence <- calculate_anc_prevalence(data)
  data$anc_art_coverage <- calculate_anc_art_coverage(data)
  data
}

calculate_anc_prevalence <- function(data) {
  (data$anc_known_pos + data$anc_tested_pos) / (data$anc_known_pos + data$anc_tested)
}

calculate_anc_art_coverage <- function(data) {
  data$anc_already_art / (data$anc_known_pos + data$anc_tested_pos)
}
