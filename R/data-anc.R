#' Calculate prevalence and ART coverage from ANC input data
#'
#' @param data Data frame of input ANC data
#'
#' @return Data frame with prevalence and ART coverage
#' @export
calculate_prevalence_art_coverage <- function(data) {
  data$prevalence <- calculate_anc_prevalence(data)
  data$art_coverage <- calculate_anc_art_coverage(data)
  data
}

calculate_anc_prevalence <- function(data) {
  (data$ancrt_known_pos + data$ancrt_test_pos) / (data$ancrt_known_pos + data$ancrt_tested)
}

calculate_anc_art_coverage <- function(data) {
  data$ancrt_already_art / (data$ancrt_known_pos + data$ancrt_test_pos)
}
