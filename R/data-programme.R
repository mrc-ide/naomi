#' Prepare ANC input data for web interface
#'
#' This function adds additional columns required for plotting ANC data
#' in the Naomi web app. It will
#' * Calculate prevalence and ART coverage
#' * Add sample size
#'
#' @param data Data frame of input ANC data
#'
#' @return Data frame with additional columns
#' @export
anc_prepare_hintr <- function(data) {
  data$anc_prevalence <- calculate_anc_prevalence(data)
  data$anc_art_coverage <- calculate_anc_art_coverage(data)
  data$sample_size <- 0
  data
}

calculate_anc_prevalence <- function(data) {
  (data$anc_known_pos + data$anc_tested_pos) / (data$anc_known_pos + data$anc_tested + data$anc_known_neg)
}

calculate_anc_art_coverage <- function(data) {
  data$anc_already_art / (data$anc_known_pos + data$anc_tested_pos)
}

#' Prepare ART input data for web interface
#'
#' This function adds additional columns required for plotting ART data
#' in the Naomi web app. It will
#' * Add sample size
#'
#' @param data Data frame of input ART data
#'
#' @return Data frame with additional columns
#' @export
art_prepare_hintr <- function(data) {
  data$sample_size <- 0
  data
}
