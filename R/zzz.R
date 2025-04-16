##' @importFrom traduire t_
.onLoad <- function(...) {
  naomi_init_traduire() # nocov
}

##' We need to import these as they are used by dependencies (first90)
##' unalified and need to be available in the package environment
##' for the tests to be able to run in a background progress (i.e.
##' when running in parllel)
##' @importFrom stats setNames
##' @importFrom stats qlogis
##' @importFrom stats plogis
##' @importFrom stats qnorm
##' @importFrom stats approx
##' @importFrom utils unzip
##' @importFrom utils type.convert
##' @name Imports
NULL

naomi_init_traduire <- function() {
  root <- system.file("traduire", package = "naomi", mustWork = TRUE)
  pattern <- sprintf("%s/{language}-{namespace}.json", root)
  languages <- c("en", "fr", "pt")
  namespaces <- "translation"
  traduire::translator_register(resources = NULL,
                                language = languages[[1]],
                                default_namespace = namespaces[[1]],
                                resource_pattern = pattern,
                                namespaces = namespaces,
                                fallback = "en",
                                languages = languages)
}

t_ <- function(...) {
  traduire::t_(..., package = "naomi")
}

## This mess is to avoid R CMD check NOTEs about "no visible binding for
## global variable". These NOTEs aren't that important as we're using
## lots of dplyr here, but they do obscure real issues in the CMD check
## output. So declare a list of global variables to avoid this.
utils::globalVariables(c(
  ".", ":=", "adjusted", "age", "age15plus", "age15to49", "age_below15",
  "age_fct", "age_group", "age_group1", "age_group2", "age_group_coarse",
  "age_group_infection", "age_group_label", "age_group_sort_order",
  "age_group_span", "age_group_span_out", "age_group_start",
  "age_group_start_out", "age_quarter", "age_sex_rse", "age_uid", "all_of",
  "anc_already_art", "anc_art_coverage", "anc_artcov_n", "anc_artcov_x",
  "anc_clients", "anc_clients_x", "anc_indicator", "anc_known_neg",
  "anc_known_pos", "anc_prevalence", "anc_prev_n", "anc_prev_x", "anc_status",
  "anc_tested", "anc_tested_pos", "anc_total_pos", "area_hierarchy", "area_id",
  "area_id_out", "area_idx", "area_level", "area_level_label", "area_name",
  "area_sort_order", "art_adult", "art_adult_f", "art_adult_m", "art_child",
  "art_coverage", "art_current_adjusted", "art_adjusted_adult",
  "art_adjusted_adult_f", "art_adjusted_adult_m", "art_adjusted_child",
  "art_current_calibrated","art_current_internal_spectrum", "art_current_raw",
  "art_current_residents","art_current_spectrum", "artdat_age_end",
  "artdat_age_start", "art_dec31",
  "art_new", "artnum_idx", "artpop", "artpop_dec31", "artpop_external",
  "art_prop", "art_current", "as", "asfr", "attend_area_id", "attend_area_idx",
  "attend_area_name", "attend_area_sort_order", "attend_idx",
  "aware_plhiv_attend", "aware_plhiv_num", "aware_plhiv_prop", "births",
  "births_artpop", "births_artpop_spectrum", "births_clients_ratio",
  "births_facility", "births_hivpop", "births_hivpop_spectrum",
  "births_spectrum", "calendar_quarter", "calibrated", "calibration_ratio",
  "center_x", "center_y", "ci_lower", "ci_upper", "cohort_quarter",
  ".data", "dataelement_uid", "datapack_age_group_id",
  "datapack_age_group_label", "datapack_indicator_code",
  "datapack_indicator_id", "datapack_sex_id", "datapack_sex_label",
  "data_range", "data_type", "display", "distribution",
  "district_rse", "element_blank",
  "est_calibrated", "estimate", "est_raw", "female_15plus", "fit",
  "frr_already_art", "frr_plhiv", "geometry", "hivpop", "hivpop1", "hivpop2",
  "i", "idx", "idx_out", "incidence", "indicator", "indicator_code",
  "indicator_label", "indicator_sort_order", "infections", "infections_age",
  "infections_calibrated", "infections_raw", "infections_spectrum", "input",
  "is", "is_integer", "iso3", "is_paed", "istar", "j", "jstar", "L_hivpop",
  "log_lambda_t1_offset", "log_lambda_t2_offset", "log_lambda_t3_offset",
  "log_lambda_t4_offset", "log_lambda_t5_offset", "log_odds", "lower", "L_paed",
  "map_id", "map_name", "Matrix", "max_data_quarter", "mean_strat", "median",
  "min_data_quarter", "mod_agegr_span", "mod_agegr_start", "model_area_id", "n",
  "naomi_input", "n_areas", "na_rm", "n_eff", "n_eff_kish", "net_growth_ratio",
  "n_observations", "out_idx", "parent_area_id", "plhiv", "plhiv_attend",
  "plhiv_calibrated", "plhiv_raw", "plhiv_spectrum", "population",
  "population1", "population2", "population_calibrated",
  "population_calibration_ratio", "population_denominator", "population_raw",
  "population_spectrum", "population_t1", "population_t2", "population_t3",
  "population_t4", "population_t5", "pregartcov", "pregprev", "prevalence",
  "psnu", "psnu_uid", "quarter", "quarter_id", "quarter_id_dec31",
  "quarter_id_out", "quarter_label",
  "ratio", "reside_area_id", "reside_area_idx",
  "reside_area_name", "reside_area_sort_order", "rse", "se", "sex", "sex2",
  "sex_datapack", "sex_join", "sex_naomi", "sex_out", "sex_uid",
  "spec_artcov15to49_t1", "spec_artcov15to49_t2", "spec_artcov15to49_t3",
  "spec_artcov15to49_t4", "spec_artcov15to49_t5", "spec_artcov_t1",
  "spec_artcov_t2", "spec_artcov_t3", "spec_artcov_t4", "spec_artcov_t5",
  "spec_incid_t1", "spec_incid_t2", "spec_incid_t3", "spec_incid_t4",
  "spec_incid_t5", "spec_prev15to49f_t1", "spec_prev15to49f_t2",
  "spec_prev15to49f_t3", "spec_prev15to49f_t4", "spec_prev15to49f_t5",
  "spec_prev15to49_t1", "spec_prev15to49_t2", "spec_prev15to49_t3",
  "spec_prev15to49_t4", "spec_prev15to49_t5", "spec_prev_t1", "spec_prev_t2",
  "spec_prev_t3", "spec_prev_t4", "spec_prev_t5", "spectrum",
  "spectrum_country",
  "spectrum_region_code", "spectrum_region_code_out", "spectrum_region_name",
  "spectrum_region_name_out", "std_error", "survey_id",
  "survey_mid_calendar_quarter", "threshold", "time_period", "time_step",
  "totpop", "totpop1", "totpop2", "totpop_ratio_area", "totpop_ratio_spec",
  "tree_idx", "unaware", "unaware_calibrated", "unaware_plhiv_attend",
  "unaware_plhiv_num", "unaware_plhiv_prop", "unaware_raw", "unaware_spectrum",
  "unaware_untreated_prop", "untreated_plhiv_attend", "untreated_plhiv_num",
  "upper", "value", "value_naomi", "value_spectrum", "vl_suppressed_12mos",
  "vl_suppressed_12mos_adult", "vl_suppressed_12mos_adult_f",
  "vl_suppressed_12mos_adult_m", "vl_suppressed_12mos_child",
  "vl_suppressed_12mos_total", "vl_tested_12mos", "vl_tested_12mos_adult",
  "vl_tested_12mos_adult_f", "vl_tested_12mos_adult_m", "vl_tested_12mos_child",
  "vl_tested_12mos_total", "year"))
