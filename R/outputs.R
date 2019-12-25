
meta_indicator <-
  data.frame(
    indicator_id = 1:10,
    indicator = c("population",
                  "prevalence",
                  "plhiv",
                  "art_coverage",
                  "art_num_residents",
                  "art_num_attend",
                  "incidence",
                  "infections",
                  "anc_prevalence",
                  "anc_art_coverage"),
    indicator_label = c("Population",
                        "HIV Prevalence",
                        "PLHIV",
                        "ART Coverage",
                        "ART Number (residents)",
                        "ART Number (attending)",
                        "HIV Incidence",
                        "New Infections",
                        "ANC HIV Prevalence",
                        "ANC Prior ART Coverage"),
    description = c("Population size",
                    "Proportion of total population HIV positive",
                    "Number of people living with HIV",
                    "Proportion of PLHIV on ART (residents)",
                    "Number on ART (residents)",
                    "Number receiving ART (attending)",
                    "HIV incidence rate per year",
                    "Number of new infections per year",
                    "HIV prevalence among ANC attendees",
                    "ART coverage among ANC attendees prior to first ANC"),
    parameter = c("population_out",
                  "rho_out",
                  "plhiv_out",
                  "alpha_out",
                  "artnum_out",
                  "artattend_out",
                  "lambda_out",
                  "infections_out",
                  "anc_rho",
                  "anc_alpha"),
    format = NA,
    scale = NA,
    stringsAsFactors = FALSE
  )


add_stats <- function(df, mode = NULL, sample = NULL, prefix = ""){

  v <- df

  if(!is.null(mode)) {
    v[[paste0(prefix, "mode")]] <- mode
  } else {
    v[[paste0(prefix, "mode")]] <- NA_real_
  }

  if(!is.null(sample)) {
    qtl <- apply(sample, 1, stats::quantile, c(0.5, 0.025, 0.975))
    v[[paste0(prefix, "mean")]] <- rowMeans(sample)
    v[[paste0(prefix, "se")]] <- sqrt(rowSums((sample - v[[paste0(prefix, "mean")]])^2) / (max(ncol(sample), 2) - 1))
    v[[paste0(prefix, "median")]] <- qtl[1,]
    v[[paste0(prefix, "lower")]] <- qtl[2,]
    v[[paste0(prefix, "upper")]] <- qtl[3,]
  } else {
    v[paste0(prefix, c("mean", "se", "median", "lower", "upper"))] <- NA_real_
  }

  v[c(names(df), paste0(prefix, c("mean", "se", "median", "mode", "lower", "upper")))]
}


extract_indicators <- function(naomi_fit, naomi_mf) {
  
  get_est <- function(varname,
                      indicator_id,
                      calendar_quarter,
                      mf = naomi_mf$mf_out) {
    v <- dplyr::mutate(
      mf,
      quarter_id = calendar_quarter_to_quarter_id(calendar_quarter),
      indicator_id = indicator_id)

    if(!is.null(naomi_fit$sample)) {
      v <- add_stats(v, naomi_fit$mode[[varname]], naomi_fit$sample[[varname]])
    } else {
      v <- add_stats(v, naomi_fit$mode[[varname]])
    }

    v
  }

  indicator_ids_t1 <- c("population_t1_out" = 1,
                        "rho_t1_out" = 2,
                        "plhiv_t1_out" = 3,
                        "alpha_t1_out" = 4,
                        "artnum_t1_out" = 5,
                        "artattend_t1_out" = 6,
                        "lambda_t1_out" = 7,
                        "infections_t1_out" = 8)

  indicator_ids_t2 <- c("population_t2_out" = 1,
                        "rho_t2_out" = 2,
                        "plhiv_t2_out" = 3,
                        "alpha_t2_out" = 4,
                        "artnum_t2_out" = 5,
                        "artattend_t2_out" = 6,
                        "lambda_t2_out" = 7,
                        "infections_t2_out" = 8)

  indicators_t1 <- Map(get_est, names(indicator_ids_t1), indicator_ids_t1, naomi_mf$calendar_quarter1)
  indicators_t2 <- Map(get_est, names(indicator_ids_t2), indicator_ids_t2, naomi_mf$calendar_quarter2)

  mf_anc_out <- naomi_mf$mf_areas %>%
    dplyr::transmute(area_id,
                     sex = "female",
                     age_group = "15-49")

  out <- dplyr::bind_rows(
                  indicators_t1,
                  get_est("anc_rho_t1_out", 9, naomi_mf$calendar_quarter1, mf_anc_out),
                  get_est("anc_alpha_t1_out", 10, naomi_mf$calendar_quarter1, mf_anc_out),
                  indicators_t2,
                  get_est("anc_rho_t2_out", 9, naomi_mf$calendar_quarter2, mf_anc_out),
                  get_est("anc_alpha_t2_out", 10, naomi_mf$calendar_quarter2, mf_anc_out))
  
  dplyr::select(out, names(naomi_mf$mf_out),
                quarter_id, indicator_id, mean, se, median, mode, lower, upper)
}

extract_art_attendance <- function(naomi_fit, naomi_mf) {

  mode <- naomi_fit$mode
  
  mfout <- naomi_mf$mf_out %>%
    dplyr::mutate(out_idx = dplyr::row_number())
  
  v <- naomi_mf$mf_artattend %>%
    dplyr::select(reside_area_id, attend_area_id) %>%
    dplyr::mutate(sex = "both",
                  age_group = "00+") %>%
    dplyr::left_join(
             dplyr::rename(mfout, reside_area_id = area_id, reside_out_idx = out_idx),
             by = c("reside_area_id", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             dplyr::rename(mfout, attend_area_id = area_id, attend_out_idx = out_idx),
             by = c("attend_area_id", "sex", "age_group")
           )

  if(!is.null(mode)) {
    m_artattend_ij_t1 <- mode$artattend_ij_t1_out
    m_artnum_reside_t1 <- mode$artnum_t1_out[v$reside_out_idx]
    m_artnum_attend_t1 <- mode$artattend_t1_out[v$attend_out_idx]
    
    m_artattend_ij_t2 <- mode$artattend_ij_t2_out
    m_artnum_reside_t2 <- mode$artnum_t2_out[v$reside_out_idx]
    m_artnum_attend_t2 <- mode$artattend_t2_out[v$attend_out_idx]
  } else {
    m_artattend_ij_t1 <- NULL
    m_artattend_ij_t2 <- NULL
  }

  if(!is.null(m_artattend_ij_t1)) {
    m_prop_residents_t1 <- m_artattend_ij_t1 / m_artnum_reside_t1
    m_prop_attendees_t1 <- m_artattend_ij_t1 / m_artnum_attend_t1
  } else {
    m_prop_residents_t1 <- NULL
    m_prop_attendees_t1 <- NULL
  }

  if(!is.null(m_artattend_ij_t2)) {
    m_prop_residents_t2 <- m_artattend_ij_t2 / m_artnum_reside_t2
    m_prop_attendees_t2 <- m_artattend_ij_t2 / m_artnum_attend_t2
  } else {
    m_prop_residents_t2 <- NULL
    m_prop_attendees_t2 <- NULL
  }

  if(!is.null(naomi_fit$sample)) {

    s_artattend_ij_t1 <- naomi_fit$sample$artattend_ij_t1_out
    s_artnum_reside_t1 <- naomi_fit$sample$artnum_t1_out[v$reside_out_idx, ]
    s_artnum_attend_t1 <- naomi_fit$sample$artattend_t1_out[v$attend_out_idx, ]
   
    s_artattend_ij_t2 <- naomi_fit$sample$artattend_ij_t2_out
    s_artnum_reside_t2 <- naomi_fit$sample$artnum_t2_out[v$reside_out_idx, ]
    s_artnum_attend_t2 <- naomi_fit$sample$artattend_t2_out[v$attend_out_idx, ]
  } else {
    s_artattend_ij_t1 <- NULL
    s_artattend_ij_t2 <- NULL
  }

  if(!is.null(s_artattend_ij_t1)) {
    s_prop_residents_t1 <- s_artattend_ij_t1 / s_artnum_reside_t1
    s_prop_attendees_t1 <- s_artattend_ij_t1 / s_artnum_attend_t1
  } else {
    s_prop_residents_t1 <- NULL
    s_prop_attendees_t1 <- NULL
  }

  if(!is.null(s_artattend_ij_t2)) {
    s_prop_residents_t2 <- s_artattend_ij_t2 / s_artnum_reside_t2
    s_prop_attendees_t2 <- s_artattend_ij_t2 / s_artnum_attend_t2
  } else {
    s_prop_residents_t2 <- NULL
    s_prop_attendees_t2 <- NULL
  }

  v$reside_out_idx <- NULL
  v$attend_out_idx <- NULL

  v_t1 <- dplyr::mutate(v, quarter_id = calendar_quarter_to_quarter_id(naomi_mf$calendar_quarter1))
  v_t1 <- add_stats(v_t1, m_artattend_ij_t1, s_artattend_ij_t1, "artnum_")
  v_t1 <- add_stats(v_t1, m_prop_residents_t1, s_prop_residents_t1, "prop_residents_")
  v_t1 <- add_stats(v_t1, m_prop_attendees_t1, s_prop_attendees_t1, "prop_attendees_")

  v_t2 <- dplyr::mutate(v, quarter_id = calendar_quarter_to_quarter_id(naomi_mf$calendar_quarter2))
  v_t2 <- add_stats(v_t2, m_artattend_ij_t2, s_artattend_ij_t2, "artnum_")
  v_t2 <- add_stats(v_t2, m_prop_residents_t2, s_prop_residents_t2, "prop_residents_")
  v_t2 <- add_stats(v_t2, m_prop_attendees_t2, s_prop_attendees_t2, "prop_attendees_")

  dplyr::bind_rows(v_t1, v_t2)
}


#' Build output package from fit
#'
#' @param naomi_fit Fitted naomi model
#' @param naomi_mf Naomi model frame
#' @param area_merged Merged area dataset
#'
#' @return List containing output indicators and metadata.
#' @export
output_package <- function(naomi_fit, naomi_mf, area_merged) {

  indicators <- extract_indicators(naomi_fit, naomi_mf)

  ## ## !!! Temporary insert <1 / 1-4 results
  ## indicators <- indicators %>%
  ##   dplyr::bind_rows(
  ##     dplyr::filter(indicators, age_group_id == 1) %>%
  ##     dplyr::select(-age_group_id) %>%
  ##     tidyr::crossing(age_group_id = 30:31)
  ##   )

  art_attendance <- extract_art_attendance(naomi_fit, naomi_mf)
  
  meta_area <- area_merged %>%
    dplyr::filter(area_id %in% unique(naomi_mf$mf_out$area_id)) %>%
    dplyr::select(area_level, area_level_label, area_id, area_name, parent_area_id, spectrum_region_code, area_sort_order, center_x, center_y, geometry) %>%
    sf::st_as_sf()

  meta_period <- data.frame(
    calendar_quarter = c(naomi_mf$calendar_quarter1, naomi_mf$calendar_quarter2),
    stringsAsFactors = FALSE
  )%>%
    dplyr::mutate(
             quarter_id = calendar_quarter_to_quarter_id(calendar_quarter),
             quarter_label = naomi::quarter_year_labels(quarter_id)
           )

  meta_age_group <- get_age_groups()

  ## # Fitting outputs
  fit <- list()
  fit$spectrum_calibration <- naomi_mf$spectrum_calibration
  fit$calibration_options <- naomi_mf$calibration_options
  
  val <- list(
    indicators = indicators,
    art_attendance = art_attendance,
    meta_area = meta_area,
    meta_age_group = meta_age_group,
    meta_period = meta_period,
    meta_indicator = meta_indicator,
    fit = fit
  )

  class(val) <- "naomi_output"

  val
}


#' Add labels to output indicators
#'
#' @param naomi_output Naomi output object.
#'
#' @return Labelled output indicators
#' @export
add_output_labels <- function(naomi_output) {

  stopifnot(inherits(naomi_output, "naomi_output"))

  indicators <- naomi_output$indicators %>%
    dplyr::left_join(
             naomi_output$meta_area %>%
             as.data.frame %>%
             dplyr::select(area_id, area_level, area_level_label, area_name, area_sort_order),
             by = "area_id"
           ) %>%
    dplyr::left_join(
             naomi_output$meta_age_group %>%
             dplyr::select(age_group_id, age_group, age_group_label, age_group_sort_order),
             by = "age_group"
           ) %>%
    dplyr::left_join(naomi_output$meta_period, by = "quarter_id") %>%
    dplyr::left_join(
             naomi_output$meta_indicator %>%
             dplyr::select(indicator_id, indicator, indicator_label),
             by = "indicator_id"
           ) %>%
    dplyr::arrange(
             area_level,
             area_sort_order,
             quarter_id,
             indicator_id,
             sex,
             age_group_sort_order
           ) %>%
    dplyr::select(
             area_level,
             area_level_label,
             area_id,
             area_name,
             sex,
             age_group,
             age_group_id,
             age_group_label,
             calendar_quarter,
             quarter_id,
             quarter_label,
             indicator,
             indicator_id,
             indicator_label,
             mean,
             se,
             median,
             mode,
             lower,
             upper
           )

  indicators
}

add_art_attendance_labels <- function(naomi_output) {

  stopifnot(inherits(naomi_output, "naomi_output"))

  art_attendance <- naomi_output$art_attendance %>%
    dplyr::left_join(
             sf::st_drop_geometry(naomi_output$meta_area) %>%
             dplyr::select(reside_area_id = area_id,
                           reside_area_name = area_name,
                           reside_area_sort_order = area_sort_order),
             by = "reside_area_id"
           ) %>%
    dplyr::left_join(
             sf::st_drop_geometry(naomi_output$meta_area) %>%
             dplyr::select(attend_area_id = area_id,
                           attend_area_name = area_name,
                           attend_area_sort_order = area_sort_order),
             by = "attend_area_id"
           ) %>%
    dplyr::left_join(
             naomi_output$meta_age_group %>%
             dplyr::select(age_group_id, age_group, age_group_label, age_group_sort_order),
             by = "age_group"
           ) %>%
    dplyr::left_join(naomi_output$meta_period, by = "quarter_id") %>%
    dplyr::arrange(
             reside_area_sort_order,
             attend_area_sort_order,
             quarter_id,
             sex,
             age_group_sort_order
           ) %>%
    dplyr::select(
             reside_area_id,
             reside_area_name,
             attend_area_id,
             attend_area_name,
             sex,
             age_group,
             age_group_id,
             age_group_label,
             calendar_quarter,
             quarter_id,
             quarter_label,
             dplyr::starts_with("artnum"),
             dplyr::starts_with("prop_residents"),
             dplyr::starts_with("prop_attendees")
           )

  art_attendance
}

#' Save outputs to zip file
#'
#' @param naomi_output Naomi output object
#' @param filename Name of file to create
#' @param dir Directory to create zip in
#' @param overwrite If TRUE overwrite any existing file
#' @param with_labels If TRUE save indicator ids with labels
#' @param boundary_format Either geojson or shp for saving boundary as geojson
#' or shape format
#' @param single_csv If TRUE only output the csv of indicators, otherwise save
#' the metadata too
#'
#' @return Path to created zip file
#' @export
save_output_package <- function(naomi_output,
                                filename,
                                dir,
                                overwrite = FALSE,
                                with_labels = FALSE,
                                boundary_format = "geojson",
                                single_csv = FALSE) {

  save_output(filename, dir, naomi_output, overwrite,
              with_labels, boundary_format, single_csv)
}

save_result_summary <- function(path, naomi_output) {
  save_output(basename(path), dirname(path), naomi_output, overwrite = FALSE,
              with_labels = TRUE, boundary_format = "geojson",
              single_csv = FALSE)
}

save_output_spectrum <- function(path, naomi_output) {
  save_output(basename(path), dirname(path), naomi_output,
              overwrite = FALSE,
              with_labels = TRUE, boundary_format = "geojson",
              single_csv = FALSE)
}


save_output <- function(filename, dir,
                        naomi_output,
                        overwrite = FALSE,
                        with_labels = FALSE,
                        boundary_format = "geojson",
                        single_csv = FALSE) {
  dir <- normalizePath(dir, mustWork = TRUE)
  if(!file.access(dir, 2) == 0) {
    stop(paste("Directory", dir, "is not writable."))
  }
  if (!grepl(".zip", filename)) {
    filename <- paste0(filename, ".zip")
  }
  path <- file.path(dir, filename)
  stopifnot(inherits(naomi_output, "naomi_output"))
  if (file.access(path, 0) == 0 && !overwrite) {
    stop(paste(
      "File", path, "already exists. Set overwrite = TRUE to write output."))
  }

  if (with_labels) {
    indicators <- add_output_labels(naomi_output)
    art_attendance <- add_art_attendance_labels(naomi_output)
  } else {
    indicators <- naomi_output$indicators
    art_attendance <- naomi_output$art_attendance
  }

  tmpd <- tempfile()
  dir.create(tmpd)
  old <- setwd(tmpd)
  on.exit(setwd(old))
  naomi_write_csv(indicators, "indicators.csv")

  if(!single_csv) {
    naomi_write_csv(art_attendance, "art_attendance.csv")
    naomi_write_csv(sf::st_drop_geometry(naomi_output$meta_area),
                    "meta_area.csv")
    naomi_write_csv(naomi_output$meta_age_group, "meta_age_group.csv")
    naomi_write_csv(naomi_output$meta_period, "meta_period.csv")
    naomi_write_csv(naomi_output$meta_indicator, "meta_indicator.csv")

    naomi_output$meta_area$name <- naomi_output$meta_area$area_id
    if(!is.null(boundary_format) && !is.na(boundary_format)) {
      if(boundary_format == "geojson") {
        sf::st_write(naomi_output$meta_area, "boundaries.geojson", quiet = TRUE)
      } else if(boundary_format == "shp") {
        dir.create("shp")
        sf::st_write(naomi_output$meta_area, "shp/boundaries.shp", quiet = TRUE)
      } else {
        stop(paste("Boundary file format", boundary_format, "not recognized.",
                   "Please select 'geojson', 'shp', or NA to not save boundaries."))
      }
    }
  }

  info <- attr(naomi_output, "info")
  if (length(info) > 0L) {
    dir.create("info")
    for (p in names(info)) {
      writeLines(trimws(info[[p]]), file.path("info", p))
    }
  }

  fit <- naomi_output$fit
  if(length(fit) > 0L) {
    dir.create("fit")
    naomi_write_csv(fit$spectrum_calibration, "fit/spectrum_calibration.csv")

    naomi_write_csv(
      data.frame(option = names(fit$calibration_options),
                 value  = unlist(fit$calibration_options)),
      "fit/calibration_options.csv")
  }

  zip::zipr(path, list.files())
  path
}


#' @rdname save_output_package
#' @param path Path to output zip file.
#' @export
read_output_package <- function(path) {

  tmpd <- tempfile()
  on.exit(unlink(tmpd))

  utils::unzip(path, exdir = tmpd)

  v <- list(
    indicators = readr_read_csv(file.path(tmpd, "indicators.csv")),
    art_attendance = readr_read_csv(file.path(tmpd, "art_attendance.csv")),
    meta_area = sf::read_sf(file.path(tmpd, "boundaries.geojson")),
    meta_age_group = readr_read_csv(file.path(tmpd, "meta_age_group.csv")),
    meta_period = readr_read_csv(file.path(tmpd, "meta_period.csv")),
    meta_indicator = readr_read_csv(file.path(tmpd, "meta_indicator.csv"))
  )

  class(v) <- "naomi_output"
  v
}


## !!! TODO: Documentation and tests

calibrate_outputs <- function(output,
                              naomi_mf,
                              spectrum_plhiv_calibration_level,
                              spectrum_plhiv_calibration_strat,
                              spectrum_artnum_calibration_level,
                              spectrum_artnum_calibration_strat) {

  stopifnot(inherits(output, "naomi_output"))
  stopifnot(inherits(naomi_mf, "naomi_mf"))
  
  group_vars <- c("spectrum_region_code", "calendar_quarter", "sex", "age_group")

  ## !!! TODO: much of this joining will be obolete by replacing _id with
  ##     the more readable versions.

  mf <- dplyr::select(naomi_mf$mf_model, area_id, sex, age_group) %>%
    dplyr::left_join(
             sf::st_drop_geometry(output$meta_area) %>%
             dplyr::select(area_id, spectrum_region_code),
             by = "area_id"
           )
  
  mfout <- dplyr::select(naomi_mf$mf_out, area_id, sex, age_group)

  indicators <- output$indicators %>%
    dplyr::select(area_id, sex, age_group, quarter_id, indicator_id, 
                  mean, se, median, mode, lower, upper) %>%
    dplyr::left_join(dplyr::select(output$meta_indicator, indicator_id, indicator),
                     by = "indicator_id") %>%
    dplyr::left_join(dplyr::select(output$meta_period, quarter_id, calendar_quarter),
                     by = "quarter_id")
             
  ## Subset to most granular estimates in model frame.
  ## Add ID columns to merge to spectrum_calibration data frame.
  val <- indicators %>%
    dplyr::filter(indicator %in% c("plhiv", "art_num_residents")) %>%
    dplyr::inner_join(mf, by = c("area_id", "sex", "age_group")) %>%
    dplyr::select(area_id, indicator, group_vars, mean)

  val_aggr <- val %>%
    dplyr::group_by_at(c("indicator", group_vars)) %>%
    dplyr::summarise(est_raw = sum(mean)) %>%
    dplyr::ungroup()
  
  spectrum_calibration <- naomi_mf$spectrum_calibration %>%
    dplyr::mutate(age_coarse = dplyr::if_else(
                                        age_group %in% c("00-04", "05-09", "10-14"),
                                        "00-14", "15+")) %>%
    dplyr::left_join(
             val_aggr %>%
             dplyr::filter(indicator == "plhiv") %>%
             dplyr::select(-indicator) %>%
             dplyr::rename(plhiv_raw = est_raw),
             by = group_vars
           ) %>%
    dplyr::left_join(
             val_aggr %>%
             dplyr::filter(indicator == "art_num_residents") %>%
             dplyr::select(-indicator) %>%
             dplyr::rename(art_num_raw = est_raw),
             by = group_vars
           )

  ## Calculate calibration ratios for PLHIV and ART Number
  
  plhiv_aggr_var <- get_spectrum_aggr_var(spectrum_plhiv_calibration_level,
                                          spectrum_plhiv_calibration_strat)
  
  if(length(plhiv_aggr_var) > 0L) {
    
    spectrum_calibration <- spectrum_calibration %>%
      dplyr::group_by_at(plhiv_aggr_var) %>%
      dplyr::mutate(plhiv_calibration = sum(plhiv_spectrum) / sum(plhiv_raw)) %>%
      dplyr::ungroup()
    
  } else {
    spectrum_calibration$plhiv_calibration <- 1.0
  }
  
  artnum_aggr_var <- get_spectrum_aggr_var(spectrum_artnum_calibration_level,
                                           spectrum_artnum_calibration_strat)
  
  
  if(length(artnum_aggr_var) > 0L) {
    
    spectrum_calibration <- spectrum_calibration %>%
      dplyr::group_by_at(artnum_aggr_var) %>%
      dplyr::mutate(art_num_calibration = sum(art_num_spectrum) / sum(art_num_raw)) %>%
      dplyr::ungroup()
    
  } else {
    spectrum_calibration$art_num_calibration <- 1.0
  }
  
  spectrum_calibration <- spectrum_calibration %>%
    dplyr::mutate(plhiv = plhiv_raw * plhiv_calibration,
                  art_num = art_num_raw * art_num_calibration) %>%
    dplyr::select(spectrum_region_code, sex, age_group, calendar_quarter,
                  population_spectrum, population_raw, population_calibration, population,
                  plhiv_spectrum, plhiv_raw, plhiv_calibration, plhiv,
                  art_num_spectrum, art_num_raw, art_num_calibration, art_num)
  

  ## Calculate calibrated PLHIV at finest stratification (mf_model)
  
  val <- val %>%
    dplyr::left_join(
             spectrum_calibration %>%
             dplyr::select(
                      group_vars,
                      plhiv = plhiv_calibration,
                      art_num_residents = art_num_calibration
                    ) %>%
             tidyr::gather(indicator, calibration, plhiv, art_num_residents),
             by = c("indicator", group_vars)
           ) %>%
    dplyr::mutate(adjusted = mean * calibration)
  
  
  .expand <- function(cq, ind) {
    byv <- c("area_id", "sex", "spectrum_region_code", "age_group")
    m <- dplyr::filter(val, calendar_quarter == cq, indicator == ind)
    m <- dplyr::left_join(mf, m, by = byv)
    dplyr::mutate(mfout,
                  calendar_quarter = cq,
                  indicator = ind,
                  adjusted = as.numeric(naomi_mf$A_out %*% m$adjusted))
  }
  
  adj <- dplyr::bind_rows(
                  .expand(naomi_mf$calendar_quarter1, "plhiv"),
                  .expand(naomi_mf$calendar_quarter2, "plhiv"),
                  .expand(naomi_mf$calendar_quarter1, "art_num_residents"),
                  .expand(naomi_mf$calendar_quarter2, "art_num_residents")
                )
  
  byv <- c("indicator", "area_id", "sex", "age_group", "calendar_quarter")
  
  adj <- dplyr::inner_join(
                  dplyr::select(indicators, byv, mean),
                  dplyr::select(adj, byv, adjusted),
                  by = byv
                ) %>%
    dplyr::mutate(
             ratio = adjusted / mean,
             mean = NULL,
             adjusted = NULL
           ) %>%
    tidyr::spread(indicator, ratio) %>%
    dplyr::rename(plhiv_calib = plhiv, artnum_calib = art_num_residents)
  
  out <- indicators %>%
    dplyr::left_join(adj, by = c("area_id", "sex", "age_group", "calendar_quarter")) %>%
    dplyr::mutate(
             calibration = dplyr::case_when(
                                    indicator == "plhiv" ~ plhiv_calib,
                                    indicator == "prevalence" ~ plhiv_calib,
                                    indicator == "art_coverage" ~ artnum_calib / plhiv_calib,
                                    indicator == "art_num_residents" ~ artnum_calib,
                                    indicator == "art_num_attend" ~ artnum_calib,
                                    TRUE ~ 1.0),
             mean = mean * calibration,
             se = se * calibration,
             median = median * calibration,
             mode = mode * calibration,
             lower = lower * calibration,
             upper = upper * calibration
           ) %>%
    dplyr::select(names(output$indicators))
  
  
  ## Save calibration options
  
  calibration_options <- c(output$fit$calibration_options,
                           list(spectrum_plhiv_calibration_level  = spectrum_plhiv_calibration_level,
                                spectrum_plhiv_calibration_strat  = spectrum_plhiv_calibration_strat,
                                spectrum_artnum_calibration_level = spectrum_artnum_calibration_level,
                                spectrum_artnum_calibration_strat = spectrum_artnum_calibration_strat)
                           )

  output$indicators <- out
  output$fit$spectrum_calibration <- spectrum_calibration
  output$fit$calibration_options <- calibration_options
  
  output
}
                
get_spectrum_aggr_var <- function(level, strat) {

  if(!level %in% c("national", "subnational", "none"))
    stop(paste0("Calibration level \"", level, "\" not found."))

  if(level == "none")
    return(character(0))

  
  aggr_vars <- "calendar_quarter"
  
  if(level == "subnational")
    aggr_vars <- c(aggr_vars, "spectrum_region_code")
  
  stratvar <- switch(strat,
                   "age_coarse" = "age_coarse",
                   "sex_age_coarse" = c("sex", "age_coarse"),
                   "age_group" = "age_group",
                   "sex_age_group" = c("sex", "age_group"))

  if(is.null(stratvar))
    stop(paste0("Calibration stratification \"", strat, "\" not found."))
  
  aggr_vars <- c(aggr_vars, stratvar)

  aggr_vars
}
 
