
meta_indicator <-
  data.frame(
    indicator_id = 1:7,
    indicator_label = c("Population",
                        "HIV Prevalence",
                        "PLHIV",
                        "ART Coverage",
                        "ART Number",
                        "HIV Incidence",
                        "New Infections"),
    description = c("Population size",
                    "Proportion of total population HIV positive",
                    "Number of people living with HIV",
                    "Proportion of PLHIV on ART (residents)",
                    "Number on ART (residents)",
                    "HIV incidence rate per year",
                    "Number of new infections per year"),
    parameter = c(NA,
                  "rho_out",
                  "plhiv_out",
                  "alpha_out",
                  "artnum_out",
                  "lambda_out",
                  "infections_out"),
    format = NA,
    scale = NA,
    stringsAsFactors = FALSE
  )


extract_indicators <- function(naomi_fit, naomi_mf) {

  mf_out <- naomi_mf$mf_out
  
  if(is.null(naomi_fit$sdreport)) {
    report <- naomi_fit$obj$report(naomi_fit$par.full)
    mode <- lapply(c("rho_out", "plhiv_out",
                     "alpha_out", "artnum_out",
                     "lambda_out", "infections_out"),
                   function(s) setNames(report[[s]], rep(s, length(report[[s]])))
                   ) %>%
      unlist()
    mean <- NA
    lower <- NA
    upper <- NA
  } else {
    mode <- naomi_fit$sdreport$value
    se <- naomi_fit$sdreport$sd
    mean <- naomi_fit$sdreport$unbiased$value
    lower <- mean - qnorm(0.975) * se
    upper <- mean + qnorm(0.975) * se
  }


  get_est <- function(s, indicator_id, quarter_id) {
    idx <- which(names(mode) == s)
    dplyr::mutate(mf_out,
                  quarter_id,
                  indicator_id,
                  mode = mode[idx],
                  mean = mean[idx],
                  lower = lower[idx],
                  upper = upper[idx])
  }
  
   indicators <-
     bind_rows(
       mf_out %>% dplyr::mutate(
                           quarter_id = naomi_mf$quarter_id1,
                           indicator_id = 1L,
                           mode = as.vector(naomi_mf$A_out %*% naomi_mf$mf_model$population_t1),
                       mean = mode,
                       lower = mode,
                       upper = mode
                       ),
       get_est("rho_out",        2L, naomi_mf$quarter_id1),
       get_est("plhiv_out",      3L, naomi_mf$quarter_id1),
       get_est("alpha_out",      4L, naomi_mf$quarter_id1),
       get_est("artnum_out",     5L, naomi_mf$quarter_id1),
       get_est("lambda_out",     6L, naomi_mf$quarter_id1),
       get_est("infections_out", 7L, naomi_mf$quarter_id1)
     )
  
  indicators
}


output_package <- function(naomi_fit, naomi_mf, areas) {

  indicators <- extract_indicators(naomi_fit, naomi_mf)
  meta_area <- data.tree::ToDataFrameTree(areas$tree, traversal = "level",
                                          "area_level", "area_level_label",
                                          "area_id", "area_name",
                                          "area_sort_order",
                                          "center_x", "center_y") %>%
    dplyr::mutate(levelName = NULL,
                  geometry = areas$boundaries[area_id]) %>%
    sf::st_as_sf()
  
  meta_period <- data.frame(quarter_id = c(quarter_id1, quarter_id2)) %>%
    mutate(quarter_label = naomi::quarter_year_labels(quarter_id))

  meta_age_group <- get_age_groups()
  
  val <- list(
    indicators = indicators,
    meta_area = meta_area,
    meta_age_group = meta_age_group,
    meta_period = meta_period,
    meta_indicator = meta_indicator
  )

  class(val) <- "naomi_output"

  val
}


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
             dplyr::select(age_group_id, age_group_label, age_group_sort_order),
             by = "age_group_id"
           ) %>%
    dplyr::left_join(naomi_output$meta_period, by = "quarter_id") %>%
    dplyr::left_join(
             naomi_output$meta_indicator %>%
             dplyr::select(indicator_id, indicator_label),
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
             age_group_id,
             age_group_label,
             quarter_id,
             quarter_label,
             indicator_id,
             indicator_label,
             mode,
             mean,
             lower,
             upper
           )

  indicators
}

save_output_package <- function(naomi_output,
                                filename,
                                dir,
                                overwrite = FALSE,
                                with_labels = FALSE,
                                boundary_format = "geojson",
                                with_data = FALSE,
                                single_csv = FALSE) {

  stopifnot(inherits(naomi_output, "naomi_output"))
  
  if(!file.access(dir, 2) == 0)
    stop(paste("Directory", dir, "is not writable."))
  
  path <- file.path(dir, paste0(filename, ".zip"))
  if(file.access(path, 0) == 0 && !overwrite)
    stop(paste("File", path, "already exists. Set overwrite = TRUE to write output."))
  
  
  if(with_labels){
    indicators <- add_output_labels(naomi_output)
  } else {
    indicators <- naomi_output$indicators
  }

  tmpd <- tempfile()
  dir.create(tmpd)
  old <- setwd(tmpd)
  on.exit(setwd(old))

  
   write.csv(indicators, "indicators.csv", row.names = FALSE, na = "")
  
  
  if(!single_csv) {
    write.csv(naomi_output$meta_area %>%
              as.data.frame() %>%
              dplyr::select(-geometry),
              "meta_area.csv", row.names = FALSE, na = "")
    write.csv(naomi_output$meta_age_group,
              "meta_age_group.csv", row.names = FALSE, na = "")
    write.csv(naomi_output$meta_period,
              "meta_period.csv", row.names = FALSE, na = "")
    write.csv(naomi_output$meta_indicator,
              "meta_indicator.csv", row.names = FALSE, na = "")
    if(!is.null(boundary_format) && !is.na(boundary_format)) {
      if(boundary_format == "geojson") {
        st_write(naomi_output$meta_area, "boundaries.geojson")
      } else if(boundary_format == "shp") {
        dir.create("shp")
        st_write(naomi_output$meta_area, "shp/boundaries.shp")
      } else {
        stop(paste("Boundary file format", boundary_format, "not recognized.",
                   "Please select 'geojson', 'shp', or NA to not save boundaries."))
      }
    }
  } 

  zip(path, list.files())

  path
}
