
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
    parameter = c("population_out",
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

  indicator_ids <- c("population_out" = 1,
                     "rho_out" = 2,
                     "plhiv_out" = 3,
                     "alpha_out" = 4,
                     "artnum_out" = 5,
                     "lambda_out" = 6,
                     "infections_out" = 7)

  report <- naomi_fit$obj$report(naomi_fit$par.full)

  get_est <- function(varname) {
    v <- dplyr::mutate(
      mf_out,
      quarter_id = naomi_mf$quarter_id1,
      indicator_id = indicator_ids[varname],
      mode = report[[varname]]
    )
    if(!is.null(naomi_fit$sample)) {
      smp <- naomi_fit$sample[[varname]]
      qtl <- apply(smp, 1, stats::quantile, c(0.5, 0.025, 0.975))
      v$mean <- rowMeans(smp)
      v$se <- sqrt(rowSums((smp - v$mean)^2) / (max(ncol(smp), 2) - 1))
      v$median <- qtl[1,]
      v$lower <- qtl[2,]
      v$upper <- qtl[3,]
    } else {
      v[c("mean", "se", "median", "lower", "upper")] <- NA_real_
    }

    v
  }

  indicators <- lapply(names(indicator_ids), get_est) %>%
    dplyr::bind_rows()

  indicators
}


#' Build output package from fit
#'
#' @param naomi_fit Fitted naomi model
#' @param naomi_mf Naomi model frame
#' @param areas Area data
#'
#' @return List containing output indicators and metadata.
#' @export
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

  meta_period <- data.frame(quarter_id = c(naomi_mf$quarter_id1, naomi_mf$quarter_id2)) %>%
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
             se,
             median,
             lower,
             upper
           )

  indicators
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

  stopifnot(inherits(naomi_output, "naomi_output"))

  dir <- normalizePath(dir)
  if(!file.access(dir, 2) == 0) {
    stop(paste("Directory", dir, "is not writable."))
  }

  path <- file.path(dir, paste0(filename, ".zip"))
  if(file.access(path, 0) == 0 && !overwrite) {
    stop(paste(
      "File", path, "already exists. Set overwrite = TRUE to write output."))
  }


  if(with_labels){
    indicators <- add_output_labels(naomi_output)
  } else {
    indicators <- naomi_output$indicators
  }

  tmpd <- tempfile()
  dir.create(tmpd)
  old <- setwd(tmpd)
  on.exit(setwd(old))
  naomi_write_csv(indicators, "indicators.csv")

  if(!single_csv) {
    naomi_write_csv(naomi_output$meta_area %>%
                      as.data.frame() %>%
                      dplyr::select(-geometry),
                    "meta_area.csv")
    naomi_write_csv(naomi_output$meta_age_group, "meta_age_group.csv")
    naomi_write_csv(naomi_output$meta_period, "meta_period.csv")
    naomi_write_csv(naomi_output$meta_indicator, "meta_indicator.csv")
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

  utils::zip(path, list.files())
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
    indicators = read_csv(file.path(tmpd, "indicators.csv")),
    meta_area = sf::read_sf(file.path(tmpd, "boundaries.geojson")),
    meta_age_group = read_csv(file.path(tmpd, "meta_age_group.csv")),
    meta_period = read_csv(file.path(tmpd, "meta_period.csv")),
    meta_indicator = read_csv(file.path(tmpd, "meta_indicator.csv"))
  )

  class(v) <- "naomi_output"
  v
}
