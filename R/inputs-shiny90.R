#' Export proportion aware by five year age group from Shiny90
#'
#' Export estimates for proportion aware of status from a Shiny90
#' output file to five-year age groups 15-19 to 50+.
#'
#' @param shiny90_path file path to .shiny90 digest file.
#' @param pjnz_path file path to PJNZ file
#' @param years year(s) to generate estimates; an integer or a vector of integers.
#'   If NULL, all years available in estimates are returned (default).
#'
#' @return A data frame consisting of the number of PLHIV, aware of status
#'   and on ART by sex and five year age group 15-49 and age 50+ for
#'   specified `years`.
#'
#'
#' @details
#'
#' The 'artnum' divided by 'plhiv' columns in the output will give
#' a different ART coverage than Spectrum output for the same year
#' because these outputs are based on the internal mid-year ART
#' coverage in Spectrum, while Spectrum reports an end of year (Dec 31)
#' ART coverage.
#'
#' @examples
#'
#' pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
#' shiny90dir <- tempfile()
#' utils::unzip(pjnz, "malawi.zip.shiny90", exdir = shiny90dir)
#' shiny90_path <- file.path(shiny90dir, "malawi.zip.shiny90")
#'
#' extract_shiny90_age_sex(shiny90_path, pjnz, year = 2010:2019)
#'
#' @export
#'
extract_shiny90_age_sex <- function(shiny90_path, pjnz_path = NULL, years = NULL) {

  tmpd <- tempfile()
  on.exit(unlink(tmpd))

  utils::unzip(shiny90_path, exdir = tmpd)

  has_pjnz <- !is.null(pjnz_path) && file.exists(pjnz_path)

  if (file.exists(file.path(tmpd, "country.txt"))) {
      name <- brio::readLines(file.path(tmpd, "country.txt"))[1]
  } else if (has_pjnz) {
    name <- eppasm::read_country(pjnz_path)
    pjnz_region <- eppasm::read_region(pjnz_path)
    if (!is.null(pjnz_region)) {
      name <- paste0(name, " - ", pjnz_region)
    }
  } else {
    stop("PJNZ file required for .shiny90 created by Spectrum")
  }

  spectrum_data <- list.files(file.path(tmpd, "spectrum_data"), "rds$", full.names = TRUE)
  if (length(spectrum_data) > 0) {
    spec <- lapply(spectrum_data, readRDS)
    spec <- lapply(spec, "[[", "data")
  } else if (has_pjnz) {
    spec <- list(first90::extract_pjnz(pjnz_path))
  } else {
    stop("PJNZ file required for .shiny90 created by Spectrum")
  }
  
  fp <- first90::prepare_inputs_from_extracts(spec)

  if (!exists("popadjust", fp)) {
    ## From first90 v1.5.0 (for 2022 estimates, popadjust is read from the 
    ## PJNZ and saved in the .shiny90 output. This is retained for backward 
    ## compatibility with .shiny90 files created before first90 v1.5.0.
    fp$popadjust <- FALSE
  }

  proj_years <- fp$ss$proj_start + seq_len(fp$ss$PROJ_YEARS) - 1L

  if (is.null(years)) {
    years <- proj_years
  }

  if (!all(years %in% proj_years)) {
    stop("Ouput years not contained in shiny90 projection: ",
         paste0(setdiff(years, proj_years), collapse = ", "))
  }

  if (file.exists(file.path(tmpd, "model_outputs/par.rds"))) {
    par <- readRDS(file.path(tmpd, "model_outputs/par.rds"))
  } else if (has_pjnz) {
    par <- as.numeric(readr_read_csv(file.path(tmpd, "model_outputs/par.csv"), col_names = FALSE))
  } else {
    stop("PJNZ file required for .shiny90 created by Spectrum")
  }

  fpsim <- first90::create_hts_param(par, fp)
  mod <- first90::simmod(fpsim)

  age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-99")

  val <- expand.grid(area = name,
                     year = years,
                     sex = c("female", "male"),
                     agegr = age_groups,
                     hivstatus = "positive",
                     stringsAsFactors = FALSE)
  df <- first90::add_ss_indices(val, fp$ss)

  plhiv <- numeric(nrow(df))
  artnum  <- numeric(nrow(df))
  aware <- numeric(nrow(df))

  for (i in seq_along(df$haidx)) {
    haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
    sidx <- if (df$sidx[i] == 0) { 1:2 } else { df$sidx[i] }
    paidx <- fp$ss$agfirst.idx[df$haidx[i]] + 1:sum(fp$ss$h.ag.span[haidx]) - 1L

    artnum[i] <- sum(attr(mod, "artpop")[ , , df$haidx[i] + 1:df$hagspan[i] - 1, sidx, df$yidx[i]])
    plhiv[i] <- artnum[i] + sum(attr(mod, "hivpop")[ , df$haidx[i] + 1:df$hagspan[i] - 1, sidx, df$yidx[i]])
    aware[i] <- artnum[i] + sum(attr(mod, "diagnpop")[, df$haidx[i] + 1:df$hagspan[i] - 1, sidx, df$yidx[i]])
  }

  val$plhiv <- plhiv
  val$aware <- aware
  val$artnum <- artnum

  val
}
