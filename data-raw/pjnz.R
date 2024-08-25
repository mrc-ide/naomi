#' This script adds the Malawi 2019 and 2024 Spectrum PJNZ file for testing and
#' demonstration purposes.
#'
#' Files are taken from UNAIDS estimates files produced by national HIV estimates
#' teams for UNAIDS.
#'
#' To access data and estimates for these and other countries, please visit
#' http://aidsinfo.unaids.org/
#' or http://www.unaids.org/en/dataanalysis/datatools/spectrum-epp to request
#' the most recent Spectrum estimates files.
#' 

library(here)
library(zip)

raw <- "~/Data/Spectrum files/2019 final shared/SSA/Malawi_2019_v22_MM_BF.PJNZ"
path <- here("inst/extdata/demo_mwi2019.PJNZ")

file.copy(raw, path)
zip_list(path)

## Remove files to reduce size
## Keep only .PJN and .DP file
utils::zip(path, grep("DP$|PJN$", zip_list(path)$filename, value = TRUE, invert = TRUE), flags="-d")

## Add .shiny90 file into PJNZ

shiny90 <- "~/Data/Spectrum files/2020 final shared/shiny90/malawi_2020_shiny90_outputs_v5.shiny90"
zip_list(shiny90)

shiny90unz <- tempfile()
unzip(shiny90, exdir = shiny90unz)
shiny90tmp <- file.path(tempdir(), "malawi.zip.shiny90")

file.remove(list.files(file.path(shiny90unz, "model_outputs"), "[^par.rds]", full.names = TRUE))

zip(shiny90tmp,
    file.path(shiny90unz, c("spectrum_data", "model_outputs", "country.txt")),
    mode = "cherry-pick")

zip_list(shiny90tmp)

zip_append(path, shiny90tmp, mode = "cherry-pick")
zip_list(path)


#' ## Add 2024 Spectrum PJNZ file

raw24 <- "~/Data/Spectrum files/2024 final shared/Public spectrum files/ESA/Malawi_2024_v11_ART_Num.pjnz"
path24 <- here("inst/extdata/demo_mwi2024.PJNZ")

file.copy(raw24, path24)
zip_list(path24)

## Remove files to reduce size
## Keep only .PJN, .DP, and .shiny90 file

utils::zip(path24, grep("DP$|PJN$|shiny90$", zip_list(path24)$filename, value = TRUE, invert = TRUE), flags="-d")

zip_list(path24)
