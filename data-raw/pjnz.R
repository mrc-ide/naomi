devtools::load_all()
library(here)

#' This script adds the Malawi 2019 Spectrum PJNZ file for testing and demonstration purposes.
#' Files are taken from UNAIDS estimates files produced by national HIV estimates teams for UNAIDS.
#'
#' To access data and estimates for these and other countries, please visit http://aidsinfo.unaids.org/
#' or http://www.unaids.org/en/dataanalysis/datatools/spectrum-epp to request the most recent Spectrum
#' estimates files.

raw <- "~/Data/Spectrum files/2019 final shared/SSA/Malawi_2019_v22_MM_BF.PJNZ"
path <- here("inst/extdata/demo_mwi2019.PJNZ")

file.copy(raw, path)
unzip(path, list=TRUE)

## Remove files to reduce size
## Keep only .PJN and .DP file
zip(path, grep("DP$|PJN$", unzip(path, list=TRUE)$Name, value = TRUE, invert = TRUE), flags="-d")

unzip(path, list=TRUE)
