#'
#'

library(usethis)

create_package("naomi")
use_mit_license()

use_readme_rmd()

use_git()
git_vaccinate()

use_roxygen_md()
use_readme_rmd()

devtools::document()
use_pipe()

use_data_raw("areas")
use_data_raw("population")
use_data_raw("survey")


use_vignette("data-model", "Naomi data model")

use_package("sf")

use_package("survey", "Suggests")
use_package("rdhs", "Suggests")


#' Folder for external data
dir.create(here::here("inst"))
dir.create(here::here("inst/extdata"))


#' use_r
