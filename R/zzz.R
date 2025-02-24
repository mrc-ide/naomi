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
