##' Set language to use for naomi messages, etc
##' @title Set naomi language
##' @param language Either "en" or "fr"
##' @export
naomi_set_language <- function(language) {
  traduire::translator_set_language(language)
}

naomi_init_traduire <- function() {
  root <- system.file("traduire", package = "naomi", mustWork = TRUE)
  pattern <- sprintf("%s/{language}-{namespace}.json", root)
  languages <- c("en", "fr")
  namespaces <- "translation"
  traduire::translator_register(NULL, languages[[1]], namespaces[[1]],
                                resource_pattern = pattern,
                                namespaces = namespaces,
                                languages = languages)
}
