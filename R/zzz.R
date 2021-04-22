##' @importFrom traduire t_
.onLoad <- function(...) {
  naomi_init_traduire() # nocov
}

naomi_init_traduire <- function() {
  root <- system.file("traduire", package = "naomi", mustWork = TRUE)
  pattern <- sprintf("%s/{language}-{namespace}.json", root)
  languages <- c("en", "fr")
  namespaces <- "translation"
  traduire::translator_register(resources = NULL,
                                language = languages[[1]],
                                default_namespace = namespaces[[1]],
                                resource_pattern = pattern,
                                namespaces = namespaces,
                                fallback = "en",
                                languages = languages)
}
