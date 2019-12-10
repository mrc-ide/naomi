naomi_set_language <- function(language) {
  traduire::translator_set_language(language, package = "naomi")
}

naomi_translator <- function() {
  traduire::translator()
}

naomi_translator_unregister <- function() {
  traduire::translator_unregister()
}
