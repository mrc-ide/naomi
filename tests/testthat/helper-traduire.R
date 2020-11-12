naomi_set_language <- function(language) {
  traduire::translator_set_language(language, package = "naomi")
}

naomi_translator <- function() {
  traduire::translator(package = "naomi")
}
