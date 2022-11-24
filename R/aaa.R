cache <- new.env(parent = emptyenv())

#' Make a function cache invariant
#'
#' This first time a function from this is called it will run the function
#' itself and save the output in memory. On subsequent calls the in
#' memory cached version will be returned. This cache is separate by
#' language.
#'
#' @param name The name of the function in the cache
#' @param fn The function itself
#'
#' @return Cache invariant version of fn
#' @keywords internal
cache_invariant <- function(name, fn) {
  function() {
    lang <- traduire::translator()$language()
    if (is.null(cache[[name]][[lang]])) {
      if (is.null(cache[[name]])) {
        cache[[name]] <- list()
      }
      cache[[name]][[lang]] <- fn()
    }
    cache[[name]][[lang]]
  }
}
