#' Migrate model options
#'
#' Migrate list of options from specified version to current version of naomi.
#'
#' @param options List of options to be migrated.
#' @param version Version the options are from.
#'
#' @return The migrated model options.
#' @export
#'
migrate_model_options <- function(options, version) {
  migration_functions <- get_required_migrations(version)
  browser()
  lapply(migration_functions, function(func) {
    options <- func(options)
  })
  options
}

get_required_migrations <- function(version) {
  available <- dir(system_file("migrations"), pattern = "^([0-9]+\\.){3}R$",
               full.names = TRUE)
  names(available) <- sub("\\.R$", "", basename(available))
  available[order(numeric_version(names(available)))]
  available[numeric_version(version) >= numeric_version(names(available))]
}

source_to_function <- function(filename, name, parent = topenv()) {
  e <- new.env(parent = parent)
  sys.source(filename, e)
  ret <- e[[name]]
  assert_is(ret, "function", sprintf("'%s' within '%s'", name, filename))
  ret
}

check_options_valid <- function(options, options_template) {
  ## For each option check that it shows up in one of the tempaltes as a "name"
  ## If there are multiple then type is multiselect
  ## No required options are missing
  ## No additional options are present
}
