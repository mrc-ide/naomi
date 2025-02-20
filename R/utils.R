naomi_write_csv <- function(...) {
  utils::write.csv(..., row.names = FALSE, na = "")
}

naomi_read_csv <- function(file, ..., col_types = readr::cols()) {
  as.data.frame(csv_reader(file, TRUE)(file, ..., col_types = col_types,
                                       progress = FALSE))
}

readr_read_csv <- function(file, ..., col_types = readr::cols()) {
  csv_reader(file, TRUE)(file, ..., col_types = col_types, progress = FALSE)
}

csv_reader <- function(file, readr = FALSE) {
  header <- brio::readLines(file, 1)
  if (!grepl(",", header) && grepl(";", header)) {
    if (readr) readr::read_csv2 else utils::read.csv2
  } else {
    if (readr) readr::read_csv else utils::read.csv
  }
}

system_file <- function(...) {
  system.file(..., package = "naomi", mustWork = TRUE)
}

write_csv_string <- function(x, ..., row.names = FALSE) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  utils::write.csv(x, tmp, ..., row.names = row.names)
  paste0(brio::readLines(tmp), collapse = "\n")
}

suppress_conditions <- function(expr, message_regexp = NULL,
                                warning_regexp = NULL) {
  handlers <- list()
  if (!is.null(message_regexp)) {
    handlers$message <- function(w) {
      if(grepl(paste(message_regexp, collapse = "|"), w$message)) {
        invokeRestart("muffleMessage")
      }
    }
  }
  if (!is.null(warning_regexp)) {
    handlers$warning <- function(w) {
      if(grepl(paste(warning_regexp, collapse = "|"), w$message)) {
        invokeRestart("muffleWarning")
      }
    }
  }
  with_handlers <- function(...) {
    withCallingHandlers(expr, ...)
  }
  do.call(with_handlers, handlers)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

naomi_translator_unregister <- function() {
  traduire::translator_unregister()
}

squote <- function(x) {
  sprintf("'%s'", x)
}

assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}

match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(squote(choices), collapse = ", ")),
         call. = FALSE)
  }
  arg
}

match_values <- function(args, choices, name = deparse(substitute(args))) {
  for (arg in args) {
    match_value(arg, choices, name)
  }
  args
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, ..., FUN.VALUE = logical(1))
}

is_empty <- function(x) {
  length(x) == 0 || is.null(x) || is.na(x) || !nzchar(x)
}


assert_package_installed <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' must be installed to use this function.",
              package_name),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
