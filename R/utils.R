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

suppress_one_warning <- function(expr, regexp) {
  withCallingHandlers(expr,
    warning = function(w) {
        if(grepl(regexp, w$message))
          invokeRestart("muffleWarning")
    })
}

suppress_one_message <- function(expr, regexp) {
  withCallingHandlers(expr,
    message = function(w) {
        if(grepl(regexp, w$message))
          invokeRestart("muffleMessage")
    })
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

vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, ..., FUN.VALUE = numeric(1))
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, ..., FUN.VALUE = character(1))
}

is_empty <- function(x) {
  length(x) == 0 || is.null(x) || is.na(x) || !nzchar(x)
}

area_level_from_id <- function(area_ids) {
  ## Area ids are of format <iso3>_<area_level>_<area_num>
  ## so we can split and return the 2nd
  split_ids <- strsplit(area_ids, "_")
  vnapply(split_ids, function(id)  {
    if (length(id) == 1) {
      ## This is our top level ID i.e. level = 0
      level = 0
    } else {
      level = as.numeric(id[2])
    }
    level
  })
}
