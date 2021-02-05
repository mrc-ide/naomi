naomi_write_csv <- function(...) {
  write.csv(..., row.names = FALSE, na = "")
}

naomi_read_csv <- function(file, ..., col_types = readr::cols()) {
  as.data.frame(csv_reader(file, TRUE)(file, ..., col_types = col_types))
}

readr_read_csv <- function(file, ..., col_types = readr::cols()) {
  csv_reader(file, TRUE)(file, ..., col_types = col_types)
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
  write.csv(x, tmp, ..., row.names = row.names)
  paste0(brio::readLines(tmp), collapse = "\n")
}

suppress_one_warning <- function(expr, regexp) {
  withCallingHandlers(expr,
    warning = function(w) {
        if(grepl(regexp, w$message))
          invokeRestart("muffleWarning")
    })
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

naomi_translator_unregister <- function() {
  traduire::translator_unregister()
}
