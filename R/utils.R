naomi_write_csv <- function(...) {
  write.csv(..., row.names = FALSE, na = "")
}

naomi_read_csv <- function(...) {
  read.csv(..., stringsAsFactors = FALSE)
}

readr_read_csv <- function(..., col_types = readr::cols()) {
  readr::read_csv(..., col_types = col_types)
}

system_file <- function(...) {
  system.file(..., package = "naomi", mustWork = TRUE)
}

write_csv_string <- function(x, ..., row.names = FALSE) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  write.csv(x, tmp, ..., row.names = row.names)
  paste0(readLines(tmp), collapse = "\n")
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
