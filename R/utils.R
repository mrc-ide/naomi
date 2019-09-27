naomi_write_csv <- function(...) {
  write.csv(..., row.names = FALSE, na = "")
}

naomi_read_csv <- function(...) {
  read.csv(..., stringsAsFactors = FALSE)
}
