#!/usr/bin/env Rscript

## Run as ./collect_translations.R 2021-11-17
## Date format is YYYY-MM-DD
args = commandArgs(trailingOnly=TRUE)
if (!is.null(args[1]) && !is.na(args[1])) {
  from_date <- args[1]
} else {
  from_date <- NULL
}

files <- file.path("inst/traduire", list.files("inst/traduire"))

## Helpers for reading file from git archive
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
system3 <- function(command, args) {
  res <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  code <- attr(res, "status") %||% 0
  attr(res, "status") <- NULL
  list(success = code == 0,
       code = code,
       output = res)
}
git_run <- function(args, root = NULL, check = FALSE) {
  if (!is.null(root)) {
    args <- c("-C", root, args)
  }
  res <- system3(git, args)
  if (check && !res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res
}
git_show <- function(path, date) {
  path <- sprintf("HEAD@{%s}:./%s", date, path)
  git <- unname(Sys.which("git"))
  res <- system3(git, c("show", path))
  if (!res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res$output
}

yml <- lapply(files, function(file) {
  lang <- strsplit(basename(file), "-")[[1]][1]
  current_strings <- jsonlite::read_json(file)
  d <- data.frame(names(current_strings),
                  unname(unlist(current_strings)))
  colnames(d) <- c("key", lang)
  d
})

if (!is.null(from_date)) {
  old_yml <- lapply(files, function(file) {
    lang <- strsplit(basename(file), "-")[[1]][1]
    content <- git_show(file, from_date)
    strings <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    d <- data.frame(names(strings),
                    unname(unlist(strings)))
    colnames(d) <- c("key", lang)
    d
  })

  yml <- Map(function(old, current) {
    ## Keep rows from current not in old or if the value has changed
    row.names(current) <- current$key
    row.names(old) <- old$key
    keys_out <- vapply(row.names(current), function(key) {
      !(key %in% row.names(old)) || any(current[key, ] != old[key, ])
    }, logical(1))
    row.names(current) <- NULL
    current[keys_out, ]
  }, old_yml, yml)
}

translations <- Reduce(function(x, y) merge(x, y, by = "key", all = TRUE),
                       yml, accumulate = FALSE)

out <- sprintf("translations-%s.csv", Sys.Date())
message(sprintf("Saving translations to %s", out))
write.csv(translations, out, row.names = FALSE, na = "")
