library(dplyr)
library(pdftools)
library(readr)
library(stringr)
library(tidyr)

url <- "https://dhsprogram.com/pubs/pdf/FR319/FR319.pdf"

file <- tempfile(fileext = ".pdf")
download.file(url, file)

#' Tables C.2 (page 492) through C.35 (page 525) contain standard
#' errors for HIV prevalence

file <- pdf_subset(file, 492:525)

raw <- pdf_text(file)

#' ## Annex 1 in pages 32-38 contains Malawi national projections
#' for five year age groups and "special age groups" to 2050

#' ## Annex 2 in pages numbered 39-263 contains population projections
#' for each district 2018-2043.
#' * Corresponds to actual pages 49:273

text <- str_split(raw, "\n")
x <- text[[1]]

parse_page <- function(x) {

  print(x[[1]])
  
  ## Extract district name from the page if exists
  district <- x %>%
    str_extract("Sampling errors: (.*) Malawi DHS") %>%
    str_replace("Sampling errors: ", "") %>%
    str_replace(" sample, Malawi DHS", "")
  district <- district[!is.na(district)]


  ## Get data rows
  data <- str_subset(str_trim(x), "HIV prevalence")
  group <- sub("HIV prevalence \\((.+)\\).*", "\\1", data)

  datar <- sub("HIV prevalence \\(.*\\)", "", data)
  datat <- str_squish(datar) %>%
    str_split(" ") %>%
    lapply(str_replace, ",", "") %>%
    lapply(as.numeric) %>%
    do.call(what = rbind) %>%
    as.data.frame()

  datat$group <- group
  datat$district <- district

  datat
}

res <- lapply(text, parse_page) %>%
  bind_rows() %>%
  rename(
    est = V1,
    se = V2,
    n_unwgt = V3,
    n_wgt = V4,
    deft = V5,
    cv = V6,
    lower = V7,
    upper = V8
  ) %>%
  select(district, group, everything())

write_csv(res, "~/Downloads/mdhs15-district-pevalence.csv")

