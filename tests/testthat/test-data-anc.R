context("data-anc")

test_that("can calculate prevalence and ART coverage from ANC data", {
  path <- system.file("extdata", "demo_anc_testing.csv",
                      package = "naomi")
  anc_data <- read.csv(path, stringsAsFactors = FALSE)
  out <- calculate_prevalence_art_coverage(anc_data)
  expect_equal(colnames(out),
               c(colnames(anc_data), "anc_prevalence", "anc_art_coverage"))
  expect_equal(nrow(out), nrow(anc_data))
})
