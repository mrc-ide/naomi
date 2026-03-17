test_that("can calculate dervied columns from ANC data", {
  path <- system_file("extdata", "demo_anc_testing.csv")
  anc_data <- read_anc_testing(path)
  out <- anc_prepare_hintr(anc_data)
  expect_equal(colnames(out),
               c(colnames(anc_data),
                 "anc_prevalence", "anc_art_coverage", "sample_size"))
  expect_equal(nrow(out), nrow(anc_data))
  expect_true(!all(out$sample_size == 0))
})


test_that("can calculate dervied columns from ART data", {
  path <- system_file("extdata", "demo_art_number.csv")
  art_data <- readr::read_csv(path, show_col_types = FALSE)
  out <- art_prepare_hintr(art_data)
  expect_equal(colnames(out), c(colnames(art_data), "sample_size"))
  expect_true(!all(out$sample_size == 0))
})
