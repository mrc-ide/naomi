context("info")

test_that("can get naomi info", {
  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  expect_equal(names(info), c("inputs.csv", "options.yml", "packages.csv"))
})

test_that("naomi package info", {
  info <- naomi_info_packages()
  expect_equal(colnames(info), c("name", "version", "type"))
  expect_true(nrow(info) > 50)
  expect_true("R" %in% info$name)
})

test_that("naomi_info_input contains filename and hash info", {
  data <- list(
    pjnz = list(
      path = system_file("extdata/demo_mwi2019.PJNZ"),
      hash = "pjnz_hash",
      filename = "demo_mwi2019.PJNZ"
    ),
    population = list(
      path = system_file("extdata/demo_population_agesex.csv"),
      hash = "population_hash",
      filename = "demo_population_agesex.csv"
    ),
    art = NULL
  )

  info <- naomi_info_input(data)

  expect_equal(nrow(info), 3)
  expect_equal(info, data.frame(
    role = c("pjnz", "population", "art"),
    filename = c("demo_mwi2019.PJNZ", "demo_population_agesex.csv", NA),
    md5sum = c("pjnz_hash", "population_hash", NA),
    stringsAsFactors = FALSE
  ))
})
