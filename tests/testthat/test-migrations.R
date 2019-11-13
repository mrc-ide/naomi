context("migrations")

test_that("migrations work as expected", {
  options <- readRDS(system_file("migrations", "reference", "0.0.11.RDS"))
  migrated_options <- migrate_model_options(options, "0.0.11")

  expect_equal(options, migrated_options)
  expect_null(migrated_options$extra_field)

  migrated_options <- migrate_model_options(options, "0.0.12")
  expect_false(is.null(migrated_options$extra_field))
  expect_equal(migrated_options$extra_field, "test")

  migrated_options <- migrate_model_options(options, "0.0.13")
  expect_equal(options, migrated_options)
  expect_null(migrated_options$extra_field)
})

test_that("all migrations work as expected", {

})
