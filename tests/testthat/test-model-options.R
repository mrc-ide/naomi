context("model-options")

test_that("can get valid model run options template", {
  options <- get_model_options_template()
  expect_type(options, "character")
  expect_length(options, 1)

  ## Basic test that some data has been read
  expect_true(any(grepl("controlSections", options)))
  expect_true(any(grepl("General", options)))
  expect_true(any(grepl("ART", options)))
  expect_true(any(grepl("Advanced options", options)))
  expect_true(any(grepl("<area_scope_options>", options)))
  expect_true(any(grepl("<area_scope_default>", options)))
  expect_true(any(grepl("<area_level_options>", options)))
  expect_true(any(grepl("<art_t1_options>", options)))
  expect_true(any(grepl("<art_t2_options>", options)))
})

