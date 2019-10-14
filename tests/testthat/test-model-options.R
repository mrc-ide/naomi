context("model-options")

test_that("can get valid model run options", {
  options <- get_model_run_options()
  expect_type(options, "character")

  ## Basic test that some data has been read
  response <- jsonlite::parse_json(options)
  expect_equal(names(response), "controlSections")
  expect_length(response$controlSections, 3)
  expect_equal(response$controlSections[[1]]$label, "General")
  expect_equal(response$controlSections[[2]]$label, "ART")
  expect_equal(response$controlSections[[3]]$label, "Advanced options")
})

test_that("invalid model run options returns an error", {
  mock_read_schema <- mockery::mock('["invalid_json"]')
  with_mock("naomi:::read_schema" = mock_read_schema, {
    expect_error(get_model_run_options(), class = "validation_error")
  })
})

