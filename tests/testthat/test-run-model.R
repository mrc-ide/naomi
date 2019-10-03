context("test-run-model")

test_that("fit_tmb() returns expected", {
  expect_equal(fit$convergence, 0)
  expect_equal(fit_noartattend$convergence, 0)
})
