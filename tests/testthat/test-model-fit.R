context("test-model-fit")

test_that("setting rng_seed returns same outputs", {

  a_fit_sample2 <- sample_tmb(a_fit, nsample = 30, rng_seed = 28)
  a_output2 <- output_package(a_fit_sample2, a_naomi_mf, area_merged)

  expect_equal(a_fit_sample$sample, a_fit_sample2$sample)
  expect_equal(a_output$indicators$mean, a_output2$indicators$mean)
})

test_that("setting different rng_seed returns different output", {

  a_fit_sample3 <- sample_tmb(a_fit, nsample = 30, rng_seed = 1)
  expect_true(
    a_fit_sample$sample$artnum_t1_out[1] !=
    a_fit_sample3$sample$artnum_t1_out[1]
  )

  a_fit_sample_null <- sample_tmb(a_fit, nsample = 30)
  expect_true(
    a_fit_sample$sample$artnum_t1_out[1] !=
    a_fit_sample_null$sample$artnum_t1_out[1]
  )
})

test_that("exceeding maximum iterations throws a warning", {
  expect_warning(fit_tmb(a_tmb_inputs, outer_verbose = FALSE, max_iter = 5),
                 "iteration limit reached")
})

