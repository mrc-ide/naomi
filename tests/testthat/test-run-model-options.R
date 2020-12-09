context("run-model-options")

test_that("model test options return expected frames", {

  expect_true(all(vapply(a_hintr_data, file.exists, logical(1))))
  expect_true(all(!duplicated(names(a_hintr_options))))

  ndata <- naomi_prepare_data(format_data_input(a_hintr_data),
                              format_options(a_hintr_options))
  ninputs <- prepare_tmb_inputs(ndata)

  expect_equal(nrow(ninputs$data$Xgamma), sum(ndata$mf_areas$n_neighbors+1))
  expect_equal(ncol(ninputs$data$Xgamma), nrow(ndata$mf_areas))
  expect_equal(nrow(ninputs$data$Xgamma_t2), sum(ndata$mf_areas$n_neighbors+1))
  expect_equal(ncol(ninputs$data$Xgamma_t2), 0)
})
