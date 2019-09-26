context("test-model-frames")

test_that("get_age_group_id_out() returns expected groups", {
  expect_equal(get_age_group_id_out(18), 18)
  expect_equal(get_age_group_id_out(c(18, 28, 29)), c(18:21, 28:29))
  expect_equal(get_age_group_id_out(1:17), 1:29)
  expect_equal(get_age_group_id_out(4:17), c(4:21, 25:29))
})
