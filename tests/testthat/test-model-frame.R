context("test-model-frames")

test_that("get_age_group_id_out() returns expected groups", {
  expect_equal(get_age_group_id_out(18), 18)
  expect_equal(get_age_group_id_out(c(18, 28, 29)), c(18:21, 28:29))
  expect_equal(get_age_group_id_out(1:17), 1:29)
  expect_equal(get_age_group_id_out(4:17), c(4:21, 25:29))
})

test_that("artnum_mf() returns expected number of records", {
  expect_equal(nrow(artnum_mf(465, NULL, naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf(NULL, mwi_art_number, naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf(465, NULL, naomi_mf)), 0L)
  expect_named(artnum_mf(NULL, mwi_art_number, naomi_mf), 
               c("area_id", "sex", "age_group_id", "artnum_idx", "current_art"))
  expect_equal(nrow(artnum_mf(465, mwi_art_number, naomi_mf)), 64L)
  expect_named(artnum_mf(465, mwi_art_number, naomi_mf), 
               c("area_id", "sex", "age_group_id", "artnum_idx", "current_art"))
})

test_that("artnum_mf() throws errors for invalid inputs", {
  expect_error(artnum_mf(100, mwi_art_number, naomi_mf))
  expect_error(artnum_mf(465, mwi_art_number, "jibberish"))
  expect_error(artnum_mf(c(465, 466), mwi_art_number, "jibberish"))
})
