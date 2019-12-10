context("traduire")

# Hopefully we'll provide a simpler way of doing this...
test_that("translation initialisation calls traduire_register", {
  expect_true("package:naomi" %in% traduire::translator_list())
  expect_error(naomi_translator(), NA)
  naomi_translator_unregister()
  expect_false("package:naomi" %in% traduire::translator_list())
  expect_error(naomi_translator())
  naomi_init_traduire()
  expect_error(naomi_translator(), NA)
  expect_true("package:naomi" %in% traduire::translator_list())
})
