context("traduire")

# Hopefully we'll provide a simpler way of doing this...
test_that("translation initialisation calls traduire_register", {
  expect_true("package:naomi" %in% traduire::translator_list())
  expect_error(traduire::translator("package:naomi"), NA)
  traduire::translator_unregister("package:naomi")
  expect_false("package:naomi" %in% traduire::translator_list())
  expect_error(traduire::translator("package:naomi"))
  naomi_init_traduire()
  expect_error(traduire::translator("package:naomi"), NA)
  expect_true("package:naomi" %in% traduire::translator_list())
})

test_that("set language", {
  obj <- traduire::translator("package:naomi")
  expect_equal(obj$language(), "en")
  reset <- naomi_set_language("fr")
  expect_equal(obj$language(), "fr")
  reset()
  expect_equal(obj$language(), "en")
})
