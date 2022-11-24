test_that("functions can be cached with language", {
  fn <- function() {t_("HIV_PREVALENCE")}
  cached_fn <- cache_invariant("test", fn)
  expect_equal(cached_fn(), fn())
  expect_equal(cached_fn(), "HIV prevalence")

  reset <- naomi_set_language("fr")
  on.exit(reset())
  expect_equal(cached_fn(), fn())
  expect_equal(cached_fn(), "Prévalence du VIH")
  reset()

  reset <- naomi_set_language("pt")
  on.exit(reset())
  expect_equal(cached_fn(), fn())
  expect_equal(cached_fn(), "Prevalência de VIH")

  reset()
  expect_equal(cached_fn(), fn())
  expect_equal(cached_fn(), "HIV prevalence")
})
