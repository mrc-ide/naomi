expect_no_error <- function(object) {
  expect_error(object, NA)
}

with_mock <- function(..., .parent = parent.frame()) {
  mockr::with_mock(..., .parent = .parent, .env = "naomi")
}
