context("SPEI News")

test_that("SPEI news works without errors/warnings", {
  expect_error(SPEINews(), NA)
})