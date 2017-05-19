context("kern")


test_that("example w/ defaults", {
  k12Out = readRDS("data/k12_Out.rds")
  expect_equal(k12Out, kern(12))
})


test_that("example w/ defaults", {
  k12GaussOut = readRDS("data/k12_gauss_Out.rds")
  expect_equal(k12GaussOut, kern(12, 'gaussian'))
})