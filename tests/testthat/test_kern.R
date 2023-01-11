context("kern")


test_that("example w/ defaults", {
  k12Out <- readRDS("data/k12_Out.rds")
  expect_equal(k12Out, kern(12))
})


test_that("example w/ defaults", {
  k12GaussOut <- readRDS("data/k12_gauss_Out.rds")
  expect_equal(k12GaussOut, kern(12, "gaussian"))
})


test_that("plot works without errors/warnings", {
  pdf(NULL) # Stops Rplots.pdf from being created
  expect_error(kern.plot(12), NA)
})


test_that("type error", {
  expect_error(
    kern(12, "gaussiann"),
    "type must be one of: rectangular, triangular, circular, gaussian"
  )
})

test_that("shift lower than scale", {
  expect_error(
    kern(12, "gaussian", shift = 13),
    'Parameter "shift" must be lower than "scale"'
  )
})

test_that("negative shift", {
  expect_error(
    kern(12, "gaussian", shift = -13),
    'Parameter "shift" cannot have a negative value'
  )
})
