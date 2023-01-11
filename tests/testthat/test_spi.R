context("spi")

data(wichita)
attach(wichita)

x_vec <- as.numeric(wichita$PRCP)
x_tsvec <- ts(x_vec, c(1980, 1), fr = 12)
x_mat <- matrix(x_vec, nrow = length(x_vec), ncol = 4)
x_tsmat <- ts(x_mat, c(1980, 1), fr = 12)
x_array <- array(x_vec, dim = c(length(x_vec), 2, 2))

# Test input data checks and error messages

# spi(x_vec, 1)

test_that("Non-numeric scale", {
  expect_error(
    spi(x_vec, scale = "1"),
    "Argument `scale` must be numeric."
  )
})

test_that("Bad distribution", {
  expect_error(
    spi(x_vec, 1, distribution = "moments"),
    "Argument `distribution` must be one of `log-Logistic`, `Gamma` or `PearsonIII`."
  )
})

test_that("Bad fit", {
  expect_error(
    spi(x_vec, 1, fit = "logistic"),
    "Argument `fit` must be one of `ub-pwm`, `pp-pwm` or `max-lik`."
  )
})

test_that("Bad ref.start", {
  expect_error(
    spi(x_vec, 1, ref.start = 1),
    "Argument `ref.start` must be a numeric vector of length two."
  )
})

test_that("Bad ref.start 2", {
  expect_error(
    spi(x_vec, 1, ref.start = c("a", "b")),
    "Argument `ref.start` must be a numeric vector of length two."
  )
})

test_that("Bad ref.end", {
  expect_error(
    spi(x_vec, 1, ref.end = 1),
    "Argument `ref.end` must be a numeric vector of length two."
  )
})

test_that("Bad ref.end 2", {
  expect_error(
    spi(x_vec, 1, ref.end = c("a", "b")),
    "Argument `ref.end` must be a numeric vector of length two."
  )
})

test_that("There are NAs when na.rm=FALSE", {
  expect_error(
    spi(c(NA, x_vec), 1),
    "`data` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("Incorrect input dimensions", {
  expect_error(
    spi(array(x_vec, c(382, 2, 2, 2)), 1),
    "Input data can not have more than three dimensions."
  )
})

test_that("Incorrect data type: passing a data.frame", {
  expect_error(
    spi(as.data.frame(x_mat), 1),
    "Bad data type: input must be a vector, tsvector, matrix, tsmatrix, or 3-d array."
  )
})


# Test function results

# out <- spi(x_vec, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_1_vector.rds')
test_that("example with vector data, scale 1", {
  expect_equal(
    readRDS("data/spi_1_vector.rds"),
    spi(x_vec, 1)$fitted
  )
})

# out <- spi(x_vec, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_12_vector.rds')
test_that("example with vector data, scale 12", {
  expect_equal(
    readRDS("data/spi_12_vector.rds"),
    spi(x_vec, 12)$fitted
  )
})

# out <- spi(x_tsvec, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_1_tsvector.rds')
test_that("example with tsvector data, scale 1", {
  expect_equal(
    readRDS("data/spi_1_tsvector.rds"),
    spi(x_tsvec, 1)$fitted
  )
})

# out <- spi(x_tsvec, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_12_tsvector.rds')
test_that("example with tsvector data, scale 12", {
  expect_equal(
    readRDS("data/spi_12_tsvector.rds"),
    spi(x_tsvec, 12)$fitted
  )
})

# out <- spi(x_mat, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_1_matrix.rds')
test_that("example with matrix data, scale 1", {
  expect_equal(
    readRDS("data/spi_1_matrix.rds"),
    spi(x_mat, 1)$fitted
  )
})

# out <- spi(x_mat, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_12_matrix.rds')
test_that("example with matrix data, scale 12", {
  expect_equal(
    readRDS("data/spi_12_matrix.rds"),
    spi(x_mat, 12)$fitted
  )
})

# out <- spi(x_tsmat, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_1_tsmatrix.rds')
test_that("example with tsmatrix data, scale 1", {
  expect_equal(
    readRDS("data/spi_1_tsmatrix.rds"),
    spi(x_tsmat, 1)$fitted
  )
})

# out <- spi(x_tsmat, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_12_tsmatrix.rds')
test_that("example with tsmatrix data, scale 12", {
  expect_equal(
    readRDS("data/spi_12_tsmatrix.rds"),
    spi(x_tsmat, 12)$fitted
  )
})

# out <- spi(x_array, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_1_array.rds')
test_that("example with 3-d array data, scale 1", {
  expect_equal(
    readRDS("data/spi_1_array.rds"),
    spi(x_array, 1)$fitted
  )
})

# out <- spi(x_array, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_12_array.rds')
test_that("example with 3-d array data, scale 12", {
  expect_equal(
    readRDS("data/spi_12_array.rds"),
    spi(x_array, 12)$fitted
  )
})

rs <- c(10, 1)
re <- c(25, 12)

# out <- spi(x_vec, 12, ref.start=rs, ref.end=re)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_refst_refend.rds')
test_that("example with both ref.start and ref.end", {
  expect_equal(
    readRDS("data/spi_refst_refend.rds"),
    spi(x_vec, 12, ref.start = rs, ref.end = re)$fitted
  )
})

# out <- spi(x_vec, 12, ref.start=rs)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_refst.rds')
test_that("example with only ref.start", {
  expect_equal(
    readRDS("data/spi_refst.rds"),
    spi(x_vec, 12, ref.start = rs)$fitted
  )
})

# out <- spi(x_vec, 12, ref.end=rs)$fitted
# saveRDS(out, file='./tests/testthat/data/spi_refend.rds')
test_that("example with only ref.end", {
  expect_equal(
    readRDS("data/spi_refend.rds"),
    spi(x_vec, 12, ref.end = rs)$fitted
  )
})

# out <- spi(x_vec, 1, distribution='Gamma')$fitted
# saveRDS(out, file='./tests/testthat/data/spi_gamma.rds')
test_that("example with Gamma distribution", {
  expect_equal(
    readRDS("data/spi_gamma.rds"),
    spi(x_vec, 1, distribution = "Gamma")$fitted
  )
})

# out <- spi(x_vec, 1, distribution='PearsonIII')$fitted
# saveRDS(out, file='./tests/testthat/data/spi_pe3.rds')
test_that("example with PearsonIII distribution", {
  expect_equal(
    readRDS("data/spi_pe3.rds"),
    spi(x_vec, 1, distribution = "PearsonIII")$fitted # , tol = 1e-7
  )
})

pars <- spi(x_vec, 1)$coefficients
test_that("example with user-provided parameters", {
  expect_equal(
    readRDS("data/spi_1_vector.rds"),
    spi(x_vec, 1, params = pars)$fitted
  )
})
