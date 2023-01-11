context("spei")

data(wichita)
attach(wichita)
wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
wichita$BAL <- wichita$PRCP - wichita$PET

data(balance)

data(cruts4)

x_vec <- as.numeric(wichita$BAL)
x_tsvec <- ts(x_vec, c(1980, 1), fr = 12)
x_mat <- as.matrix(balance)
x_tsmat <- ts(x_mat, c(1980, 1), fr = 12)
x_array <- cruts4

# Test input data checks and error messages

# spei(x_vec, 1)

test_that("Non-numeric scale", {
  expect_error(
    spei(x_vec, scale = "1"),
    "Argument `scale` must be numeric."
  )
})

test_that("Non-list kernel", {
  expect_error(
    spei(x_vec, 1, kernel = "a"),
    "Argument `kernel` must be a list."
  )
})

test_that("Badly conformed kernel", {
  expect_error(
    spei(x_vec, 1, kernel = list(a = 1, b = 0)),
    "Argument `kernel` must be a list with components `type` and `shift`."
  )
})

test_that("Badly conformed kernel 2", {
  expect_error(
    spei(x_vec, 1, kernel = list(type = 1, shift = 0)),
    "Element `type` of `kernel` must be a single valued character vector"
  )
})

test_that("Bad distribution", {
  expect_error(
    spei(x_vec, 1, distribution = "moments"),
    "Argument `distribution` must be one of `log-Logistic`, `Gamma` or `PearsonIII`."
  )
})

test_that("Bad fit", {
  expect_error(
    spei(x_vec, 1, fit = "logistic"),
    "Argument `fit` must be one of `ub-pwm`, `pp-pwm` or `max-lik`."
  )
})

test_that("Bad ref.start", {
  expect_error(
    spei(x_vec, 1, ref.start = 1),
    "Argument `ref.start` must be a numeric vector of length two."
  )
})

test_that("Bad ref.start 2", {
  expect_error(
    spei(x_vec, 1, ref.start = c("a", "b")),
    "Argument `ref.start` must be a numeric vector of length two."
  )
})

test_that("Bad ref.end", {
  expect_error(
    spei(x_vec, 1, ref.end = 1),
    "Argument `ref.end` must be a numeric vector of length two."
  )
})

test_that("Bad ref.end 2", {
  expect_error(
    spei(x_vec, 1, ref.end = c("a", "b")),
    "Argument `ref.end` must be a numeric vector of length two."
  )
})

test_that("There are NAs when na.rm=FALSE", {
  expect_error(
    spei(c(NA, x_vec), 1),
    "`data` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("Incorrect input dimensions", {
  expect_error(
    spei(array(x_vec, c(382, 2, 2, 2)), 1),
    "Input data can not have more than three dimensions."
  )
})

test_that("Incorrect data type: passing a data.frame", {
  expect_error(
    spei(as.data.frame(x_mat), 1),
    "Bad data type: input must be a vector, tsvector, matrix, tsmatrix, or 3-d array."
  )
})


# Test function results

# out <- spei(x_vec, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_vector.rds')
test_that("example with vector data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_vector.rds"),
    spei(x_vec, 1)$fitted
  )
})

# out <- spei(x_vec, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_vector.rds')
test_that("example with vector data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_vector.rds"),
    spei(x_vec, 12)$fitted
  )
})

# out <- spei(x_tsvec, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_tsvector.rds')
test_that("example with tsvector data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_tsvector.rds"),
    spei(x_tsvec, 1)$fitted
  )
})

# out <- spei(x_tsvec, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_tsvector.rds')
test_that("example with tsvector data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_tsvector.rds"),
    spei(x_tsvec, 12)$fitted
  )
})

# out <- spei(x_mat, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_matrix.rds')
test_that("example with matrix data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_matrix.rds"),
    spei(x_mat, 1)$fitted
  )
})

# out <- spei(x_mat, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_matrix.rds')
test_that("example with matrix data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_matrix.rds"),
    spei(x_mat, 12)$fitted
  )
})

# out <- spei(x_tsmat, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_tsmatrix.rds')
test_that("example with tsmatrix data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_tsmatrix.rds"),
    spei(x_tsmat, 1)$fitted
  )
})

# out <- spei(x_tsmat, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_tsmatrix.rds')
test_that("example with tsmatrix data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_tsmatrix.rds"),
    spei(x_tsmat, 12)$fitted
  )
})

# out <- spei(x_array, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_array.rds')
test_that("example with 3-d array data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_array.rds"),
    spei(x_array, 1)$fitted
  )
})

# out <- spei(x_array, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_array.rds')
test_that("example with 3-d array data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_array.rds"),
    spei(x_array, 12)$fitted
  )
})

rs <- c(10, 1)
re <- c(25, 12)

# out <- spei(x_vec, 12, ref.start=rs, ref.end=re)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_refst_refend.rds')
test_that("example with both ref.start and ref.end", {
  expect_equal(
    readRDS("data/spei_refst_refend.rds"),
    spei(x_vec, 12, ref.start = rs, ref.end = re)$fitted
  )
})

# out <- spei(x_vec, 12, ref.start=rs)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_refst.rds')
test_that("example with only ref.start", {
  expect_equal(
    readRDS("data/spei_refst.rds"),
    spei(x_vec, 12, ref.start = rs)$fitted
  )
})

# out <- spei(x_vec, 12, ref.end=rs)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_refend.rds')
test_that("example with only ref.end", {
  expect_equal(
    readRDS("data/spei_refend.rds"),
    spei(x_vec, 12, ref.end = rs)$fitted
  )
})

# out <- spei(x_vec, 1, distribution='Gamma')$fitted
# saveRDS(out, file='./tests/testthat/data/spei_gamma.rds')
test_that("example with Gamma distribution", {
  expect_equal(
    readRDS("data/spei_gamma.rds"),
    spei(x_vec, 1, distribution = "Gamma")$fitted
  )
})

# out <- spei(x_vec, 1, distribution='PearsonIII')$fitted
# saveRDS(out, file='./tests/testthat/data/spei_pe3.rds')
test_that("example with PearsonIII distribution", {
  expect_equal(
    readRDS("data/spei_pe3.rds"),
    spei(x_vec, 1, distribution = "PearsonIII")$fitted # , tol = 1e-7
  )
})

pars <- spei(x_vec, 1)$coefficients
test_that("example with user-provided parameters", {
  expect_equal(
    readRDS("data/spei_1_vector.rds"),
    spei(x_vec, 1, params = pars)$fitted
  )
})

# tests for different calculation methods: pp-pwm, max-lik

test_that("plot works without errors/warnings", {
  pdf(NULL) # Stops Rplots.pdf from being created
  spei1 <- spei(x_vec, 1)
  expect_error(plot(spei1), NA)
})
