context("hargreaves")

data(wichita)
attach(wichita)

# Test input data checks and error messages

test_that("No Tmin provided", {
  expect_error(
    hargreaves(Tmax = TMAX, lat = 37.6475),
    'argument "Tmin" is missing, with no default'
  )
})

test_that("No Tmax provided", {
  expect_error(
    hargreaves(Tmin = TMIN, lat = 37.6475),
    'argument "Tmax" is missing, with no default'
  )
})

test_that("No Ra or lat provided", {
  expect_error(
    hargreaves(TMIN, TMAX),
    "One of `Ra` or `lat` must be provided."
  )
})

test_that("na.rm set to a weird value", {
  expect_error(
    hargreaves(TMIN, TMAX, lat = 37.6475, na.rm = 25),
    "Argument `na.rm` must be set to either TRUE or FALSE."
  )
})

test_that("NAs in Tmin", {
  expect_error(
    hargreaves(c(NA, TMIN), c(1, TMAX), lat = 37.6475, na.rm = FALSE),
    "`Tmin` and `Tmax` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in Tmax", {
  expect_error(
    hargreaves(c(1, TMIN), c(NA, TMAX), lat = 37.6475, na.rm = FALSE),
    "`Tmin` and `Tmax` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in Tmin and Tmax", {
  expect_error(
    hargreaves(c(NA, TMIN), c(NA, TMAX), lat = 37.6475, na.rm = FALSE),
    "`Tmin` and `Tmax` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in Ra", {
  expect_error(
    hargreaves(TMIN, TMAX, Ra = TMAX * NA, na.rm = FALSE),
    "`Ra` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in PRe", {
  expect_error(
    hargreaves(TMIN, TMAX, Pre = TMAX * NA, na.rm = FALSE),
    "`Pre` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in lat", {
  expect_error(
    hargreaves(TMIN, TMAX, lat = NA, na.rm = TRUE),
    "`lat` cannot be missing."
  )
})

nt <- nrow(wichita)
tmin <- array(TMIN, dim = c(nt, 2, 2, 2))
tmax <- array(TMAX, dim = c(nt, 2, 2, 2))
test_that("input more than three dimensions", {
  expect_error(
    hargreaves(tmin, tmax, lat = 37.6475),
    "Input data can not have more than three dimensions."
  )
})

test_that("Tmin and Tmax different size", {
  expect_error(
    hargreaves(c(1, TMIN), TMAX, lat = 37.6475),
    "`Tmin` and `Tmax` cannot have different lengths."
  )
})

test_that("Ra incorrect size", {
  expect_error(
    hargreaves(TMIN, TMAX, Ra = c(1, TMIN)),
    "`Ra` has incorrect length."
  )
})

test_that("Pre incorrect size", {
  expect_error(
    hargreaves(TMIN, TMAX, Pre = c(1, TMIN)),
    "`Pre` has incorrect length."
  )
})

nt <- nrow(wichita)
tmin <- array(TMIN, dim = c(nt, 2, 2))
tmax <- array(TMAX, dim = c(nt, 2, 2))
test_that("lat length does not correspond with number of sites", {
  expect_error(
    hargreaves(tmin, tmax, lat = 37.6475),
    "`lat` has incorrect length."
  )
})

# test_that('TS wrong frequency', {
#   expect_error(hargreaves(ts(TMIN, frequency=4),ts(TMAX, frequency=4),
#                 lat=37.6475),
#                'Error: Data should be a monthly time series*'
#   )
# })


# Test function results

# out <- hargreaves(TMIN, TMAX, lat=37.6475)
# saveRDS(out, file='./tests/testthat/data/hargreaves_out_lat.rds')
test_that("example with lat", {
  expect_equal(
    readRDS("data/hargreaves_out_lat.rds"),
    hargreaves(TMIN, TMAX, lat = 37.6475)
  )
})

# out <- hargreaves(TMIN, TMAX, Ra=TMAX)
# saveRDS(out, file='./tests/testthat/data/hargreaves_out_ra.rds')
test_that("example with Ra", {
  harModOut <- readRDS("data/hargreaves_out_ra.rds")
  expect_equal(harModOut, hargreaves(TMIN, TMAX, Ra = TMAX))
})

# out <- hargreaves(TMIN, TMAX, lat=37.6475, Pre=TMAX)
# saveRDS(out, file='./tests/testthat/data/hargreaves_out_pre.rds')
test_that("example with Pre", {
  harModOut <- readRDS("data/hargreaves_out_pre.rds")
  expect_equal(harModOut, hargreaves(TMIN, TMAX, lat = 37.6475, Pre = TMAX))
})

# out <- hargreaves(ts(TMIN, c(1980,1), fr=12), TMAX, lat=37.6475)
# saveRDS(out, file='./tests/testthat/data/hargreaves_out_ts.rds')
test_that("example with lat", {
  expect_equal(
    readRDS("data/hargreaves_out_ts.rds"),
    hargreaves(ts(TMIN, c(1980, 1), fr = 12), TMAX, lat = 37.6475)
  )
})
