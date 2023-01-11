context("thornthwaite")

data(wichita)
attach(wichita)

# Test input data checks and error messages

test_that("NAs in Tave + na.rm=FALSE error", {
  expect_error(
    thornthwaite(c(NA, TMED), 37.6475),
    "`Tave` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in lat + na.rm=FALSE error", {
  expect_error(
    thornthwaite(TMED, NA),
    "`lat` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

# test_that('Non-monthly time series', {
#  expect_error(thornthwaite(ts(1:10, frequency=10), 37.6475),
#               'Data should be a monthly time series*')
# })

nt <- nrow(wichita)
tmed <- array(TMED, dim = c(nt, 2, 2, 2))
test_that("input more than three dimensions", {
  expect_error(
    thornthwaite(tmed, 37.6475),
    "Input data can not have more than three dimensions."
  )
})

test_that("lat has incorrect length", {
  expect_error(
    thornthwaite(cbind(TMED, TMED), lat = 37.6475),
    "`lat` has incorrect length."
  )
})

# Test function results

# out <- thornthwaite(TMED, 37.6475)
# saveRDS(out, file='./tests/testthat/data/thornthwaite_out.rds')
test_that("example", {
  thoOut <- readRDS("data/thornthwaite_out.rds") # ./tests/testthat/
  expect_equal(thoOut, thornthwaite(TMED, 37.6475))
})

# out <- thornthwaite(ts(TMED, c(1980,1), fr=12), 37.6475)
# saveRDS(out, file='./tests/testthat/data/thornthwaite_out_ts.rds')
test_that("example", {
  thoOut <- readRDS("data/thornthwaite_out_ts.rds") # ./tests/testthat/
  expect_equal(thoOut, thornthwaite(ts(TMED, c(1980, 1), fr = 12), 37.6475))
})
