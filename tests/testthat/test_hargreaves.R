context("hargreaves")

data(wichita)

test_that("example", {
  harOut = readRDS("data/har_Out.rds")
  expect_equal(harOut, hargreaves(wichita$TMIN,wichita$TMAX,lat=37.6475)
)
})

test_that("example with Pre", {
  harModOut = readRDS("data/har_mod_Out.rds")
  expect_equal(harModOut, hargreaves(wichita$TMIN,wichita$TMAX,lat=37.6475, Pre=seq_along(wichita$TMIN))
  )
})

test_that("Ra length 1 and wrong lat length error", {
  expect_error(hargreaves(wichita$TMIN,wichita$TMAX,lat=c(37, 38), Ra = 1),
               'Error: lat should be specified for estimating external radiation if Ra is not provided, and should have the same number of elements than Tmin.'
  )
})

test_that("NAs with na.rm false error", {
  expect_error(hargreaves(c(NA,wichita$TMIN),c(NA,wichita$TMAX),lat=37.6475),
               'Error: Data must not contain NAs'
  )
})

test_that("Ra NAs with na.rm false error", {
  expect_error(hargreaves(wichita$TMIN,wichita$TMAX,lat=37.6475,Ra=rep(NA, nrow(wichita))),
               'Error: Data must not contain NAs'
  )
})

test_that("Different length TMIN and TMAX", {
  expect_error(hargreaves(wichita$TMIN[-1],wichita$TMAX,lat=37.6475),
               'Error: Tmin and Tmax must be of the same length'
  )
})

test_that("Different length TMIN and TMAX", {
  expect_error(hargreaves(wichita$TMIN,wichita$TMAX,lat=37.6475, Ra=c(1,2)),
               'Error: Ra must be of the same length than Tmin and Tmax'
  )
})

test_that("TS wrong frequency", {
  expect_error(hargreaves(ts(wichita$TMIN, frequency=4),ts(wichita$TMAX, frequency=4),lat=37.6475),
               'Error: Data should be a monthly time series*'
  )
})