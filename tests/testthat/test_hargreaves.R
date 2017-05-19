context("hargreaves")

data(wichita)

test_that("example", {
  harOut = readRDS("data/har_Out.rds")
  expect_equal(harOut, hargreaves(wichita$TMIN,wichita$TMAX,lat=37.6475)
)
})