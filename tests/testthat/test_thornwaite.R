context("thornwaite")

data(wichita)

test_that("example", {
  thoOut = readRDS("data/tho_Out.rds")
  expect_equal(thoOut, thornthwaite(wichita$TMED,37.6475))
})

test_that("NAs + na.rm=FALSE error", {
  expect_error(thornthwaite(c(NA, wichita$TMED),37.6475),
               'Data must not contain NAs')
})

test_that("Non-monthly time series", {
  expect_error(thornthwaite(ts(1:10, frequency=10),37.6475),
               'Data should be a monthly time series*')
})

test_that("Non-monthly time series", {
  expect_error(thornthwaite(cbind(wichita$TMED, wichita$TMED),37.6475),
               'Longitude of latitudes vector does not coincide with the number of columns in Tave')
})