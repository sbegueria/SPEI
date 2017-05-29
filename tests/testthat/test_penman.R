context("penman")

data(wichita)

test_that("example w/ sun", {
  penSunOut = readRDS("data/pen_sun_Out.rds")
  expect_equal(penSunOut, 
               penman(wichita$TMIN,wichita$TMAX,wichita$AWND,
                      tsun=wichita$TSUN,lat=37.6475,z=402.6,na.rm=TRUE)
  )
})

test_that("example w/o sun", {
  penCCOut = readRDS("data/pen_cc_Out.rds")
  expect_equal(penCCOut, 
               penman(wichita$TMIN,wichita$TMAX,wichita$AWND,
                      CC=wichita$ACSH,lat=37.6475,z=402.6,na.rm=TRUE)
  )
})

test_that("First NAs and na.rm=FALSE error", {
  expect_error(penman(c(NA,wichita$TMIN),c(NA,wichita$TMAX),c(NA,wichita$AWND),
                      CC=c(NA,wichita$ACSH),lat=37.6475,z=402.6,na.rm=FALSE),
               'Error: Data must not contain NAs'
  )
})

test_that("Second NAs and na.rm=FALSE error", {
  expect_error(penman(wichita$TMIN[1:2],wichita$TMAX[1:2],wichita$TSUN[1:2],
                      CC=c(NA,wichita$ACSH),lat=37.6475,z=402.6,na.rm=FALSE),
               'Error: Non-temperature data must not contain NAs'
  )
})

test_that("No lat and no Ra", {
  expect_error(penman(wichita$TMIN[1:2],wichita$TMAX[1:2],wichita$TSUN[1:2],
                      CC=wichita$ACSH[1:2],lat=NA,z=402.6,na.rm=FALSE),
               'Error: One of Ra or lat must be provided'
  )
})

test_that("First unequal length error", {
  expect_error(penman(wichita$TMIN,wichita$TMAX,wichita$AWND,
                      tsun=wichita$TSUN[1:3],lat=37.6475,z=402.6,na.rm=TRUE),
               'Error: One of Rs, tsun or CC must be provided'
  )
})

test_that("Second unequal length error", {
  expect_error(penman(wichita$TMIN[1:3],wichita$TMAX,wichita$AWND,
                      tsun=wichita$TSUN[1:3],lat=37.6475,z=402.6,na.rm=TRUE),
               'Error: Data must be of the same length'
  )
})