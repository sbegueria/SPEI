context("penman")

data(wichita)

test_that("example w/ sun", {
  penSunOut = readRDS("data/pen_sun_Out.rds")
  expect_equal(penSunOut, 
               penman(wichita$TMIN,wichita$TMAX,wichita$AWND,
                      tsun=wichita$TSUN,lat=37.6475,z=402.6,na.rm=TRUE)
  )
})

test_that("example w/ sun", {
  penCCOut = readRDS("data/pen_cc_Out.rds")
  expect_equal(penCCOut, 
               penman(wichita$TMIN,wichita$TMAX,wichita$AWND,
                      CC=wichita$ACSH,lat=37.6475,z=402.6,na.rm=TRUE)
  )
})