context("spi")

data(wichita)
wichita$PET <- thornthwaite(wichita$TMED,37.6475)

# Changes in L-Moments calculation yield differences less than 1e-7

test_that("example w/ 1 month", {
  spi1MoOut = readRDS("data/spi_1mo_Out.rds")
  expect_equal(spi1MoOut, 
               spi(wichita$PRCP,1),
               tol = 1e-7
  )
})

test_that("example w/ 12 months", {
  spi12MoOut = readRDS("data/spi_12mo_Out.rds")
  expect_equal(spi12MoOut, 
               spi(wichita$PRCP,12),
               tol = 1e-7
  )
})

test_that("example w/ 1 month (SPEI vs SPI)", {
  spi1MoOut = readRDS("data/spi_1mo_Out.rds")
  expect_equal(spi1MoOut[-1], 
               spei(wichita$PRCP,1,distribution="Gamma")[-1],
               tol = 1e-7
  )
})
