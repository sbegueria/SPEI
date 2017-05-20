context("spei")

data(wichita)
wichita$PET <- thornthwaite(wichita$TMED,37.6475)

# Changes in L-Moments calculation yield differences less than 1e-7

test_that("example w/ 1 month", {
  spei1MoOut = readRDS("data/spei_1mo_Out.rds")
  expect_equal(spei1MoOut, 
               spei(wichita$PRCP-wichita$PET,1),
               tol = 1e-7 
  )
})


test_that("example w/ 12 months", {
  spei12MoOut = readRDS("data/spei_12mo_Out.rds")
  expect_equal(spei12MoOut, 
               spei(wichita$PRCP-wichita$PET,12),
               tol = 1e-7
  )
})


test_that("example w/ 1 month (User Params)", {
  spei1MoOut = readRDS("data/spei_1mo_Out.rds")
  expect_equal(spei1MoOut[-1], 
               spei(wichita$PRCP-wichita$PET,1, params=spei1MoOut$coefficients)[-1],
               tol = 1e-7 
  )
})
  

test_that("plot works without errors/warnings", {
  pdf(NULL) # Stops Rplots.pdf from being created
  spei1 = spei(wichita$PRCP-wichita$PET,1)
  expect_error(plot(spei1), NA)
})
  