context("thornwaite")

data(wichita)

test_that("example", {
  thoOut = readRDS("data/tho_Out.rds")
  expect_equal(thoOut, thornthwaite(wichita$TMED,37.6475))
})