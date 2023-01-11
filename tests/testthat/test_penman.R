context("penman")

data(wichita)
attach(wichita)

# Test input data checks and error messages

# penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE)

test_that("No Tmin provided", {
  expect_error(
    penman(
      Tmax = TMAX, U2 = AWND, tsun = TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    'argument "Tmin" is missing, with no default'
  )
})

test_that("No Tmax provided", {
  expect_error(
    penman(TMIN,
      U2 = AWND, tsun = TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    'argument "Tmax" is missing, with no default'
  )
})

test_that("No Ra or lat provided", {
  expect_error(
    penman(TMIN, TMAX, AWND, tsun = TSUN, z = 402.6, na.rm = TRUE),
    "One of `Ra` or `lat` must be provided."
  )
})

test_that("No Rs nor tsun provided", {
  expect_error(
    penman(TMIN, TMAX, AWND, lat = 37.6475, z = 402.6, na.rm = TRUE),
    "One of `Rs`, the pair `tsun` and `lat`, or `CC` must be provided."
  )
})

test_that("No Rs nor CC provided", {
  expect_error(
    penman(TMIN, TMAX, AWND, lat = 37.6475, z = 402.6, na.rm = TRUE),
    "One of `Rs`, the pair `tsun` and `lat`, or `CC` must be provided."
  )
})

test_that("No Tmin provided", {
  expect_error(
    penman(
      Tmax = TMAX, U2 = AWND, tsun = TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    'argument \"Tmin\" is missing, with no default'
  )
})

test_that("No P nor z provided", {
  expect_error(
    penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, na.rm = TRUE),
    "One of `P`, the pair `P0` and `z`, or `z` must be provided."
  )
})

test_that("P0 without z provided", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, P0 = TSUN,
      na.rm = TRUE
    ),
    "One of `P`, the pair `P0` and `z`, or `z` must be provided."
  )
})

test_that("Unconformed crop provided", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6,
      crop = "corn", na.rm = TRUE
    ),
    "Argument `crop` must be one of `short` or `tall`."
  )
})

test_that("NAs in Tmin", {
  expect_error(
    penman(c(NA, TMIN), c(1, TMAX), c(1, AWND),
      tsun = c(1, TSUN),
      lat = 37.6475, z = 402.6, na.rm = FALSE
    ),
    "Data must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in Tmax", {
  expect_error(
    penman(c(1, TMIN), c(NA, TMAX), c(1, TMIN),
      tsun = c(1, TMIN),
      lat = 37.6475, z = 402.6, na.rm = FALSE
    ),
    "`Tmin`, `Tmax` and `U2` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in U2", {
  expect_error(
    penman(c(1, TMIN), c(1, TMAX), c(NA, TMIN),
      tsun = c(1, TMIN),
      lat = 37.6475, z = 402.6, na.rm = FALSE
    ),
    "`Tmin`, `Tmax` and `U2` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in Tsun", {
  expect_error(
    penman(c(1, TMIN), c(1, TMAX), c(1, TMIN),
      tsun = c(NA, TMIN),
      lat = 37.6475, z = 402.6, na.rm = FALSE
    ),
    "Data must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in z", {
  expect_error(
    penman(c(1, TMIN), c(1, TMAX), c(1, TMIN),
      tsun = c(1, TMIN),
      lat = 37.6475, z = NA, na.rm = FALSE
    ),
    "Data must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("NAs in lat", {
  expect_error(
    penman(c(1, TMIN), c(1, TMAX), c(1, TMIN),
      tsun = c(1, TMIN),
      lat = NA, z = 402.6, na.rm = FALSE
    ),
    "Data must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

nt <- nrow(wichita)
tmin <- array(TMIN, dim = c(nt, 2, 2, 2))
# tmax <- array(TMAX, dim=c(nt, 2, 2, 2))
# awnd <- array(AWND, dim=c(nt, 2, 2, 2))
# tsun <- array(TSUN, dim=c(nt, 2, 2, 2))
test_that("input more than three dimensions", {
  expect_error(
    penman(tmin, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    "Input data can not have more than 3 dimensions."
  )
})

test_that("Tmax incorrect length", {
  expect_error(
    penman(TMIN, TMAX[-1], AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    "`Tmin` and `Tmax` cannot have different lengths."
  )
})

test_that("U2 incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND[-1],
      tsun = TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    "`U2` has incorrect length."
  )
})

test_that("Ra incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      Ra = TMIN[-1], lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    "`Ra` has incorrect length."
  )
})

test_that("Rs incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      Rs = TMIN[-1], lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    "`Rs` has incorrect length."
  )
})

test_that("tsun incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN[-1], lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    "`tsun` has incorrect length."
  )
})

# test_that('CC incorrect length', {
#   expect_error(penman(TMIN, TMAX, AWND, tsun=TSUN, CC=TMIN[-1], lat=37.6475,
#                 z=402.6, na.rm=TRUE),
#                '`tsun` has incorrect length.')
# })

test_that("ed incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, ed = TMIN[-1], lat = 37.6475,
      z = 402.6, na.rm = TRUE
    ),
    "`ed` has incorrect length."
  )
})

test_that("Tdew incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, Tdew = TMIN[-1], lat = 37.6475,
      z = 402.6, na.rm = TRUE
    ),
    "`Tdew` has incorrect length."
  )
})

test_that("RH incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, RH = TMIN[-1], lat = 37.6475,
      z = 402.6, na.rm = TRUE
    ),
    "`RH` has incorrect length."
  )
})


test_that("P incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, P = TMIN[-1],
      na.rm = TRUE
    ),
    "`P` has incorrect length."
  )
})

test_that("P0 incorrect length", {
  expect_error(
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, P0 = TMIN[-1],
      z = 402.6, na.rm = TRUE
    ),
    "`P0` has incorrect length."
  )
})

# test_that('z incorrect length', {
#   expect_error(penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, P0=TMIN,
#   z=c(402.6,1), na.rm=TRUE),
#                '`z` has incorrect length.')
# })


# Test function results

# out <- penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_tsun.rds')
test_that("example with tsun", {
  expect_equal(
    readRDS("data/penman_out_tsun.rds"),
    penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE)
  )
})

# out <- penman(ts(TMIN, c(1980,1), fr=12), TMAX, AWND, tsun=TSUN, lat=37.6475,
#   z=402.6, na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_ts.rds')
test_that("example with ts input", {
  expect_equal(
    readRDS("data/penman_out_ts.rds"),
    penman(ts(TMIN, c(1980, 1), fr = 12), TMAX, AWND,
      tsun = TSUN, lat = 37.6475,
      z = 402.6, na.rm = TRUE
    )
  )
})

# out <- penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_tsun.rds')
test_that("example with tsun", {
  expect_equal(
    readRDS("data/penman_out_tsun.rds"),
    penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE)
  )
})

# out <- penman(TMIN, TMAX, AWND, CC=ACSH, lat=37.6475, z=402.6, na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_cc.rds')
test_that("example with cc", {
  expect_equal(
    readRDS("data/penman_out_cc.rds"),
    penman(TMIN, TMAX, AWND, CC = ACSH, lat = 37.6475, z = 402.6, na.rm = TRUE)
  )
})

# out <- penman(TMIN, TMAX, tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_u2.rds')
test_that("example with constant wind", {
  expect_equal(
    readRDS("data/penman_out_u2.rds"),
    penman(TMIN, TMAX, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE)
  )
})

# out <- penman(ts(TMIN, start=c(1980, 1), frequency=12), TMAX, AWND,
#   tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_tsun_ts.rds')
test_that("example with tsvector", {
  expect_equal(
    readRDS("data/penman_out_tsun_ts.rds"),
    penman(ts(TMIN, start = c(1980, 1), frequency = 12), TMAX, AWND,
      tsun = TSUN,
      lat = 37.6475, z = 402.6, na.rm = TRUE
    )
  )
})

data(cabinda)
# out <- penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2,	Rs=cabinda$Rs,
#   tsun=cabinda$tsun, RH=cabinda$RH, lat=-5.33, z=20)
# saveRDS(out, file='./tests/testthat/data/penman_out_cabinda.rds')
test_that("example with cabinda data", {
  expect_equal(
    readRDS("data/penman_out_cabinda.rds"),
    penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2,
      Rs = cabinda$Rs,
      tsun = cabinda$tsun, RH = cabinda$RH, lat = -5.33, z = 20
    )
  )
})

# out <- penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE,
#   method='ASCE')
# saveRDS(out, file='./tests/testthat/data/penman_out_asce.rds')
test_that("example with ASCE`s method", {
  expect_equal(
    readRDS("data/penman_out_asce.rds"),
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE,
      method = "ASCE"
    )
  )
})

# out <- penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, na.rm=TRUE,
#   method='FAO')
# saveRDS(out, file='./tests/testthat/data/penman_out_fao.rds')
test_that("example with FAO`s method", {
  expect_equal(
    readRDS("data/penman_out_fao.rds"),
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE,
      method = "FAO"
    )
  )
})

# out <- penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, CO2=450,
#   na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_co2.rds')
test_that("example with different CO2", {
  expect_equal(
    readRDS("data/penman_out_co2.rds"),
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6, CO2 = 450,
      na.rm = TRUE
    )
  )
})

co2 <- seq(300, 450, length.out = length(TMIN))
# out <- penman(TMIN, TMAX, AWND, tsun=TSUN, lat=37.6475, z=402.6, CO2=co2,
#   na.rm=TRUE)
# saveRDS(out, file='./tests/testthat/data/penman_out_co2ts.rds')
test_that("example with increasing CO2", {
  expect_equal(
    readRDS("data/penman_out_co2ts.rds"),
    penman(TMIN, TMAX, AWND,
      tsun = TSUN, lat = 37.6475, z = 402.6, CO2 = co2,
      na.rm = TRUE
    )
  )
})
