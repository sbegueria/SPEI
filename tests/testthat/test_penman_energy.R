context("penman: energy-only potential evapotranspiration (Milly and Dunne, 2016)")

pen <- function(...) penman(..., verbose = FALSE)

# FAO-56 Example 18 (Brussels, 6 July): Tmax = 21.5, Tmin = 12.3 ºC,
# ea = 1.409 kPa, n = 9.25 h, lat = 50º48'N, z = 100 m; published
# Rn = 13.28 MJ m-2 d-1 and, with daily data, G = 0
lat18 <- 50 + 48 / 60

test_that("FAO-56 Example 18: PET = 0.8 Rn / 2.45 (daily, G = 0)", {
  e <- pen(12.3, 21.5,
    tsun = 9.25, lat = lat18, z = 100, ed = 1.409,
    method = "energy", dates = "2005-07-06"
  )
  expect_s3_class(e, "zoo")
  expect_equal(as.numeric(e), 0.8 * 13.28 / 2.45, tolerance = 0.02 / 4.34)
  # Exact value, with Rn reproduced as in penman()
  j <- 187
  delta <- 0.409 * sin(0.0172 * j - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * j)
  latr <- lat18 / 57.2957795
  ws <- acos(-tan(latr) * tan(delta))
  ra <- 37.6 * dr * (ws * sin(latr) * sin(delta) +
    cos(latr) * cos(delta) * sin(ws))
  rs <- (0.25 + 0.5 * 9.25 / (7.64 * ws)) * ra
  rso <- (0.75 + 2e-5 * 100) * ra
  rn <- 0.77 * rs - (1.35 * min(rs / rso, 1) - 0.35) *
    (0.34 - 0.14 * sqrt(1.409)) * 4.9e-9 *
    ((273.15 + 21.5)^4 + (273.15 + 12.3)^4) / 2
  expect_equal(rn, 13.28, tolerance = 0.03 / 13.28)
  expect_equal(as.numeric(e), 0.8 * rn / 2.45, tolerance = 1e-12)
  # Same with Ra and Rs given
  expect_equal(
    as.numeric(pen(12.3, 21.5,
      Ra = ra, Rs = rs, z = 100, ed = 1.409,
      method = "energy", dates = "2005-07-06"
    )),
    0.8 * rn / 2.45,
    tolerance = 1e-12
  )
})

test_that("monthly mode uses G (FAO-56 eq. 43) and the month length", {
  tmin <- c(-2, 0, 3, 6, 10, 14, 17, 16, 12, 8, 3, 0)
  tmax <- c(8, 10, 14, 17, 22, 27, 31, 30, 25, 19, 12, 9)
  ra <- c(14, 19, 26, 34, 39, 41, 40, 36, 29, 21, 16, 13)
  rs <- 0.5 * ra
  ed <- 0.611 * exp(17.27 * tmin / (tmin + 237.3))
  e <- pen(tmin, tmax, Ra = ra, Rs = rs, ed = ed, z = 300, method = "energy")
  tmean <- (tmin + tmax) / 2
  rso <- (0.75 + 2e-5 * 300) * ra
  rn <- 0.77 * rs - (1.35 * pmin(rs / rso, 1) - 0.35) *
    (0.34 - 0.14 * sqrt(ed)) * 4.9e-9 *
    ((273.15 + tmax)^4 + (273.15 + tmin)^4) / 2
  g <- c(
    0.14 * (tmean[2] - tmean[1]),
    0.07 * (tmean[3:12] - tmean[1:10]),
    0.14 * (tmean[12] - tmean[11])
  )
  mlen <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  expect_equal(e, pmax(0.8 * (rn - g) / 2.45, 0) * mlen, tolerance = 1e-12)
})

test_that("negative values are set to zero", {
  # Cold, clear-sky days with little solar radiation: Rn < 0
  e <- pen(c(-30, -30), c(-20, -20),
    Ra = c(1, 1), Rs = c(0.75, 0.75), ed = c(0.05, 0.05), z = 0,
    method = "energy", dates = c("2001-01-01", "2001-01-02")
  )
  expect_equal(as.numeric(e), c(0, 0))
})

test_that("matrix and array input give the same values as vectors", {
  data(wichita, envir = environment())
  w <- wichita[1:60, ]
  v <- pen(w$TMIN, w$TMAX,
    tsun = w$TSUN, lat = 37.6475, z = 402.6,
    method = "energy"
  )
  m <- pen(cbind(w$TMIN, w$TMIN), cbind(w$TMAX, w$TMAX),
    tsun = cbind(w$TSUN, w$TSUN), lat = c(37.6475, 37.6475),
    z = c(402.6, 402.6), method = "energy"
  )
  expect_equal(unname(m[, 1]), v, tolerance = 1e-12)
  expect_equal(unname(m[, 2]), v, tolerance = 1e-12)
  a <- pen(array(w$TMIN, c(60, 2, 2)), array(w$TMAX, c(60, 2, 2)),
    tsun = array(w$TSUN, c(60, 2, 2)), lat = matrix(37.6475, 2, 2),
    z = matrix(402.6, 2, 2), method = "energy"
  )
  expect_equal(dim(a), c(60L, 2L, 2L))
  expect_equal(as.vector(a[, 2, 2]), v, tolerance = 1e-12)
})

test_that("U2 is not needed; U2, CO2 and crop = 'tall' are ignored", {
  args <- list(12.3, 21.5,
    tsun = 9.25, lat = lat18, z = 100, ed = 1.409,
    method = "energy", dates = "2005-07-06"
  )
  ref <- do.call(pen, args)
  expect_silent(do.call(pen, args))
  expect_message(
    do.call(penman, args),
    "not used by the energy-only method"
  )
  expect_warning(
    r <- do.call(pen, c(args, list(U2 = 3))),
    "`U2` is ignored"
  )
  expect_equal(r, ref, tolerance = 1e-12)
  expect_warning(
    r <- do.call(pen, c(args, list(CO2 = 500))),
    "`CO2` is ignored"
  )
  expect_equal(r, ref, tolerance = 1e-12)
  expect_warning(
    r <- do.call(pen, c(args, list(crop = "tall"))),
    "`crop` is ignored"
  )
  expect_equal(r, ref, tolerance = 1e-12)
  # NA in U2 is irrelevant
  expect_warning(do.call(pen, c(args, list(U2 = NA))), "`U2` is ignored")
})

test_that("RH and temperature-only routes work", {
  e1 <- pen(12.3, 21.5,
    RH = 63, lat = lat18, z = 100, tsun = 9.25,
    method = "energy", dates = "2005-07-06"
  )
  # FAO-56 eq. 19: ed = es RH / 100
  es <- (0.611 * exp(17.27 * 21.5 / (21.5 + 237.3)) +
    0.611 * exp(17.27 * 12.3 / (12.3 + 237.3))) / 2
  e2 <- pen(12.3, 21.5,
    ed = es * 0.63, lat = lat18, z = 100, tsun = 9.25,
    method = "energy", dates = "2005-07-06"
  )
  expect_equal(e1, e2, tolerance = 1e-12)
  e3 <- pen(12.3, 21.5,
    lat = lat18, z = 100, method = "energy",
    dates = "2005-07-06"
  )
  expect_true(is.finite(as.numeric(e3)) && as.numeric(e3) > 0)
})

test_that("invalid method is rejected", {
  expect_error(
    pen(12.3, 21.5, lat = 40, z = 100, method = "Energy"),
    "must be one of"
  )
})
