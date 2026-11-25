context("Time information for non-ts input (start, frequency)")

data(cruts4)
data(wichita)

# The equivalent time series matrix (one column per cell) of a 3-d array
as_tsmat <- function(x, start, frequency = 12) {
  ts(matrix(x, nrow = dim(x)[1]), start = start, frequency = frequency)
}

# spei() and spi() with 3-d arrays

test_that("arrays with start give the same values as the ts matrix", {
  m <- as_tsmat(cruts4, c(1900, 1))
  for (fun in c(spei, spi)) {
    x <- if (identical(fun, spi)) abs(cruts4) else cruts4
    mm <- if (identical(fun, spi)) abs(m) else m
    a <- fun(x, 12, start = c(1900, 1), verbose = FALSE)
    s <- fun(mm, 12, verbose = FALSE)
    expect_equal(as.vector(a$fitted), as.vector(s$fitted), tolerance = 1e-12)
    expect_equal(dim(a$fitted), dim(cruts4))
    a <- fun(x, 12,
      start = c(1900, 1), ref.start = c(1961, 1),
      ref.end = c(1990, 12), verbose = FALSE
    )
    s <- fun(mm, 12,
      ref.start = c(1961, 1), ref.end = c(1990, 12),
      verbose = FALSE
    )
    expect_equal(as.vector(a$fitted), as.vector(s$fitted), tolerance = 1e-12)
    expect_equal(unname(a$coefficients), unname(s$coefficients),
      tolerance = 1e-12
    )
  }
})

test_that("arrays starting in a month other than January", {
  x <- cruts4[4:1440, , ]
  a <- spei(x, 6,
    start = c(1900, 4), ref.start = c(1950, 1), ref.end = c(1980, 12),
    verbose = FALSE
  )
  s <- spei(as_tsmat(x, c(1900, 4)), 6,
    ref.start = c(1950, 1), ref.end = c(1980, 12), verbose = FALSE
  )
  expect_equal(as.vector(a$fitted), as.vector(s$fitted), tolerance = 1e-12)
  expect_equal(dimnames(a$fitted)[[1]][1:3], c("1900-04", "1900-05", "1900-06"))
})

test_that("weekly arrays (frequency 48 and 52)", {
  for (f in c(48, 52)) {
    a <- spei(cruts4, 4,
      start = c(1961, 3), frequency = f,
      ref.start = c(1970, 1), ref.end = c(1985, f), verbose = FALSE
    )
    s <- spei(as_tsmat(cruts4, c(1961, 3), f), 4,
      ref.start = c(1970, 1), ref.end = c(1985, f), verbose = FALSE
    )
    expect_equal(as.vector(a$fitted), as.vector(s$fitted), tolerance = 1e-12)
    expect_equal(dim(a$coefficients), c(3, 6, f))
    lab <- dimnames(a$fitted)[[1]]
    expect_equal(lab[1:2], c("1961-P03", "1961-P04"))
    expect_equal(lab[f - 1], paste0("1962-P01"))
  }
})

test_that("params round trip with a non-monthly frequency", {
  a <- spi(abs(cruts4), 3, start = c(1961, 1), frequency = 48, verbose = FALSE)
  p <- spi(abs(cruts4), 3,
    start = c(1961, 1), frequency = 48,
    params = a$coefficients, verbose = FALSE
  )
  expect_equal(p$fitted, a$fitted, tolerance = 1e-12)
  expect_error(
    spi(abs(cruts4), 3, params = a$coefficients, verbose = FALSE),
    "Parameters array should have dimensions"
  )
})

test_that("time labels of the output keep the other dimnames", {
  x <- cruts4
  dimnames(x) <- list(NULL, c("w", "e"), c("s", "c", "n"))
  a <- spei(x, 1, start = 1900, verbose = FALSE)
  expect_equal(dimnames(a$fitted)[2:3], dimnames(x)[2:3])
  expect_equal(dimnames(a$fitted)[[1]][c(1, 1440)], c("1900-01", "2019-12"))
  # the labels survive subsetting
  expect_equal(a$fitted["2019-12", "e", "n"], a$fitted[1440, 2, 3])
  a <- spei(x, 1, start = 1900, frequency = 1, verbose = FALSE)
  expect_equal(dimnames(a$fitted)[[1]][1:2], c("1900", "1901"))
})

test_that("vector and matrix input with start become time series", {
  x <- as.numeric(cruts4[, 1, 1])
  a <- spei(x, 12, start = c(1900, 1), ref.end = c(1950, 12), verbose = FALSE)
  s <- spei(ts(x, start = c(1900, 1), frequency = 12), 12,
    ref.end = c(1950, 12), verbose = FALSE
  )
  expect_equal(a$fitted, s$fitted, tolerance = 1e-12)
  m <- matrix(cruts4, nrow = 1440)
  a <- spi(abs(m), 3, start = c(1900, 5), frequency = 52, verbose = FALSE)
  s <- spi(ts(abs(m), start = c(1900, 5), frequency = 52), 3, verbose = FALSE)
  expect_equal(a$fitted, s$fitted, tolerance = 1e-12)
})

test_that("time information taken from monthly time dimnames", {
  x <- cruts4
  dimnames(x)[[1]] <- format(seq(as.Date("1900-01-16"),
    by = "month",
    length.out = 1440
  ))
  expect_message(
    a <- spei(x, 12, ref.start = c(1961, 1), ref.end = c(1990, 12)),
    "Time information taken from the time dimnames"
  )
  s <- spei(cruts4, 12,
    start = c(1900, 1), ref.start = c(1961, 1),
    ref.end = c(1990, 12), verbose = FALSE
  )
  expect_equal(as.vector(a$fitted), as.vector(s$fitted), tolerance = 1e-12)
  # the input dimnames are kept
  expect_equal(dimnames(a$fitted)[[1]], dimnames(x)[[1]])
  # labels that are not a regular monthly series are not used
  dimnames(x)[[1]][10] <- "1900-12-16"
  expect_message(
    spei(x, 12),
    "No time information provided"
  )
})

test_that("calls without start/frequency are unchanged", {
  expect_equal(
    spei(cruts4, 12, verbose = FALSE)$fitted,
    readRDS("data/spei_12_array.rds"),
    tolerance = 1e-12
  )
  expect_equal(
    as.vector(spi(abs(cruts4), 1, verbose = FALSE)$fitted),
    as.vector(spi(abs(cruts4), 1, start = c(1, 1), verbose = FALSE)$fitted),
    tolerance = 1e-12
  )
  expect_null(dimnames(spei(cruts4, 1, verbose = FALSE)$fitted))
})

test_that("plot works for arrays with time labels", {
  a <- spei(cruts4, 12,
    start = c(1900, 1), ref.start = c(1961, 1),
    ref.end = c(1990, 12), verbose = FALSE
  )
  expect_s3_class(plot(a), "ggplot")
  g <- plot(a)
  expect_equal(min(g$data$time), 1900 + 11 / 12, tolerance = 1e-9)
})

test_that("errors for bad or conflicting start / frequency", {
  m <- as_tsmat(cruts4, c(1900, 1))
  expect_error(
    spei(m, 1, start = c(1900, 1)),
    "can not be used when `data` is a time series"
  )
  expect_error(
    spi(abs(m), 1, frequency = 12),
    "can not be used when `data` is a time series"
  )
  expect_error(spei(cruts4, 1, start = "1900"), "Argument `start` must be")
  expect_error(spei(cruts4, 1, start = c(1900, 1, 1)), "Argument `start` must be")
  expect_error(spei(cruts4, 1, start = 1900.5), "Argument `start` must be")
  expect_error(
    spei(cruts4, 1, start = c(1900, 13)),
    "The period in `start` \\(13\\) must be between 1 and"
  )
  expect_error(
    spei(cruts4, 1, start = c(1900, 50), frequency = 48),
    "must be between 1 and the number of periods per year \\(48\\)"
  )
  expect_error(spei(cruts4, 1, frequency = 0), "Argument `frequency` must be")
  expect_error(spei(cruts4, 1, frequency = 12.5), "Argument `frequency` must be")
  expect_error(spei(cruts4, 1, frequency = c(12, 48)), "Argument `frequency` must be")
})

# Reference period outside the time span of the data

test_that("a reference period outside the data gives a warning, same result", {
  m <- as_tsmat(cruts4, c(1900, 1))
  inside <- spei(m, 12,
    ref.start = c(1900, 1), ref.end = c(2019, 12),
    verbose = FALSE
  )
  whole <- spei(m, 12, verbose = FALSE)
  expect_silent(spei(m, 12,
    ref.start = c(1900, 1), ref.end = c(2019, 12),
    verbose = FALSE
  ))
  expect_equal(inside$fitted, whole$fitted, tolerance = 1e-12)
  expect_warning(
    s <- spei(m, 12, ref.start = c(1850, 1), verbose = FALSE),
    "`ref.start` falls outside the time span of `data`"
  )
  expect_equal(s$fitted, whole$fitted, tolerance = 1e-12)
  expect_warning(
    s <- spi(abs(m), 12, ref.end = c(2020, 1), verbose = FALSE),
    "`ref.end` falls outside the time span"
  )
  expect_equal(s$fitted, spi(abs(m), 12, verbose = FALSE)$fitted,
    tolerance = 1e-12
  )
  # both
  expect_warning(
    spei(m, 1, ref.start = c(1800, 1), ref.end = c(2100, 1), verbose = FALSE),
    "`ref.start` and `ref.end` fall outside"
  )
  # an array without time information: years counted from year 1
  expect_warning(
    spei(cruts4, 1, ref.start = c(62, 1), ref.end = c(1990, 12), verbose = FALSE),
    "`ref.end` falls outside the time span of `data`"
  )
  # an array with time information inferred from its dimnames, and the
  # reference period given in years counted from year 1 (the old way)
  x <- cruts4
  dimnames(x)[[1]] <- time_labels(c(1900, 1), 12, 1440)
  expect_warning(
    s <- spei(x, 12, ref.start = c(62, 1), ref.end = c(2019, 12), verbose = FALSE),
    "`ref.start` falls outside the time span"
  )
  expect_equal(as.vector(s$fitted), as.vector(whole$fitted), tolerance = 1e-12)
  # weekly data
  expect_warning(
    spei(cruts4, 4, start = c(1961, 1), frequency = 48, ref.end = c(1991, 1),
      verbose = FALSE),
    "`ref.end` falls outside"
  )
  expect_silent(
    spei(cruts4, 4, start = c(1961, 1), frequency = 48, ref.end = c(1990, 48),
      verbose = FALSE)
  )
})

# Monthly PET functions with 3-d arrays and start

n <- 120
i <- 6:(n + 5)
site_array <- function(x) array(rep(x[i], 6), dim = c(n, 2, 3))
lat <- matrix(c(30, 40, 50, 35, 45, 55), 2, 3)
tsm <- function(x) as_tsmat(x, c(1980, 6))

test_that("PET arrays with start give the same values as ts matrices", {
  tmed <- site_array(wichita$TMED)
  tmin <- site_array(wichita$TMIN)
  tmax <- site_array(wichita$TMAX)
  u2 <- site_array(wichita$AWND)
  a <- thornthwaite(tmed, lat, start = c(1980, 6), verbose = FALSE)
  b <- thornthwaite(tsm(tmed), as.vector(lat), verbose = FALSE)
  expect_equal(as.vector(a), as.vector(b), tolerance = 1e-12)
  expect_equal(dimnames(a)[[1]][1:2], c("1980-06", "1980-07"))
  # without start the array is taken to start in January
  a0 <- thornthwaite(tmed, lat, verbose = FALSE)
  expect_false(isTRUE(all.equal(as.vector(a0), as.vector(b))))
  expect_null(dimnames(a0))

  a <- hargreaves(tmin, tmax, lat = lat, start = c(1980, 6), verbose = FALSE)
  b <- hargreaves(tsm(tmin), tsm(tmax), lat = as.vector(lat), verbose = FALSE)
  expect_equal(as.vector(a), as.vector(b), tolerance = 1e-12)

  a <- penman(tmin, tmax,
    U2 = u2, lat = lat, z = matrix(400, 2, 3),
    start = c(1980, 6), na.rm = TRUE, verbose = FALSE
  )
  b <- penman(tsm(tmin), tsm(tmax),
    U2 = tsm(u2), lat = as.vector(lat),
    z = rep(400, 6), na.rm = TRUE, verbose = FALSE
  )
  expect_equal(as.vector(a), as.vector(b), tolerance = 1e-12)
  expect_equal(dimnames(a)[[1]][n], "1990-05")
})

test_that("PET vectors with start become monthly time series", {
  v <- thornthwaite(wichita$TMED[i], 37.6, start = c(1980, 6), verbose = FALSE)
  w <- thornthwaite(ts(wichita$TMED[i], start = c(1980, 6), frequency = 12),
    37.6,
    verbose = FALSE
  )
  expect_equal(v, w, tolerance = 1e-12)
})

test_that("PET errors for conflicting start", {
  tmin <- site_array(wichita$TMIN)
  tmax <- site_array(wichita$TMAX)
  expect_error(
    thornthwaite(tsm(site_array(wichita$TMED)), as.vector(lat),
      start = c(1980, 1)
    ),
    "can not be used when the input data are a time series"
  )
  expect_error(
    hargreaves(tmin[1:3, 1, 1], tmax[1:3, 1, 1],
      lat = 40,
      dates = as.Date("2000-01-01") + 0:2, start = 2000
    ),
    "can not be used with daily data"
  )
  expect_error(
    penman(tmin[1:3, 1, 1], tmax[1:3, 1, 1],
      lat = 40, z = 100,
      dates = as.Date("2000-01-01") + 0:2, start = 2000
    ),
    "can not be used with daily data"
  )
  expect_error(
    hargreaves(tmin, tmax, lat = lat, start = c(1980, 0)),
    "The period in `start` \\(0\\) must be between 1 and"
  )
})
