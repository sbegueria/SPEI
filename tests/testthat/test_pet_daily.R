context("Daily PET (penman and hargreaves with `dates`)")

# Monthly results must not change -----------------------------------------

test_that("monthly results are unchanged (dates = NULL)", {
  # Reference values computed with SPEI 1.9.0.9000 before daily support was
  # added (git commit 1f82e99), for the calls below.
  ref <- readRDS("data/pet_monthly_baseline.rds")
  e <- new.env()
  data("wichita", "cabinda", package = "SPEI", envir = e)
  w <- e$wichita
  cab <- e$cabinda
  tsmin <- ts(w$TMIN, start = c(1980, 1), frequency = 12)
  nt <- nrow(w)
  arr <- function(x) array(x, dim = c(nt, 2, 2))
  latarr <- array(c(40, 30, 40, 30), dim = c(2, 2))
  zarr <- array(c(400, 10, 1000, 50), dim = c(2, 2))
  pen <- function(...) penman(..., verbose = FALSE)
  har <- function(...) hargreaves(..., verbose = FALSE)
  new <- list(
    pen_tsun = pen(w$TMIN, w$TMAX, w$AWND,
      tsun = w$TSUN, lat = 37.6475,
      z = 402.6, na.rm = TRUE
    ),
    pen_cc = pen(w$TMIN, w$TMAX, w$AWND,
      CC = w$ACSH, lat = 37.6475,
      z = 402.6, na.rm = TRUE
    ),
    pen_const = pen(w$TMIN, w$TMAX,
      tsun = w$TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    ),
    pen_ts = pen(tsmin, w$TMAX, w$AWND,
      tsun = w$TSUN, lat = 37.6475,
      z = 402.6, na.rm = TRUE
    ),
    pen_ts_jun = pen(ts(w$TMIN, start = c(1980, 6), frequency = 12), w$TMAX,
      w$AWND,
      tsun = w$TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE
    ),
    pen_asce = pen(w$TMIN, w$TMAX, w$AWND,
      tsun = w$TSUN, lat = 37.6475,
      z = 402.6, na.rm = TRUE, method = "ASCE"
    ),
    pen_fao = pen(w$TMIN, w$TMAX, w$AWND,
      tsun = w$TSUN, lat = 37.6475,
      z = 402.6, na.rm = TRUE, method = "FAO"
    ),
    pen_tall_co2 = pen(w$TMIN, w$TMAX, w$AWND,
      tsun = w$TSUN, lat = 37.6475,
      z = 402.6, na.rm = TRUE, crop = "tall", CO2 = 450
    ),
    pen_cab_icid = pen(cab$Tmin, cab$Tmax, cab$U2,
      Rs = cab$Rs, tsun = cab$tsun,
      RH = cab$RH, lat = -5.33, z = 20
    ),
    pen_cab_fao_tsun = pen(cab$Tmin, cab$Tmax, cab$U2,
      tsun = cab$tsun,
      RH = cab$RH, lat = -5.33, z = 20, method = "FAO"
    ),
    pen_cab_asce_rs = pen(cab$Tmin, cab$Tmax, cab$U2,
      Rs = cab$Rs, RH = cab$RH,
      lat = -5.33, z = 20, P0 = rep(101, 12), method = "ASCE"
    ),
    pen_matrix = pen(cbind(w$TMIN, w$TMIN + 1.5), cbind(w$TMAX, w$TMAX + 1.5),
      cbind(w$AWND, w$AWND),
      tsun = cbind(w$TSUN, w$TSUN),
      lat = c(37.6475, 35), z = c(402.6, 100), na.rm = TRUE
    ),
    pen_tsmatrix = pen(
      ts(cbind(w$TMIN, w$TMIN + 1.5), start = c(1980, 3), frequency = 12),
      cbind(w$TMAX, w$TMAX + 1.5), cbind(w$AWND, w$AWND),
      tsun = cbind(w$TSUN, w$TSUN), lat = c(37.6475, 35), z = c(402.6, 100),
      na.rm = TRUE
    ),
    pen_array = pen(arr(w$TMIN), arr(w$TMAX), arr(w$AWND),
      tsun = arr(w$TSUN),
      lat = latarr, z = zarr, na.rm = TRUE
    ),
    pen_one = pen(10, 20, 2, tsun = 8, ed = 1, lat = 40, z = 100),
    pen_two = pen(c(10, 12), c(20, 24), c(2, 2),
      tsun = c(8, 8), ed = c(1, 1),
      lat = 40, z = 100
    ),
    har_lat = har(w$TMIN, w$TMAX, lat = 37.6475, na.rm = TRUE),
    har_pre = har(w$TMIN, w$TMAX, lat = 37.6475, Pre = w$PRCP, na.rm = TRUE),
    har_ra = har(cab$Tmin, cab$Tmax, Ra = cab$Rs * 1.6),
    har_ts = har(ts(w$TMIN, start = c(1980, 6), frequency = 12), w$TMAX,
      lat = 37.6475, na.rm = TRUE
    ),
    har_matrix = har(cbind(w$TMIN, w$TMIN + 1.5), cbind(w$TMAX, w$TMAX + 1.5),
      lat = c(37.6475, 35), na.rm = TRUE
    ),
    har_tsmatrix = har(
      ts(cbind(w$TMIN, w$TMIN + 1.5), start = c(1980, 3), frequency = 12),
      cbind(w$TMAX, w$TMAX + 1.5),
      lat = c(37.6475, 35), na.rm = TRUE
    ),
    har_array = har(arr(w$TMIN), arr(w$TMAX), lat = latarr, na.rm = TRUE)
  )
  expect_identical(names(new), names(ref))
  for (k in names(ref)) {
    # The baseline was saved on one platform; other platforms can differ
    # in the last bits of floating-point results
    expect_equal(new[[k]], ref[[k]], tolerance = 1e-12, label = k)
  }
  # An explicit `dates = NULL` is the default
  expect_equal(
    pen(w$TMIN, w$TMAX, w$AWND,
      tsun = w$TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE, dates = NULL
    ),
    ref$pen_tsun,
    tolerance = 1e-12
  )
  expect_equal(
    har(w$TMIN, w$TMAX, lat = 37.6475, na.rm = TRUE, dates = NULL),
    ref$har_lat,
    tolerance = 1e-12
  )
})

test_that("penman() reproduces FAO-56 Example 17 (monthly, Bangkok)", {
  # Allen et al. (1998), Example 17: April, 13 deg 44' N, z = 2 m,
  # Tmax = 34.8, Tmin = 25.6, ea = 2.85 kPa, u2 = 2 m/s, n = 8.5 h,
  # mean temperature of March 29.2 (G = 0.14 MJ m-2 d-1, eq. 44).
  # ET0 = 5.72 mm/day.
  p <- penman(ts(c(24.6, 25.6), start = c(2023, 3), frequency = 12),
    c(33.8, 34.8), c(2, 2),
    lat = 13 + 44 / 60, tsun = c(8.5, 8.5),
    ed = c(2.85, 2.85), z = 2, method = "FAO", verbose = FALSE
  )
  expect_equal(as.numeric(p[2]) / 30, 5.72, tolerance = 0.01, scale = 1)
})

# Daily computation ----------------------------------------------------------

# FAO-56 Example 18: Uccle (Brussels), 6 July, 50 deg 48' N, z = 100 m,
# Tmax = 21.5, Tmin = 12.3, RHmax = 84, RHmin = 63 (ea = 1.409 kPa),
# u2 = 2.078 m/s, n = 9.25 h. Published ET0 = 3.9 mm/day; Ra = 41.09.
ex18_lat <- 50 + 48 / 60
ex18_date <- as.Date("2023-07-06")

# Plain numeric values of a daily (zoo) result
num <- function(x) as.numeric(zoo::coredata(x))

test_that("penman() reproduces FAO-56 Example 18 (daily, Uccle)", {
  p <- penman(12.3, 21.5, 2.078,
    lat = ex18_lat, tsun = 9.25, ed = 1.409,
    z = 100, method = "FAO", dates = ex18_date, verbose = FALSE
  )
  # The published value is rounded to one decimal
  expect_equal(num(p), 3.9, tolerance = 0.05, scale = 1)
  # Same with ea computed from RHmax and RHmin (FAO-56 eq. 17)
  e0 <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))
  ea <- (e0(12.3) * 0.84 + e0(21.5) * 0.63) / 2
  p2 <- penman(12.3, 21.5, 2.078,
    lat = ex18_lat, tsun = 9.25, ed = ea,
    z = 100, method = "FAO", dates = ex18_date, verbose = FALSE
  )
  expect_equal(num(p2), 3.9, tolerance = 0.05, scale = 1)
  # The other methods give similar values
  for (m in c("ICID", "ASCE")) {
    pm <- penman(12.3, 21.5, 2.078,
      lat = ex18_lat, tsun = 9.25, ed = 1.409,
      z = 100, method = m, dates = ex18_date, verbose = FALSE
    )
    expect_equal(num(pm), 3.9, tolerance = 0.05, scale = 1)
  }
})

test_that("hargreaves() daily matches a hand computation (FAO-56 eq. 52)", {
  # Ra given: exact formula
  h <- hargreaves(12.3, 21.5,
    Ra = 41.09, dates = ex18_date,
    verbose = FALSE
  )
  expect_equal(num(h), 0.0023 * 0.408 * 41.09 * (16.9 + 17.8) * sqrt(21.5 - 12.3))
  # Ra estimated from latitude and the day of the year (Ra = 41.09 in
  # FAO-56 Example 18)
  h2 <- hargreaves(12.3, 21.5,
    lat = ex18_lat, dates = ex18_date,
    verbose = FALSE
  )
  expect_equal(h2, h, tolerance = 0.002)
})

test_that("daily ET0 summed over a month is close to the monthly ET0", {
  # Constant daily inputs during July 2023 (and a constant monthly series, so
  # that the monthly G is zero)
  d <- seq(as.Date("2023-07-01"), as.Date("2023-07-31"), by = "day")
  nd <- length(d)
  pd <- penman(rep(15, nd), rep(28, nd), rep(2, nd),
    lat = 40, tsun = rep(10, nd),
    ed = rep(1.4, nd), z = 300, dates = d, verbose = FALSE
  )
  pm <- penman(ts(rep(15, 3), start = c(2023, 6), frequency = 12), rep(28, 3),
    rep(2, 3),
    lat = 40, tsun = rep(10, 3), ed = rep(1.4, 3), z = 300,
    verbose = FALSE
  )
  expect_equal(sum(pd), as.numeric(pm[2]), tolerance = 0.02)
  hd <- hargreaves(rep(15, nd), rep(28, nd), lat = 40, dates = d, verbose = FALSE)
  hm <- hargreaves(ts(rep(15, 3), start = c(2023, 6), frequency = 12),
    rep(28, 3),
    lat = 40, verbose = FALSE
  )
  expect_equal(sum(hd), as.numeric(hm[2]), tolerance = 0.02)
})

test_that("daily penman() sets G to zero", {
  # With G = 0 each day only depends on its own inputs and day of the year:
  # the result for a day does not change with the neighbouring days.
  d <- as.Date("2023-05-01") + 0:2
  p3 <- penman(c(5, 12, 20), c(15, 22, 30), c(2, 2, 2),
    lat = 40,
    tsun = c(8, 8, 8), ed = c(1, 1, 1), z = 100, dates = d, verbose = FALSE
  )
  p1 <- penman(12, 22, 2,
    lat = 40, tsun = 8, ed = 1, z = 100, dates = d[2],
    verbose = FALSE
  )
  expect_equal(p3[2], p1)
  # Gaps between dates are allowed
  p_gap <- penman(c(5, 12), c(15, 22), c(2, 2),
    lat = 40, tsun = c(8, 8),
    ed = c(1, 1), z = 100, dates = as.Date(c("2023-01-15", "2023-05-02")),
    verbose = FALSE
  )
  expect_equal(p_gap[2], p1)
})

test_that("daily computation handles leap years", {
  run_pen <- function(dates) {
    n <- length(dates)
    penman(rep(5, n), rep(15, n), rep(2, n),
      lat = 45, tsun = rep(6, n),
      ed = rep(0.8, n), z = 200, dates = dates, verbose = FALSE
    )
  }
  run_har <- function(dates) {
    n <- length(dates)
    hargreaves(rep(5, n), rep(15, n), lat = 45, dates = dates, verbose = FALSE)
  }
  leap <- as.Date(c("2020-02-28", "2020-02-29", "2020-03-01"))
  for (f in list(run_pen, run_har)) {
    x <- f(leap)
    expect_length(x, 3)
    expect_false(anyNA(x))
    # Day of the year 61 is 1 March in a leap year and 2 March otherwise
    expect_equal(num(x[3]), num(f(as.Date("2021-03-02"))))
    # 29 February (day 60) equals 1 March of a regular year
    expect_equal(num(x[2]), num(f(as.Date("2021-03-01"))))
    # ET0 increases from late February to March at 45 N
    expect_true(all(diff(x) > 0))
  }
  # Character dates are converted with as.Date()
  expect_equal(run_pen(c("2020-02-28", "2020-02-29", "2020-03-01")), run_pen(leap))
})

test_that("daily vector, matrix and 3-d array inputs are consistent", {
  d <- seq(as.Date("2020-12-30"), as.Date("2021-01-03"), by = "day")
  n <- length(d)
  tmin <- c(2, 4, 3, 5, 1)
  tmax <- tmin + c(9, 10, 8, 11, 12)
  u2 <- c(1, 2, 3, 2, 1)
  tsun <- c(3, 5, 7, 2, 0)
  lats <- c(40, -30, 60, 10)
  zs <- c(100, 0, 500, 1500)
  pen1 <- function(i) {
    penman(tmin + i, tmax + i, u2,
      lat = lats[i], tsun = tsun, z = zs[i],
      dates = d, verbose = FALSE
    )
  }
  har1 <- function(i) {
    hargreaves(tmin + i, tmax + i, lat = lats[i], dates = d, verbose = FALSE)
  }
  m <- function(v, k) matrix(rep(v, k), ncol = k)
  tmin_m <- sapply(1:4, function(i) tmin + i)
  tmax_m <- sapply(1:4, function(i) tmax + i)

  # vector: a zoo series indexed by the dates
  v <- pen1(1)
  vh <- har1(1)
  for (x in list(v, vh)) {
    expect_s3_class(x, "zoo")
    expect_identical(zoo::index(x), d)
    expect_null(dim(zoo::coredata(x)))
    expect_length(x, n)
  }
  # values as computed before the output became a zoo object (commit
  # f1815a9, which returned a plain numeric vector)
  expect_equal(num(v), c(
    0.75133485476, 1.18641040963, 1.12998940886,
    1.37402091111, 0.951204673141
  ), tolerance = 1e-10)
  expect_equal(num(vh), c(
    0.980904458021, 1.14023380661, 0.947571223181,
    1.26692267855, 1.17022001705
  ), tolerance = 1e-10)

  # matrix, one latitude and elevation per site: a multi-column zoo
  # series, keeping the column names
  colnames(tmin_m) <- c("A", "B", "C", "D")
  pm <- penman(tmin_m, tmax_m, m(u2, 4),
    lat = lats, tsun = m(tsun, 4), z = zs,
    dates = d, verbose = FALSE
  )
  hm <- hargreaves(tmin_m, tmax_m, lat = lats, dates = d, verbose = FALSE)
  for (x in list(pm, hm)) {
    expect_s3_class(x, "zoo")
    expect_identical(zoo::index(x), d)
    expect_equal(dim(x), c(n, 4))
    expect_identical(colnames(x), c("A", "B", "C", "D"))
  }
  # without column names
  pm0 <- penman(unname(tmin_m), tmax_m, m(u2, 4),
    lat = lats, tsun = m(tsun, 4), z = zs,
    dates = d, verbose = FALSE
  )
  expect_s3_class(pm0, "zoo")
  expect_null(colnames(pm0))
  expect_equal(unname(zoo::coredata(pm0)), unname(zoo::coredata(pm)))
  for (i in 1:4) {
    expect_equal(as.numeric(pm[, i]), num(pen1(i)))
    expect_equal(as.numeric(hm[, i]), num(har1(i)))
  }

  # 3-d array (time first), latitude and elevation as 2-d arrays
  a <- function(x) array(x, dim = c(n, 2, 2))
  pa <- penman(a(tmin_m), a(tmax_m), a(m(u2, 4)),
    lat = array(lats, c(2, 2)),
    tsun = a(m(tsun, 4)), z = array(zs, c(2, 2)), dates = d, verbose = FALSE
  )
  ha <- hargreaves(a(tmin_m), a(tmax_m),
    lat = array(lats, c(2, 2)),
    dates = d, verbose = FALSE
  )
  for (x in list(pa, ha)) {
    expect_true(is.array(x) && !inherits(x, "zoo"))
    expect_equal(dim(x), c(n, 2, 2))
    expect_identical(dimnames(x), list(format(d, "%Y-%m-%d"), NULL, NULL))
  }
  expect_equal(as.numeric(pa), as.numeric(zoo::coredata(pm)))
  expect_equal(as.numeric(ha), as.numeric(zoo::coredata(hm)))
  # other dimnames of the input are kept
  ta <- a(tmin_m)
  dimnames(ta) <- list(NULL, c("x1", "x2"), c("y1", "y2"))
  ha2 <- hargreaves(ta, a(tmax_m),
    lat = array(lats, c(2, 2)),
    dates = d, verbose = FALSE
  )
  expect_identical(
    dimnames(ha2),
    list(format(d, "%Y-%m-%d"), c("x1", "x2"), c("y1", "y2"))
  )
  expect_equal(unname(ha2), unname(ha))

  # Time series input gives a zoo series; the ts attributes are ignored
  pts <- penman(ts(tmin + 1, start = c(2020, 12), frequency = 12), tmax + 1,
    u2,
    lat = lats[1], tsun = tsun, z = zs[1], dates = d, verbose = FALSE
  )
  expect_false(is.ts(pts))
  expect_identical(pts, v)
  hts <- hargreaves(ts(tmin_m, frequency = 365), tmax_m,
    lat = lats,
    dates = d, verbose = FALSE
  )
  expect_s3_class(hts, "zoo")
  expect_false(is.ts(hts))
  expect_identical(zoo::index(hts), d)
  expect_identical(hts, hm)

  # Daily results aggregate to other time steps
  dd <- seq(as.Date("2021-01-01"), as.Date("2021-03-31"), by = "day")
  hd <- hargreaves(rep(5, length(dd)), rep(15, length(dd)),
    lat = 40,
    dates = dd, verbose = FALSE
  )
  hmon <- aggregate(hd, zoo::as.yearmon, sum)
  expect_length(hmon, 3)
  expect_equal(num(hmon)[2], sum(num(hd)[32:59]))
})

test_that("invalid `dates` are rejected", {
  d <- as.Date("2020-01-01") + 0:2
  pen <- function(dates) {
    penman(1:3, 6:8,
      tsun = c(5, 5, 5), lat = 40, z = 100, dates = dates,
      verbose = FALSE
    )
  }
  har <- function(dates) {
    hargreaves(1:3, 6:8, lat = 40, dates = dates, verbose = FALSE)
  }
  for (f in list(pen, har)) {
    expect_error(f(1:3), "must be a `Date` vector")
    expect_error(f(as.POSIXlt(d)), "must be a `Date` vector")
    expect_error(f(d[1:2]), "one element per time step \\(3\\), but it has 2")
    expect_error(f(c(d[1], NA, d[3])), "must not contain NA values")
    expect_error(f(c("2020-01-01", "2020-01-02", NA)), "must not contain NA")
    expect_error(f(d[c(1, 3, 2)]), "strictly increasing order")
    expect_error(f(d[c(1, 1, 2)]), "strictly increasing order")
    expect_silent(f(d))
  }
  # The precipitation-modified Hargreaves is monthly only
  expect_error(
    hargreaves(1:3, 6:8, lat = 40, Pre = c(1, 0, 3), dates = d, verbose = FALSE),
    "can not be used with daily data"
  )
})

test_that("daily computation reports its options", {
  expect_message(
    penman(10, 20,
      tsun = 8, lat = 40, z = 100,
      dates = as.Date("2020-06-01")
    ),
    "mm per day and the soil heat flux is set to zero"
  )
  expect_message(
    hargreaves(10, 20, lat = 40, dates = as.Date("2020-06-01")),
    "mm per day"
  )
})
