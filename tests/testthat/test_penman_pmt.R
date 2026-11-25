context("penman: temperature-only method (PMT), aT and the U2 limit")

pen <- function(...) penman(..., verbose = FALSE)
e0 <- function(t) 0.611 * exp(17.27 * t / (t + 237.3))
# Extraterrestrial radiation, MJ m-2 d-1, as computed in penman()
ra_daily <- function(lat, d) {
  j <- as.numeric(format(as.Date(d), "%j"))
  delta <- 0.409 * sin(0.0172 * j - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * j)
  latr <- lat / 57.2957795
  ws <- acos(-tan(latr) * tan(delta))
  37.6 * dr * (ws * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(ws))
}

# FAO-56 Rev.1 (Pereira et al., 2025), Example 4.4 (pp. 125-126) -----------

test_that("FAO-56 Rev.1 Example 4.4 (Foggia, July) is reproduced", {
  # Monthly average daily Tmax = 32.2 and Tmin = 19.2 ºC, lat = 41º28'N,
  # altitude 74 m, u2 = 2.42 m s-1, dry sub-humid climate (aT = 1, so
  # Tdew = 18.2 ºC), Rs from eq. 3.52 with kRs = 0.19 and G = 0. Published
  # result: ETo = 6.41 mm day-1.
  # The example has two errata: kRs = 0.19 is obtained from eq. 3.54c with
  # TD = 10.1 ºC, not with the stated TDavg = 13.0 ºC (which gives
  # kRs = 0.157 and ETo = 5.76 mm day-1); and Rso = 30.63 MJ m-2 d-1 is
  # obtained with the station altitude z = 74 m, not with the z = 200 m
  # written in the Rso line (which gives 30.73 and ETo = 6.42). With
  # kRs = 0.19 and z = 74 m the published value is reproduced.
  lat <- 41 + 28 / 60
  daily <- pen(19.2, 32.2,
    U2 = 2.42, lat = lat, z = 74, kRs = 0.19, aT = 1,
    method = "FAO", dates = as.Date("2001-07-15")
  )
  expect_equal(as.numeric(daily), 6.41, tolerance = 0.005 / 6.41)
  # Monthly mode: a single July value (G = 0), in mm per month
  monthly <- pen(ts(19.2, start = c(2001, 7), frequency = 12), 32.2,
    U2 = 2.42, lat = lat, z = 74, kRs = 0.19, aT = 1, method = "FAO"
  )
  expect_equal(as.numeric(monthly) / 31, 6.41, tolerance = 0.005 / 6.41)
  # Same as giving the intermediate values Rs and Tdew of eqs. 3.52 and 2.6
  ra <- 40.76
  rs <- 0.19 * sqrt(32.2 - 19.2) * ra
  expect_equal(
    pen(19.2, 32.2,
      U2 = 2.42, Ra = ra, z = 74, kRs = 0.19, aT = 1,
      method = "FAO", dates = "2001-07-15"
    ),
    pen(19.2, 32.2,
      U2 = 2.42, Ra = ra, z = 74, Rs = rs, Tdew = 18.2,
      method = "FAO", dates = "2001-07-15"
    )
  )
})

# Equivalence with explicit Rs ---------------------------------------------

test_that("PMT equals penman() with Rs = kRs sqrt(Tmax - Tmin) Ra", {
  tmin <- c(-2, 0, 3, 6, 10, 14, 17, 16, 12, 8, 3, 0)
  tmax <- c(8, 10, 14, 17, 22, 27, 31, 30, 25, 19, 12, 9)
  ra <- c(14, 19, 26, 34, 39, 41, 40, 36, 29, 21, 16, 13)
  for (method in c("ICID", "FAO", "ASCE")) {
    for (k in c(0.16, 0.19)) {
      rs <- pmin(k * sqrt(tmax - tmin) * ra, 0.75 * ra)
      expect_equal(
        pen(tmin, tmax, Ra = ra, z = 0, kRs = k, method = method),
        pen(tmin, tmax, Ra = ra, z = 0, Rs = rs, method = method)
      )
    }
  }
  # kRs = NULL means 0.16
  expect_identical(
    pen(tmin, tmax, Ra = ra, z = 0),
    pen(tmin, tmax, Ra = ra, z = 0, kRs = 0.16)
  )
  # Daily data
  d <- seq(as.Date("2020-01-01"), by = "day", length.out = 12)
  expect_equal(
    pen(tmin, tmax, lat = 40, z = 500, kRs = 0.19, dates = d),
    pen(tmin, tmax, Ra = ra_daily(40, d), z = 500, kRs = 0.19, dates = d)
  )
})

test_that("PMT Rs is limited to Rso and is zero when Tmax <= Tmin", {
  ra <- rep(30, 3)
  # kRs sqrt(TD) = 0.19 * sqrt(30) = 1.04 > 0.75 + 2e-5 z
  capped <- pen(c(0, 10, 10), c(30, 10, 8), Ra = ra, z = 1000, kRs = 0.19,
    dates = c("2020-06-01", "2020-06-02", "2020-06-03")
  )
  rso <- (0.75 + 2e-5 * 1000) * 30
  expect_equal(
    capped[1],
    pen(0, 30, Ra = 30, z = 1000, Rs = rso, dates = "2020-06-01")
  )
  # Tmax == Tmin and Tmax < Tmin: Rs = 0, so Rn = 0
  expect_equal(
    as.numeric(capped[2:3]),
    as.numeric(pen(c(10, 10), c(10, 8),
      Ra = c(30, 30), z = 1000, Rs = c(0, 0),
      dates = c("2020-06-02", "2020-06-03")
    ))
  )
})

test_that("PMT works with matrix and 3-d array input, scalar or per-site kRs", {
  data("wichita", package = "SPEI", envir = environment())
  w <- wichita
  tmin <- cbind(w$TMIN, w$TMIN + 1.5)
  tmax <- cbind(w$TMAX, w$TMAX + 1.5)
  lat <- c(37.6475, 35)
  z <- c(402.6, 100)
  both <- pen(tmin, tmax, lat = lat, z = z, kRs = c(0.16, 0.19), na.rm = TRUE)
  expect_equal(dim(both), dim(tmin))
  for (j in 1:2) {
    expect_equal(
      as.numeric(both[, j]),
      pen(tmin[, j], tmax[, j],
        lat = lat[j], z = z[j],
        kRs = c(0.16, 0.19)[j], na.rm = TRUE
      )
    )
  }
  # Scalar kRs is recycled to all sites
  expect_equal(
    pen(tmin, tmax, lat = lat, z = z, kRs = 0.19, na.rm = TRUE),
    pen(tmin, tmax, lat = lat, z = z, kRs = c(0.19, 0.19), na.rm = TRUE)
  )
  # 3-d array, per-site kRs and aT given as 2 x 2 matrices
  nt <- nrow(w)
  arr <- function(x) array(x, dim = c(nt, 2, 2))
  latarr <- array(c(40, 30, 40, 30), dim = c(2, 2))
  zarr <- array(c(400, 10, 1000, 50), dim = c(2, 2))
  karr <- array(c(0.16, 0.17, 0.18, 0.19), dim = c(2, 2))
  aarr <- array(c(0, 1, 2, 3), dim = c(2, 2))
  a <- pen(arr(w$TMIN), arr(w$TMAX),
    lat = latarr, z = zarr, kRs = karr,
    aT = aarr, na.rm = TRUE
  )
  expect_equal(dim(a), c(nt, 2, 2))
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(
        as.numeric(a[, i, j]),
        pen(w$TMIN, w$TMAX,
          lat = latarr[i, j], z = zarr[i, j],
          kRs = karr[i, j], aT = aarr[i, j], na.rm = TRUE
        )
      )
    }
  }
  # Daily data, matrix input: zoo output
  d <- seq(as.Date("2021-01-01"), by = "day", length.out = 365)
  tn <- 5 + 10 * sin(2 * pi * (seq_along(d) - 110) / 365)
  tx <- tn + 10
  zd <- pen(cbind(a = tn, b = tn), cbind(a = tx, b = tx),
    lat = c(40, 40),
    z = c(0, 0), kRs = c(0.16, 0.19), dates = d
  )
  expect_s3_class(zd, "zoo")
  # A larger kRs gives more radiation and, over the year, a larger ETo
  expect_gt(sum(zd[, 2]), sum(zd[, 1]))
  expect_equal(
    as.numeric(zd[, 2]),
    as.numeric(pen(tn, tx, lat = 40, z = 0, kRs = 0.19, dates = d))
  )
})

# aT -----------------------------------------------------------------------

test_that("aT shifts the dewpoint temperature estimated from Tmin", {
  tmin <- c(5, 10, 15)
  tmax <- c(20, 25, 30)
  args <- list(Tmin = tmin, Tmax = tmax, U2 = c(2, 2, 2), Ra = c(30, 35, 40),
    Rs = c(18, 22, 26), z = 100, dates = c("2020-05-01", "2020-05-02",
      "2020-05-03"))
  for (at in c(-1, 2, 4)) {
    expect_equal(
      do.call(pen, c(args, list(aT = at))),
      do.call(pen, c(args, list(Tdew = tmin - at)))
    )
    expect_equal(
      do.call(pen, c(args, list(aT = at))),
      do.call(pen, c(args, list(ed = e0(tmin - at))))
    )
  }
  # A larger aT (drier air) gives a larger ETo
  expect_true(all(
    do.call(pen, c(args, list(aT = 2))) > do.call(pen, c(args, list(aT = 0)))
  ))
})

test_that("aT = 0 and no kRs give the same results as before", {
  ref <- readRDS("data/pet_monthly_baseline.rds")
  data("wichita", "cabinda", package = "SPEI", envir = environment())
  w <- wichita
  # ed estimated from Tmin
  expect_equal(
    pen(w$TMIN, w$TMAX,
      tsun = w$TSUN, lat = 37.6475, z = 402.6,
      na.rm = TRUE, aT = 0, kRs = NULL
    ),
    ref$pen_const,
    tolerance = 1e-12
  )
  expect_identical(
    pen(w$TMIN, w$TMAX, w$AWND,
      CC = w$ACSH, lat = 37.6475, z = 402.6,
      na.rm = TRUE, aT = 0
    ),
    pen(w$TMIN, w$TMAX, w$AWND,
      CC = w$ACSH, lat = 37.6475, z = 402.6,
      na.rm = TRUE
    )
  )
})

# Argument checks ------------------------------------------------------------

test_that("kRs and aT are validated", {
  x <- list(Tmin = c(10, 12), Tmax = c(20, 24), lat = 40, z = 100)
  expect_error(do.call(penman, c(x, kRs = 0.5)), "`kRs` must be numeric")
  expect_error(do.call(penman, c(x, kRs = 0.05)), "`kRs` must be numeric")
  expect_error(do.call(penman, c(x, kRs = NA)), "`kRs` must be numeric")
  expect_error(do.call(penman, c(x, kRs = "a")), "`kRs` must be numeric")
  expect_error(
    do.call(penman, c(x, list(kRs = c(0.16, 0.19)))),
    "`kRs` must have length 1 or one value per site"
  )
  expect_error(do.call(penman, c(x, aT = 12)), "`aT` must be numeric")
  expect_error(do.call(penman, c(x, aT = NA)), "`aT` must be numeric")
  expect_error(
    do.call(penman, c(x, list(aT = c(1, 2)))),
    "`aT` must have length 1 or one value per site"
  )
})

test_that("kRs and aT are ignored, with a warning, when not used", {
  x <- list(
    Tmin = c(10, 12), Tmax = c(20, 24), lat = 40, z = 100,
    verbose = FALSE
  )
  with_rs <- do.call(penman, c(x, list(Rs = c(15, 18))))
  expect_warning(
    r <- do.call(penman, c(x, list(Rs = c(15, 18), kRs = 0.19))),
    "`kRs` is ignored"
  )
  expect_identical(r, with_rs)
  with_ed <- do.call(penman, c(x, list(ed = c(1, 1.2))))
  expect_warning(
    r <- do.call(penman, c(x, list(ed = c(1, 1.2), aT = 2))),
    "`aT` is ignored"
  )
  expect_identical(r, with_ed)
  # aT = 0 is the default and gives no warning
  expect_silent(do.call(penman, c(x, list(ed = c(1, 1.2), aT = 0))))
})

test_that("radiation inputs with only NA values are treated as missing", {
  x <- list(Tmin = c(10, 12), Tmax = c(20, 24), lat = 40, z = 100)
  pmt <- do.call(pen, x)
  expect_message(
    r <- do.call(penman, c(x, list(Rs = c(NA, NA), na.rm = TRUE))),
    "`Rs` contains only NA values and is ignored"
  )
  expect_identical(r, pmt)
})

# U2 lower limit ---------------------------------------------------------------

test_that("U2 is limited to at least 0.5 m s-1 (FAO-56 Rev.1, p. 105)", {
  x <- list(
    Tmin = c(10, 12, 14), Tmax = c(20, 24, 26), Rs = c(15, 18, 20),
    lat = 40, z = 100
  )
  for (method in c("ICID", "FAO", "ASCE")) {
    expect_identical(
      do.call(pen, c(x, list(U2 = c(0, 0.2, 1), method = method))),
      do.call(pen, c(x, list(U2 = c(0.5, 0.5, 1), method = method)))
    )
  }
  expect_message(
    do.call(penman, c(x, list(U2 = c(0, 0.2, 1)))),
    "2 values of `U2` below 0.5 m s-1 were set to 0.5 m s-1"
  )
  # NA values are kept
  r <- do.call(pen, c(x, list(U2 = c(NA, 0.2, 1), na.rm = TRUE)))
  expect_true(is.na(r[1]))
  expect_false(anyNA(r[2:3]))
})
