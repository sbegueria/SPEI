context("PET regression tests (2026 audit)")

tx <- c(5, 8, 13, 18, 23, 28, 31, 30, 25, 19, 11, 6)
n <- 13 # not a multiple of 12
Tmax <- rep(tx, 2)[1:n]
Tmin <- Tmax - 11
U2 <- rep(2, n)
tsun <- rep(8, n)
ed <- rep(1, n)

# FAO-56 eq. 21 extraterrestrial radiation, with omega_s = pi in polar day
ra_fao <- function(lat, J) {
  latr <- lat * pi / 180
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * J)
  x <- -tan(latr) * tan(delta)
  ws <- ifelse(x < -1, pi, ifelse(x > 1, 0, acos(pmin(pmax(x, -1), 1))))
  37.6 * dr * (ws * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(ws))
}
J_mid <- cumsum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) -
  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) + 15

test_that("matrix input keeps the calendar for every site (P1)", {
  h1 <- hargreaves(Tmin, Tmax, lat = 40, verbose = FALSE)
  h2 <- hargreaves(cbind(Tmin, Tmin), cbind(Tmax, Tmax),
    lat = c(40, 40), verbose = FALSE
  )
  expect_equal(as.numeric(h2[, 1]), as.numeric(h1))
  expect_equal(as.numeric(h2[, 2]), as.numeric(h1))

  t1 <- thornthwaite(Tmax, 40, verbose = FALSE)
  t2 <- thornthwaite(cbind(Tmax, Tmax), c(40, 40), verbose = FALSE)
  expect_equal(as.numeric(t2[, 2]), as.numeric(t1))

  p1 <- penman(Tmin, Tmax, U2,
    lat = 40, tsun = tsun, ed = ed, z = 100,
    verbose = FALSE
  )
  p2 <- penman(cbind(Tmin, Tmin), cbind(Tmax, Tmax), cbind(U2, U2),
    lat = c(40, 40), tsun = cbind(tsun, tsun), ed = cbind(ed, ed),
    z = c(100, 100), verbose = FALSE
  )
  expect_equal(as.numeric(p2[, 2]), as.numeric(p1))
})

test_that("penman uses each site's elevation with P0 (P2)", {
  run <- function(z, k = 1) {
    m <- function(v) matrix(rep(v, k), ncol = k)
    penman(m(Tmin), m(Tmax), m(U2),
      lat = rep(13.7, k), tsun = m(tsun), ed = m(ed), z = z,
      P0 = matrix(101.3, n, k), verbose = FALSE
    )
  }
  both <- run(c(0, 3000), 2)
  expect_equal(as.numeric(both[, 1]), as.numeric(run(0)))
  expect_equal(as.numeric(both[, 2]), as.numeric(run(3000)))
})

test_that("penman uses z for clear-sky radiation whatever the pressure input (P3)", {
  z_only <- penman(Tmin, Tmax, U2,
    lat = 40, tsun = tsun, ed = ed, z = 3000,
    verbose = FALSE
  )
  with_p0 <- penman(Tmin, Tmax, U2,
    lat = 40, tsun = tsun, ed = ed, z = 3000,
    P0 = rep(101.3, n), verbose = FALSE
  )
  P <- rep(101.3 * ((293 - 0.0065 * 3000) / 293)^5.26, n)
  with_p <- penman(Tmin, Tmax, U2,
    lat = 40, tsun = tsun, ed = ed, z = 3000,
    P = P, verbose = FALSE
  )
  expect_equal(as.numeric(with_p0), as.numeric(z_only))
  expect_equal(as.numeric(with_p), as.numeric(z_only))
  expect_error(
    penman(Tmin, Tmax, U2, lat = 40, tsun = tsun, ed = ed, P = P),
    "Argument `z`"
  )
})

test_that("extraterrestrial radiation uses a 24 h day in polar summer (P4)", {
  Tx12 <- rep(15, 12)
  Tn12 <- rep(5, 12)
  ra <- ra_fao(75, J_mid)
  expect_equal(
    as.numeric(hargreaves(Tn12, Tx12, lat = 75, verbose = FALSE)),
    as.numeric(hargreaves(Tn12, Tx12, Ra = ra, verbose = FALSE)),
    tolerance = 1e-6
  )
  rs <- rep(15, 12)
  expect_equal(
    as.numeric(penman(Tn12, Tx12, rep(2, 12),
      lat = 75, Rs = rs, ed = rep(1, 12), z = 0, verbose = FALSE
    )),
    as.numeric(penman(Tn12, Tx12, rep(2, 12),
      Ra = ra, Rs = rs, ed = rep(1, 12), z = 0, verbose = FALSE
    )),
    tolerance = 1e-6
  )
})

# Audit batch 2 regression tests

test_that("penman() with tsun in polar night returns values (P5)", {
  r <- penman(
    Tmin = rep(-20, 12), Tmax = rep(-10, 12), U2 = rep(2, 12),
    tsun = rep(0, 12), RH = rep(80, 12), lat = 80, z = 10, verbose = FALSE
  )
  expect_false(anyNA(r))
})

test_that("thornthwaite() gives zero PET for an all-cold site among others (P6)", {
  t2 <- as.numeric(wichita$TMED[1:24])
  r <- thornthwaite(cbind(rep(-5, 24), t2), lat = c(70, 37), verbose = FALSE)
  expect_false(anyNA(r))
  expect_true(all(r[, 1] == 0))
  expect_equal(unname(r[, 2]), as.numeric(thornthwaite(t2, 37, verbose = FALSE)))
})

test_that("hargreaves() accepts Pre as a matrix (P7, PR #59)", {
  n <- nrow(wichita)
  m <- function(v) matrix(rep(as.numeric(v), 2), ncol = 2)
  r <- hargreaves(m(wichita$TMIN), m(wichita$TMAX),
    lat = c(37.6, 37.6),
    Pre = m(wichita$PRCP), verbose = FALSE
  )
  single <- hargreaves(as.numeric(wichita$TMIN), as.numeric(wichita$TMAX),
    lat = 37.6, Pre = as.numeric(wichita$PRCP), verbose = FALSE
  )
  expect_equal(unname(r[, 1]), as.numeric(single))
  expect_equal(unname(r[, 2]), as.numeric(single))
})

test_that("penman() works with one or two time steps (P8)", {
  args <- list(
    Tmin = c(10, 12, 14), Tmax = c(20, 24, 26), U2 = c(2, 2, 2),
    Rs = c(15, 18, 20), RH = c(60, 60, 60), lat = 40, z = 100,
    verbose = FALSE
  )
  sub <- function(k) lapply(args, function(a) if (length(a) == 3) a[1:k] else a)
  expect_length(do.call(penman, sub(1)), 1)
  expect_length(do.call(penman, sub(2)), 2)
  expect_false(anyNA(do.call(penman, sub(2))))
})

test_that("thornthwaite() requires at least 12 months (P9)", {
  expect_error(
    thornthwaite(as.numeric(wichita$TMED[6:8]), 37, verbose = FALSE),
    "Input data must cover at least one full year"
  )
  expect_error(
    thornthwaite(5, 37, verbose = FALSE),
    "Input data must cover at least one full year"
  )
})

# Audit batch 3 regression tests

test_that("PET functions report options with message() (O8)", {
  t <- as.numeric(wichita$TMED[1:24])
  expect_message(thornthwaite(t, 37), "Input type is vector")
  expect_silent(suppressMessages(thornthwaite(t, 37)))
  expect_silent(suppressMessages(hargreaves(
    as.numeric(wichita$TMIN[1:24]), as.numeric(wichita$TMAX[1:24]),
    lat = 37
  )))
})

test_that("penman() rejects an unknown `method` (P12)", {
  expect_error(
    penman(
      Tmin = rep(10, 12), Tmax = rep(20, 12), U2 = rep(2, 12),
      Rs = rep(15, 12), RH = rep(60, 12), lat = 40, z = 100,
      method = "fao", verbose = FALSE
    ),
    "Argument `method` must be one of `ICID`, `ASCE` or `FAO`."
  )
})

test_that("na.rm and verbose must be TRUE or FALSE in PET functions (L1)", {
  t <- as.numeric(wichita$TMED[1:24])
  expect_error(
    thornthwaite(t, 37, na.rm = NA, verbose = FALSE),
    "Argument `na.rm` must be set to either TRUE or FALSE."
  )
  expect_error(
    thornthwaite(t, 37, verbose = "yes"),
    "Argument `verbose` must be set to either TRUE or FALSE."
  )
})
