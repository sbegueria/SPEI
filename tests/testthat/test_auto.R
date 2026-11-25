context("distribution = 'auto' and sedi()")

data(wichita)
w_bal <- ts(wichita$PRCP - thornthwaite(wichita$TMED, 37.6475, verbose = FALSE),
  start = c(1980, 1), frequency = 12
)
w_prcp <- ts(wichita$PRCP, start = c(1980, 1), frequency = 12)
data(balance)
data(cruts4)
ref <- readRDS("data/flexdroughtindex_ref.rds")

# FlexDroughtIndex sets infinite values to +-2.88
clamp_inf <- function(z) {
  z[z == Inf] <- 2.88
  z[z == -Inf] <- -2.88
  z
}

test_that("criterion 'aic' reproduces FlexDroughtIndex", {
  old <- options(SPEI.auto.criterion = "aic")
  on.exit(options(old))
  for (s in c(1, 3)) {
    expect_equal(
      clamp_inf(sedi(ref$def, s, verbose = FALSE)$fitted),
      ref[[paste0("sedi_", s)]],
      tolerance = 1e-6
    )
    expect_equal(
      clamp_inf(spei(ref$bal, s, distribution = "auto", verbose = FALSE)$fitted),
      ref[[paste0("spei_", s)]],
      tolerance = 1e-6
    )
    expect_equal(
      clamp_inf(spi(ref$prcp, s, distribution = "auto", verbose = FALSE)$fitted),
      ref[[paste0("spi_", s)]],
      tolerance = 1e-6
    )
  }
})

test_that("each candidate distribution matches lmom", {
  lm3 <- c(10, 4, 0.1)
  x <- c(2, 8, 10, 15, 30)
  pel <- list(lmom::pelgev, lmom::pelglo, lmom::pelgpa, lmom::pelln3,
              lmom::pelpe3, lmom::pelwei)
  cdf <- list(lmom::cdfgev, lmom::cdfglo, lmom::cdfgpa, lmom::cdfln3,
              lmom::cdfpe3, lmom::cdfwei)
  for (k in seq_along(auto_dists)) {
    p <- auto_pel(k, lm3)
    expect_equal(p, unname(pel[[k]](lm3)))
    expect_equal(auto_cdf(x, k, p), cdf[[k]](x, p))
    expect_equal(auto_z(x, k, p), qnorm(cdf[[k]](x, p)))
  }
  # invalid L-moments: no fit
  expect_null(auto_pel(1, c(10, -1, 0.1)))
})

test_that("the choice is the most normal candidate and is reproducible", {
  a <- spei(w_bal, 1, distribution = "auto", verbose = FALSE)
  b <- spei(w_bal, 1, distribution = "auto", verbose = FALSE)
  expect_identical(a$selected, b$selected)
  expect_equal(a$fitted, b$fitted)
  expect_equal(a$distribution, "auto")
  expect_equal(dim(a$selected), c(1, 12))
  expect_true(all(a$selected %in% auto_dists))
  expect_equal(a$selected[1, ], auto_dists[a$coefficients["dist", 1, ]])
  expect_equal(
    dimnames(a$coefficients)$par,
    c("dist", "location", "scale", "shape")
  )
  # W of the chosen fit is the largest among the candidates (no infinite
  # values here), in particular not smaller than with the log-Logistic
  d <- spei(w_bal, 1, verbose = FALSE)
  for (m in 1:12) {
    za <- a$fitted[cycle(w_bal) == m]
    zd <- d$fitted[cycle(w_bal) == m]
    expect_gte(
      shapiro.test(za)$statistic + 1e-12,
      shapiro.test(zd)$statistic
    )
  }
})

test_that("params round trip, also with a reference period", {
  for (f in list(spei, spi)) {
    x <- if (identical(f, spi)) w_prcp else w_bal
    a <- f(x, 3, distribution = "auto", ref.end = c(2000, 12), verbose = FALSE)
    b <- f(x, 3, distribution = "auto", params = a$coefficients, verbose = FALSE)
    expect_equal(b$fitted, a$fitted)
    expect_identical(b$selected, a$selected)
  }
  a <- sedi(ref$def, 3, verbose = FALSE)
  b <- sedi(ref$def, 3, params = a$coefficients, verbose = FALSE)
  expect_equal(b$fitted, a$fitted)
})

test_that("spi() with zeros uses the probability of zero", {
  pz <- w_prcp
  pz[cycle(pz) == 7][1:8] <- 0
  s <- spi(pz, 1, distribution = "auto", verbose = FALSE)
  expect_equal(dimnames(s$coefficients)$par[5], "pze")
  expect_equal(unname(s$coefficients["pze", 1, 7]), 8 / 32)
  co <- s$coefficients[, 1, 7]
  z0 <- qnorm(co[["pze"]] + (1 - co[["pze"]]) *
    auto_cdf(0, co[["dist"]], co[2:4]))
  expect_equal(unique(s$fitted[pz == 0 & cycle(pz) == 7]), unname(z0))
  expect_equal(
    unname(s$coefficients["pze", 1, 1]),
    mean(w_prcp[cycle(w_prcp) == 1] == 0)
  )
  expect_warning(
    spi(w_bal, 1, distribution = "auto", verbose = FALSE),
    "negative values"
  )
})

test_that("sedi() handles zeros, positive values and its options", {
  d <- ts(ref$def, start = c(1981, 1), frequency = 12)
  d[cycle(d) == 1][1:5] <- 0
  s <- sedi(d, 1, verbose = FALSE)
  expect_s3_class(s, "spei")
  expect_equal(s$index, "SEDI")
  expect_equal(unname(s$coefficients["pze", 1, 1]), 5 / 40)
  expect_equal(unique(s$fitted[d == 0]), qnorm(1 - 5 / 40))
  expect_true(all(s$fitted[d == 0] > s$fitted[cycle(d) == 1 & d < 0]))
  # log-Logistic: as spei()
  expect_equal(
    sedi(d, 3, distribution = "log-Logistic", verbose = FALSE)$fitted,
    spei(d, 3, verbose = FALSE)$fitted
  )
  expect_error(sedi(d, 1, distribution = "Gamma"), "`auto` or `log-Logistic`")
  dp <- d
  dp[3] <- 5
  expect_warning(sedi(dp, 1, verbose = FALSE), "positive values")
  expect_message(sedi(d, 1), "Evapotranspiration Deficit Index \\(SEDI\\)")
  expect_silent(plot(s))
  expect_equal(plot(s)$labels$y, "SEDI (z-values)")
})

test_that("matrix and 3-d array input", {
  m <- spei(as.matrix(balance[, 1:3]), 3, distribution = "auto", verbose = FALSE)
  expect_equal(dim(m$selected), c(3, 12))
  expect_equal(rownames(m$selected), colnames(balance)[1:3])
  one <- spei(balance[, 2], 3, distribution = "auto", verbose = FALSE)
  expect_equal(as.numeric(m$fitted[, 2]), as.numeric(one$fitted))
  expect_identical(unname(m$selected[2, ]), one$selected[1, ])
  a <- spei(cruts4, 1, distribution = "auto", verbose = FALSE)
  expect_equal(dim(a$fitted), dim(cruts4))
  expect_equal(dim(a$selected), c(6, 12))
  b <- spei(cruts4, 1, distribution = "auto", params = a$coefficients,
            verbose = FALSE)
  expect_equal(b$fitted, a$fitted)
})

test_that("non-monthly data, kernel, na.rm and pp-pwm", {
  data(tarazona)
  x <- tarazona$clim[, "PRCP"] - tarazona$clim[, "ETO"]
  a <- spei(x, 4, distribution = "auto", verbose = FALSE)
  expect_equal(dim(a$coefficients), c(4, 1, 48))
  expect_equal(dim(a$selected), c(1, 48))
  expect_equal(frequency(a$fitted), 48)
  g <- spei(w_bal, 6, distribution = "auto", verbose = FALSE,
            kernel = list(type = "gaussian", shift = 0))
  expect_equal(sum(is.na(g$fitted)), 5)
  xn <- w_bal
  xn[c(10, 50)] <- NA
  expect_equal(
    sum(is.na(spei(xn, 1, distribution = "auto", na.rm = TRUE,
                   verbose = FALSE)$fitted)),
    2
  )
  pp <- spei(w_bal, 1, distribution = "auto", fit = "pp-pwm", verbose = FALSE)
  expect_equal(pp$fit, "pp-pwm")
  expect_warning(
    spei(w_bal, 1, distribution = "auto", fit = "max-lik", verbose = FALSE),
    "using `ub-pwm` instead"
  )
})

test_that("errors and messages", {
  a <- spei(w_bal, 1, distribution = "auto", verbose = FALSE)
  expect_error(
    spei(w_bal, 1, distribution = "auto", params = a$coefficients[2:4, , , drop = FALSE]),
    "dimensions \\(4, 1, 12\\)"
  )
  bad <- a$coefficients
  bad["dist", 1, 1] <- 9
  expect_error(spei(w_bal, 1, distribution = "auto", params = bad), "distribution code")
  old <- options(SPEI.auto.criterion = "ks")
  on.exit(options(old))
  expect_error(spei(w_bal, 1, distribution = "auto"), "SPEI.auto.criterion")
  options(old)
  expect_message(spei(w_bal, 1, distribution = "auto"), "best of several distributions")
  out <- capture.output(summary(a))
  expect_true(any(grepl("dist", out)))
  expect_true(any(grepl(a$selected[1, 1], out)))
})
