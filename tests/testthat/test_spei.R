context("spei")

data(wichita)
attach(wichita)
wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
wichita$BAL <- wichita$PRCP - wichita$PET

data(balance)

data(cruts4)

x_vec <- as.numeric(wichita$BAL)
x_tsvec <- ts(x_vec, c(1980, 1), fr = 12)
x_mat <- as.matrix(balance)
x_tsmat <- ts(x_mat, c(1980, 1), fr = 12)
x_array <- cruts4

# Test input data checks and error messages

# spei(x_vec, 1)

test_that("Non-numeric scale", {
  expect_error(
    spei(x_vec, scale = "1"),
    "Argument `scale` must be numeric."
  )
})

test_that("Non-list kernel", {
  expect_error(
    spei(x_vec, 1, kernel = "a"),
    "Argument `kernel` must be a list."
  )
})

test_that("Badly conformed kernel", {
  expect_error(
    spei(x_vec, 1, kernel = list(a = 1, b = 0)),
    "Argument `kernel` must be a list with components `type` and `shift`."
  )
})

test_that("Badly conformed kernel 2", {
  expect_error(
    spei(x_vec, 1, kernel = list(type = 1, shift = 0)),
    "Element `type` of `kernel` must be a single valued character vector"
  )
})

test_that("Bad distribution", {
  expect_error(
    spei(x_vec, 1, distribution = "moments"),
    "Argument `distribution` must be one of `log-Logistic`, `Gamma` or `PearsonIII`."
  )
})

test_that("Bad fit", {
  expect_error(
    spei(x_vec, 1, fit = "logistic"),
    "Argument `fit` must be one of `ub-pwm`, `pp-pwm` or `max-lik`."
  )
})

test_that("Bad ref.start", {
  expect_error(
    spei(x_vec, 1, ref.start = 1),
    "Argument `ref.start` must be a numeric vector of length two."
  )
})

test_that("Bad ref.start 2", {
  expect_error(
    spei(x_vec, 1, ref.start = c("a", "b")),
    "Argument `ref.start` must be a numeric vector of length two."
  )
})

test_that("Bad ref.end", {
  expect_error(
    spei(x_vec, 1, ref.end = 1),
    "Argument `ref.end` must be a numeric vector of length two."
  )
})

test_that("Bad ref.end 2", {
  expect_error(
    spei(x_vec, 1, ref.end = c("a", "b")),
    "Argument `ref.end` must be a numeric vector of length two."
  )
})

test_that("There are NAs when na.rm=FALSE", {
  expect_error(
    spei(c(NA, x_vec), 1),
    "`data` must not contain NA values if argument `na.rm` is set to FALSE."
  )
})

test_that("Incorrect input dimensions", {
  expect_error(
    spei(array(x_vec, c(382, 2, 2, 2)), 1),
    "Input data can not have more than three dimensions."
  )
})

test_that("Incorrect data type: passing a data.frame", {
  expect_error(
    spei(as.data.frame(x_mat), 1),
    "Bad data type: input must be a vector, tsvector, matrix, tsmatrix, or 3-d array."
  )
})


# Test function results

# out <- spei(x_vec, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_vector.rds')
test_that("example with vector data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_vector.rds"),
    spei(x_vec, 1)$fitted
  )
})

# out <- spei(x_vec, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_vector.rds')
test_that("example with vector data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_vector.rds"),
    spei(x_vec, 12)$fitted
  )
})

# out <- spei(x_tsvec, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_tsvector.rds')
test_that("example with tsvector data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_tsvector.rds"),
    spei(x_tsvec, 1)$fitted
  )
})

# out <- spei(x_tsvec, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_tsvector.rds')
test_that("example with tsvector data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_tsvector.rds"),
    spei(x_tsvec, 12)$fitted
  )
})

# out <- spei(x_mat, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_matrix.rds')
test_that("example with matrix data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_matrix.rds"),
    spei(x_mat, 1)$fitted
  )
})

# out <- spei(x_mat, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_matrix.rds')
test_that("example with matrix data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_matrix.rds"),
    spei(x_mat, 12)$fitted
  )
})

# out <- spei(x_tsmat, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_tsmatrix.rds')
#test_that("example with tsmatrix data, scale 1", {
#  expect_equal(
#    readRDS("data/spei_1_tsmatrix.rds"),
#    spei(x_tsmat, 1)$fitted
#  )
#})

# out <- spei(x_tsmat, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_tsmatrix.rds')
#test_that("example with tsmatrix data, scale 12", {
#  expect_equal(
#    readRDS("data/spei_12_tsmatrix.rds"),
#    spei(x_tsmat, 12)$fitted
#  )
#})

# out <- spei(x_array, 1)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_1_array.rds')
test_that("example with 3-d array data, scale 1", {
  expect_equal(
    readRDS("data/spei_1_array.rds"),
    spei(x_array, 1)$fitted
  )
})

# out <- spei(x_array, 12)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_12_array.rds')
test_that("example with 3-d array data, scale 12", {
  expect_equal(
    readRDS("data/spei_12_array.rds"),
    spei(x_array, 12)$fitted
  )
})

rs <- c(10, 1)
re <- c(25, 12)

# out <- spei(x_vec, 12, ref.start=rs, ref.end=re)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_refst_refend.rds')
test_that("example with both ref.start and ref.end", {
  expect_equal(
    readRDS("data/spei_refst_refend.rds"),
    spei(x_vec, 12, ref.start = rs, ref.end = re)$fitted
  )
})

# out <- spei(x_vec, 12, ref.start=rs)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_refst.rds')
test_that("example with only ref.start", {
  expect_equal(
    readRDS("data/spei_refst.rds"),
    spei(x_vec, 12, ref.start = rs)$fitted
  )
})

# out <- spei(x_vec, 12, ref.end=rs)$fitted
# saveRDS(out, file='./tests/testthat/data/spei_refend.rds')
test_that("example with only ref.end", {
  expect_equal(
    readRDS("data/spei_refend.rds"),
    spei(x_vec, 12, ref.end = rs)$fitted
  )
})

# out <- spei(x_vec, 1, distribution='Gamma')$fitted
# saveRDS(out, file='./tests/testthat/data/spei_gamma.rds')
test_that("example with Gamma distribution", {
  expect_equal(
    readRDS("data/spei_gamma.rds"),
    expect_warning(
      spei(x_vec, 1, distribution = "Gamma"),
      "negative values"
    )$fitted
  )
})

# out <- spei(x_vec, 1, distribution='PearsonIII')$fitted
# saveRDS(out, file='./tests/testthat/data/spei_pe3.rds')
test_that("example with PearsonIII distribution", {
  expect_equal(
    readRDS("data/spei_pe3.rds"),
    expect_warning(
      spei(x_vec, 1, distribution = "PearsonIII"),
      "negative values"
    )$fitted
  )
})

pars <- spei(x_vec, 1)$coefficients
test_that("example with user-provided parameters", {
  expect_equal(
    readRDS("data/spei_1_vector.rds"),
    spei(x_vec, 1, params = pars)$fitted
  )
})

# tests for different calculation methods: pp-pwm, max-lik

test_that("plot works without errors/warnings", {
  pdf(NULL) # Stops Rplots.pdf from being created
  spei1 <- spei(x_vec, 1)
  expect_error(plot(spei1), NA)
})

# Regression tests for the 2026 audit (H1-H4) - - - - - - - - - - - - - - -

bal_ts <- ts(wichita$BAL, start = c(1980, 1), frequency = 12)
prcp_ts <- ts(wichita$PRCP, start = c(1980, 1), frequency = 12)

test_that("fit = 'max-lik' and 'pp-pwm' return values (H1, H2)", {
  ub <- spei(bal_ts, 3, verbose = FALSE)$fitted
  ml <- spei(bal_ts, 3, fit = "max-lik", verbose = FALSE)$fitted
  pp <- spei(bal_ts, 3, fit = "pp-pwm", verbose = FALSE)$fitted
  expect_equal(sum(is.na(ml)), 2)
  expect_equal(sum(is.na(pp)), 2)
  expect_gt(cor(ml, ub, use = "complete.obs"), 0.99)
  expect_gt(cor(pp, ub, use = "complete.obs"), 0.99)
  # max-lik maximises the likelihood: its log-likelihood is not lower than
  # that of the PWM estimates for every month
  x <- spei(bal_ts, 3, verbose = FALSE)
  y <- spei(bal_ts, 3, fit = "max-lik", verbose = FALSE)
  acu <- stats::filter(bal_ts, rep(1, 3), sides = 1)
  for (m in 1:12) {
    v <- acu[cycle(bal_ts) == m]
    v <- v[!is.na(v)]
    ll <- function(p) sum(log(lmomco::pdfglo(v, lmomco::vec2par(p, "glo"))))
    expect_gte(ll(y$coefficients[, 1, m]), ll(x$coefficients[, 1, m]) - 1e-6)
  }
})

test_that("fit = 'max-lik' with Gamma falls back to ub-pwm with a warning", {
  expect_warning(
    s <- spi(prcp_ts, 3, fit = "max-lik", verbose = FALSE),
    "only available for the log-Logistic"
  )
  expect_equal(s$fitted, spi(prcp_ts, 3, verbose = FALSE)$fitted)
})

test_that("early exits do not affect other series or months (H3, #64)", {
  set.seed(2)
  a <- rnorm(120, 50, 20)
  b <- rnorm(120, 50, 20)
  b[seq(1, 120, 12)] <- 10 # constant January in series 2
  one <- spei(ts(a, frequency = 12), 1, verbose = FALSE)$fitted
  expect_warning(
    two <- spei(ts(cbind(a, b), frequency = 12), 1, verbose = FALSE)$fitted,
    "In 1 case"
  )
  expect_equal(as.numeric(two[, 1]), as.numeric(one))
  expect_true(all(is.na(two[seq(1, 120, 12), 2])))
  # single series with a reference period not starting at the series start
  v <- prcp_ts
  v[cycle(v) == 4] <- 0 # all Aprils zero: early exit for April
  expect_warning(full <- spi(v, 1, verbose = FALSE)$fitted, "In 1 case")
  expect_warning(
    ref <- spi(v, 1, ref.start = c(1981, 3), verbose = FALSE)$fitted,
    "In 1 case"
  )
  expect_equal(sum(is.na(ref[cycle(v) == 2])), 0)
})

test_that("params carry pze and reproduce the reference-period fit (H4)", {
  pz <- prcp_ts
  pz[cycle(pz) == 7 & time(pz) < 1990] <- 0
  for (d in c("Gamma", "PearsonIII")) {
    cal <- spi(window(pz, end = c(1995, 12)), 1,
      distribution = d, verbose = FALSE
    )
    expect_equal(dimnames(cal$coefficients)[[1]][dim(cal$coefficients)[1]], "pze")
    expect_equal(unname(cal$coefficients["pze", 1, 7]), 10 / 16)
    a <- spi(pz, 1, distribution = d, ref.end = c(1995, 12), verbose = FALSE)
    b <- spi(pz, 1, distribution = d, params = cal$coefficients, verbose = FALSE)
    expect_equal(as.numeric(b$fitted), as.numeric(a$fitted))
    # legacy arrays without the pze row are accepted with a warning
    legacy <- cal$coefficients[-dim(cal$coefficients)[1], , , drop = FALSE]
    expect_warning(
      spi(pz, 1, distribution = d, params = legacy, verbose = FALSE),
      "no row for the probability of zero"
    )
  }
})

test_that("plot colours follow the sign of the values (O1, #72)", {
  s <- spei(bal_ts, 1, verbose = FALSE)
  fills <- function(obj) {
    g <- ggplot2::ggplot_build(plot(obj))
    f <- unique(unlist(lapply(g$data, function(d) d$fill)))
    sort(f[!is.na(f) & f != "white"])
  }
  neg <- s
  neg$fitted[] <- -abs(s$fitted) - 0.1
  pos <- s
  pos$fitted[] <- abs(s$fitted) + 0.1
  expect_equal(fills(neg), "tomato")
  expect_equal(fills(pos), "cyan3")
})

# Audit batch 2 regression tests

test_that("3-d array with dimnames works (M2)", {
  p <- as.numeric(wichita$PRCP)
  a <- array(p, c(length(p), 2, 2), dimnames = list(NULL, c("a", "b"), c("c", "d")))
  r <- spi(a, 3, verbose = FALSE)
  expect_equal(dim(r$fitted), dim(a))
  expect_equal(dimnames(r$fitted), dimnames(a))
  expect_equal(r$fitted[, 2, 2], as.numeric(spi(p, 3, verbose = FALSE)$fitted))
})

test_that("Non-integer frequency gives an error (M3)", {
  p <- ts(rep(as.numeric(wichita$PRCP), 10)[1:3000], frequency = 365.25)
  expect_error(
    spi(p, 1, verbose = FALSE),
    "The frequency of `data` must be an integer number of periods per year"
  )
})

test_that("Negative data with Gamma or PearsonIII give a warning (M5)", {
  for (d in c("Gamma", "PearsonIII")) {
    expect_warning(
      spei(x_vec, 1, distribution = d, verbose = FALSE),
      "`data` contains negative values"
    )
  }
  expect_no_warning(spi(as.numeric(wichita$PRCP), 1,
    distribution = "Gamma", verbose = FALSE
  ))
})

test_that("parglo.maxlik reports failure when no valid fit is found (O5)", {
  expect_equal(parglo.maxlik(c(1, 1, 1, 1.0001, 50), c(0, 1e-9, 0.9))$conv, 1)
  ok <- parglo.maxlik(x_vec, lmom::pelglo(lmom::samlmu(x_vec)))
  expect_equal(ok$conv, 0)
})

# Audit batch 3 regression tests

test_that("Argument checks give clear errors (L1)", {
  p <- as.numeric(wichita$PRCP)
  expect_error(
    spi(p, 3, kernel = list("rectangular", 0), verbose = FALSE),
    "Argument `kernel` must be a list with components `type` and `shift`."
  )
  expect_equal(
    spi(p, 3, kernel = list(type = "rectangular", shift = 0L), verbose = FALSE)$fitted,
    spi(p, 3, verbose = FALSE)$fitted
  )
  expect_error(
    spi(p, 3, na.rm = NA, verbose = FALSE),
    "Argument `na.rm` must be set to either TRUE or FALSE."
  )
  expect_error(
    spi(p, 1, params = matrix(1, 3, 12), verbose = FALSE),
    "Argument `params` must be a 3-d array"
  )
})

test_that("`scale` must be a positive integer not longer than the series (L2)", {
  p <- as.numeric(wichita$PRCP)
  for (sc in list(2.5, 0, -1)) {
    expect_error(
      spi(p, sc, verbose = FALSE),
      "Argument `scale` must be a positive integer."
    )
  }
  expect_error(
    spi(p[1:24], 30, verbose = FALSE),
    "Argument `scale` can not be larger than the length of the series."
  )
})

test_that("Periods that cannot be fitted give a warning (L3)", {
  q <- as.numeric(wichita$PRCP)
  q[seq(1, length(q), 12)] <- 0
  q[c(1, 13, 25)] <- 5
  expect_warning(
    r <- spi(q, 1, verbose = FALSE),
    "In 1 case\\(s\\).*fewer than 4 non-zero values"
  )
  expect_true(all(is.na(r$fitted[seq(1, length(q), 12)])))
  expect_false(anyNA(r$fitted[-seq(1, length(q), 12)]))
})

test_that("Options are reported with message() and name the right index (L6, O8)", {
  p <- as.numeric(wichita$PRCP)
  expect_message(spi(p, 1), "Standardized Precipitation Index (SPI)", fixed = TRUE)
  expect_message(spei(x_vec, 1), "(SPEI)", fixed = TRUE)
  expect_silent(suppressMessages(spi(p, 1)))
  expect_equal(
    grepl("(SPI)", spi(p, 1, verbose = FALSE)$info, fixed = TRUE),
    TRUE
  )
})

# Audit batch 4 regression tests

test_that("summary() prints the fitted values once (L4)", {
  r <- spei(x_mat, 6, verbose = FALSE)
  o <- capture.output(summary(r))
  expect_equal(sum(grepl("^Fitted:", o)), 1)
})

test_that("plot() takes the index name from the object (O2)", {
  p <- ts(as.numeric(wichita$PRCP), start = c(1980, 1), frequency = 12)
  f <- spi
  expect_equal(plot(f(p, 12, verbose = FALSE))$labels$y, "SPI (z-values)")
  expect_equal(
    plot(do.call(spi, list(p, 12, verbose = FALSE)))$labels$y,
    "SPI (z-values)"
  )
  expect_equal(plot(spei(x_tsvec, 12, verbose = FALSE))$labels$y, "SPEI (z-values)")
  expect_equal(spi(p, 1, verbose = FALSE)$index, "SPI")
})

test_that("Reference period shading covers the reference bars (O3, L5)", {
  p <- ts(as.numeric(wichita$PRCP), start = c(1980, 1), frequency = 12)
  r <- spi(p, 12, ref.start = c(1990, 1), ref.end = c(1999, 12), verbose = FALSE)
  rect <- ggplot2::ggplot_build(plot(r))$data[[1]]
  expect_equal(rect$xmin, 1990 - 0.5 / 12)
  expect_equal(rect$xmax, 1999 + 11.5 / 12)
  # quarterly data
  q <- ts(as.numeric(wichita$PRCP)[1:360], start = c(1980, 1), frequency = 4)
  rq <- spi(q, 1, ref.start = c(1990, 2), verbose = FALSE)
  rect <- ggplot2::ggplot_build(plot(rq))$data[[1]]
  expect_equal(rect$xmin, 1990 + 0.5 / 4)
  expect_equal(rect$xmax, end(q)[1] + (end(q)[2] - 0.5) / 4)
})

test_that("plot() draws without warnings (O4)", {
  g <- plot(spei(x_tsvec, 12, verbose = FALSE))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_warning(print(g))
})

test_that("plot() accepts a title and extends the y breaks (O16)", {
  r <- spei(x_tsvec, 1, verbose = FALSE)
  expect_equal(plot(r, main = "Wichita")$labels$title, "Wichita")
  r$fitted[100] <- 3.2
  b <- ggplot2::ggplot_build(plot(r))$layout$panel_params[[1]]$y$breaks
  expect_equal(max(b, na.rm = TRUE), 3)
})
