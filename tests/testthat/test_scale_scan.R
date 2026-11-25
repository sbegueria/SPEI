context("scale_scan")

data(wichita)
pet <- thornthwaite(wichita$TMED, 37.6475, verbose = FALSE)
bal <- ts(wichita$PRCP - pet, start = c(1980, 1), frequency = 12)
prcp <- ts(wichita$PRCP, start = c(1980, 1), frequency = 12)

# Synthetic annual series: water balance of June-August plus some noise, so
# the best window is August at a scale of 3 months
set.seed(123)
sel <- cycle(bal) %in% 6:8
jja <- as.numeric(tapply(bal[sel], floor(time(bal))[sel], sum))
y <- ts(jja + rnorm(length(jja), sd = 0.3 * sd(jja)),
  start = 1980, frequency = 1
)

sc <- scale_scan(bal, y, scales = 1:6)

# Output structure

test_that("Output structure", {
  expect_s3_class(sc, "scale_scan")
  for (m in c("cor", "p", "p.adj", "n")) {
    expect_equal(dim(sc[[m]]), c(12, 6))
    expect_equal(
      dimnames(sc[[m]]),
      list(cycle = as.character(1:12), scale = as.character(1:6))
    )
  }
  expect_equal(dim(sc$index), c(length(bal), 6))
  expect_equal(frequency(sc$index), 12)
  expect_equal(colnames(sc$index), as.character(1:6))
  expect_equal(sc$best$sign, c("positive", "negative"))
  expect_equal(sc$index.name, "SPEI")
})

test_that("The best window of the synthetic series is found", {
  b <- sc$best[sc$best$sign == "positive", ]
  expect_equal(b$cycle, 8)
  expect_equal(b$scale, 3)
  expect_gt(b$r, 0.8)
  expect_lte(b$p.adj, 0.05)
  expect_equal(b$r, max(sc$cor, na.rm = TRUE))
})

test_that("Correlations, p-values and n are computed per cell", {
  for (cc in c(1, 8, 11)) {
    for (s in c(1, 3, 6)) {
      w <- scan_window(sc, cycle = cc, scale = s)
      ok <- !is.na(w) & !is.na(y)
      ct <- cor.test(as.numeric(w)[ok], as.numeric(y)[ok])
      expect_equal(sc$cor[cc, s], unname(ct$estimate))
      expect_equal(sc$p[cc, s], ct$p.value)
      expect_equal(sc$n[cc, s], sum(ok))
    }
  }
  # wichita ends in October 2011: one year less in November and December
  expect_equal(sc$n[11, 1], 31)
  expect_equal(sc$n[10, 1], 32)
  # the first values at scales > 1 are NA
  expect_equal(sc$n[1, 6], 31)
})

test_that("Adjustment of p-values across all cells", {
  expect_equal(
    as.numeric(sc$p.adj),
    p.adjust(as.numeric(sc$p), method = "holm")
  )
  sc_n <- scale_scan(bal, y, scales = 1:3, p.adjust = "none")
  expect_equal(sc_n$p.adj, sc_n$p)
  sc_b <- scale_scan(bal, y, scales = 1:3, p.adjust = "bonferroni")
  expect_equal(
    as.numeric(sc_b$p.adj),
    pmin(1, as.numeric(sc_b$p) * sum(!is.na(sc_b$p)))
  )
})

test_that("No significant window gives NA", {
  set.seed(1)
  noise <- ts(rnorm(32), start = 1980, frequency = 1)
  sc0 <- scale_scan(bal, noise, scales = 1:3, alpha = 1e-6)
  expect_true(all(is.na(sc0$best$cycle)))
  expect_error(scan_window(sc0), "There is no significant positive window")
  expect_s3_class(plot(sc0), "ggplot")
})

# Input options

test_that("Precomputed index gives the same result", {
  idx <- sapply(1:3, function(s) spei(bal, s, verbose = FALSE)$fitted)
  idx <- ts(idx, start = c(1980, 1), frequency = 12)
  sc_p <- scale_scan(idx, y, index = "precomputed")
  expect_equal(sc_p$cor, sc$cor[, 1:3])
  expect_equal(sc_p$index.name, "Index")
  # scales taken from the column names
  colnames(idx) <- c("SPEI-1", "SPEI-3", "SPEI-6")
  expect_equal(
    colnames(scale_scan(idx, y, index = "precomputed")$cor),
    c("1", "3", "6")
  )
  colnames(idx) <- c("a", "b", "c")
  expect_equal(
    colnames(scale_scan(idx, y, index = "precomputed")$cor),
    c("1", "2", "3")
  )
  expect_equal(
    colnames(scale_scan(idx, y, scales = c(2, 4, 8),
      index = "precomputed")$cor),
    c("2", "4", "8")
  )
})

test_that("Options are passed to spei() / spi()", {
  sc_g <- scale_scan(prcp, y, scales = 1:2, index = "spi")
  expect_equal(sc_g$index.name, "SPI")
  expect_equal(
    as.numeric(sc_g$index[, 2]),
    as.numeric(spi(prcp, 2, verbose = FALSE)$fitted)
  )
  sc_p3 <- scale_scan(prcp, y,
    scales = 2, index = "spi",
    distribution = "PearsonIII"
  )
  expect_equal(
    as.numeric(sc_p3$index),
    as.numeric(spi(prcp, 2, distribution = "PearsonIII",
      verbose = FALSE)$fitted)
  )
  expect_silent(scale_scan(bal, y, scales = 1))
})

test_that("Annual series as a data frame", {
  df <- data.frame(year = 1980:2011, value = as.numeric(y))
  expect_equal(scale_scan(bal, df, scales = 1:6)$cor, sc$cor)
  # unordered and with missing years
  df2 <- df[sample(nrow(df)), ][-(1:3), ]
  sc2 <- scale_scan(bal, df2, scales = 1:2)
  yy <- y
  yy[match(df[-match(df2$year, df$year), "year"], 1980:2011)] <- NA
  expect_equal(sc2$cor, scale_scan(bal, yy, scales = 1:2)$cor)
  expect_error(
    scale_scan(bal, rbind(df, df[1, ]), scales = 1),
    "The years in `y` can not be repeated."
  )
})

test_that("Detrending of the annual series", {
  yt <- y + 50 * (seq_along(y) - 16)
  sc_d <- scale_scan(bal, yt, scales = 1:3, detrend = TRUE)
  y_d <- ts(residuals(lm(as.numeric(yt) ~ seq_along(yt))),
    start = 1980, frequency = 1
  )
  expect_equal(sc_d$cor, scale_scan(bal, y_d, scales = 1:3)$cor)
  expect_equal(as.numeric(sc_d$y), as.numeric(y_d))
})

test_that("Series not starting in January", {
  b6 <- window(bal, start = c(1980, 6))
  sc6 <- scale_scan(b6, y, scales = 1:3)
  expect_equal(sc6$n[6, 1], 32)
  expect_equal(sc6$n[5, 1], 31)
  expect_equal(sc6$cor[8, 3], sc$cor[8, 3])
})

test_that("Infinite values and short series", {
  idx <- ts(cbind(rnorm(120), rnorm(120)), start = c(2001, 1), frequency = 12)
  idx[1:30, 1] <- Inf
  idx[31:60, 1] <- -Inf
  yy <- ts(rnorm(10), start = 2001)
  sc_i <- scale_scan(idx, yy, index = "precomputed", min.n = 3)
  expect_equal(as.numeric(sc_i$n[, 1]), rep(5, 12))
  expect_equal(as.numeric(sc_i$n[, 2]), rep(10, 12))
  expect_false(any(is.infinite(sc_i$index)))
  sc_m <- scale_scan(idx, yy, index = "precomputed", min.n = 6)
  expect_true(all(is.na(sc_m$cor[, 1])))
  expect_true(all(is.na(sc_m$p.adj[, 1])))
  expect_false(anyNA(sc_m$cor[, 2]))
  # constant series
  idx[, 1] <- 1
  expect_true(all(is.na(
    scale_scan(idx, yy, index = "precomputed", min.n = 3)$cor[, 1]
  )))
})

test_that("Weekly data", {
  set.seed(42)
  wk <- ts(matrix(rnorm(52 * 40 * 2), ncol = 2),
    start = c(1981, 1), frequency = 52
  )
  colnames(wk) <- c("4", "8")
  target <- wk[cycle(wk) == 20, 2]
  yy <- ts(target + rnorm(40, sd = 0.3), start = 1981)
  sc_w <- scale_scan(wk, yy, index = "precomputed", method = "spearman")
  expect_equal(dim(sc_w$cor), c(52, 2))
  b <- sc_w$best[1, ]
  expect_equal(c(b$cycle, b$scale), c(20, 8))
  w <- scan_window(sc_w)
  expect_equal(as.numeric(w), as.numeric(target))
  expect_equal(time(w), time(yy))
  expect_equal(c(attr(w, "cycle"), attr(w, "scale")), c(20, 8))
  expect_s3_class(plot(sc_w), "ggplot")
})

# Methods

test_that("print and plot", {
  expect_output(print(sc), "Best windows")
  expect_output(summary(sc), "positive")
  for (s in c("mark", "mask", "none")) {
    g <- plot(sc, sig = s, main = "test")
    expect_s3_class(g, "ggplot")
  }
  expect_error(plot(sc, sig = "x"), "Argument `sig` must be one of")
})

test_that("scan_window", {
  w <- scan_window(sc)
  expect_equal(start(w), c(1980, 1))
  expect_equal(frequency(w), 1)
  expect_equal(
    as.numeric(w),
    as.numeric(spei(bal, 3, verbose = FALSE)$fitted)[cycle(bal) == 8]
  )
  expect_error(scan_window(sc, cycle = 8), "must be given together")
  expect_error(scan_window(sc, 13, 3), "Argument `cycle` must be")
  expect_error(scan_window(sc, 8, 12), "Argument `scale` must be one of")
  expect_error(scan_window(sc, which = "x"), "Argument `which` must be")
  expect_error(scan_window(1), "must be a `scale_scan` object")
})

# Argument checks

test_that("Argument checks", {
  expect_error(
    scale_scan(as.numeric(bal), y),
    "Argument `data` must be a numeric time series"
  )
  expect_error(
    scale_scan(cbind(bal, bal), y),
    "must be a univariate time series when `index` is `spei`"
  )
  expect_error(scale_scan(bal, y, index = "x"), "Argument `index` must be")
  expect_error(
    scale_scan(bal, y, scales = c(0, 1)),
    "Argument `scales` must be a vector of positive integers."
  )
  expect_error(
    scale_scan(bal, y, scales = c(1, 1)),
    "can not have repeated values"
  )
  expect_error(
    scale_scan(cbind(bal, bal), y, scales = 1:3, index = "precomputed"),
    "one value per column of `data`"
  )
  expect_error(scale_scan(bal, y, method = "x"), "Argument `method`")
  expect_error(scale_scan(bal, y, p.adjust = "x"), "Argument `p.adjust`")
  expect_error(scale_scan(bal, y, alpha = 2), "Argument `alpha`")
  expect_error(scale_scan(bal, y, min.n = 2), "Argument `min.n`")
  expect_error(scale_scan(bal, y, detrend = NA), "Argument `detrend`")
  expect_error(
    scale_scan(bal, as.numeric(y)),
    "Argument `y` must be a time series with frequency 1"
  )
  expect_error(
    scale_scan(bal, bal),
    "Argument `y` must be a time series with frequency 1"
  )
})
