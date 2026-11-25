context("drought_events")

# Synthetic series with known events
v <- c(-1.5, -2, 0, -1, 0.5, NA, -1.2, -1.1, NA, -3, 1, 2, 1.5, -1)

test_that("Dry events of a vector", {
  ev <- drought_events(v)
  expect_s3_class(ev, "data.frame")
  expect_equal(
    names(ev),
    c(
      "start", "end", "duration", "severity", "intensity", "peak",
      "peak.time"
    )
  )
  # Events at the start and at the end of the series, single-step events,
  # a value equal to the threshold, and NA breaking events
  expect_equal(ev$start, c(1L, 4L, 7L, 10L, 14L))
  expect_equal(ev$end, c(2L, 4L, 8L, 10L, 14L))
  expect_equal(ev$duration, c(2, 1, 2, 1, 1))
  expect_equal(ev$severity, c(-3.5, -1, -2.3, -3, -1))
  expect_equal(ev$intensity, ev$severity / ev$duration)
  expect_equal(ev$peak, c(-2, -1, -1.2, -3, -1))
  expect_equal(ev$peak.time, c(2L, 4L, 7L, 10L, 14L))
})

test_that("Threshold", {
  ev <- drought_events(v, threshold = -1.5)
  expect_equal(ev$start, c(1L, 10L))
  expect_equal(ev$end, c(2L, 10L))
  ev <- drought_events(v, threshold = 0)
  expect_equal(ev$start, c(1L, 7L, 10L, 14L))
  expect_equal(ev$end, c(4L, 8L, 10L, 14L))
  expect_equal(ev$severity[1], -4.5)
})

test_that("Wet events", {
  ev <- drought_events(v, type = "wet")
  expect_equal(ev$start, 11L)
  expect_equal(ev$end, 13L)
  expect_equal(ev$severity, 4.5)
  expect_equal(ev$peak, 2)
  expect_equal(ev$peak.time, 12L)
  # mirrored default threshold, and a user threshold
  expect_equal(
    drought_events(v, type = "wet"),
    drought_events(v, type = "wet", threshold = 1)
  )
  expect_equal(drought_events(v, type = "wet", threshold = 1.6)$start, 12L)
})

test_that("Minimum duration", {
  ev <- drought_events(v, min.duration = 2)
  expect_equal(ev$start, c(1L, 7L))
  expect_equal(nrow(drought_events(v, min.duration = 3)), 0)
})

test_that("Pooling", {
  # 1-2 and 4 are separated by one step; 7-8 is separated from 4 by an NA
  ev <- drought_events(v, pool = 1)
  expect_equal(ev$start, c(1L, 7L, 10L, 14L))
  expect_equal(ev$end, c(4L, 8L, 10L, 14L))
  expect_equal(ev$duration[1], 4)
  expect_equal(ev$severity[1], -4.5)
  expect_equal(ev$peak.time[1], 2L)
  # min.duration applies after pooling
  ev <- drought_events(v, pool = 1, min.duration = 3)
  expect_equal(ev$start, 1L)
  # longer gaps
  w <- c(-2, 0, 0, -2, 0, 0, 0, -2)
  expect_equal(nrow(drought_events(w, pool = 1)), 3)
  expect_equal(drought_events(w, pool = 2)$end, c(4L, 8L))
  expect_equal(drought_events(w, pool = 3)$end, 8L)
})

test_that("No events and missing values", {
  ev <- drought_events(c(0, 1, NA))
  expect_equal(nrow(ev), 0)
  expect_equal(names(ev), names(drought_events(v)))
  expect_equal(nrow(drought_events(rep(NA_real_, 5))), 0)
})

test_that("Monthly, weekly and annual time series", {
  x <- ts(v, start = c(1990, 11), frequency = 12)
  ev <- drought_events(x)
  expect_equal(
    ev$start,
    c("1990-11", "1991-02", "1991-05", "1991-08", "1991-12")
  )
  expect_equal(ev$end[1], "1990-12")
  expect_equal(ev$peak.time[1], "1990-12")
  expect_equal(ev$severity, drought_events(v)$severity)
  x <- ts(v, start = c(1990, 51), frequency = 52)
  ev <- drought_events(x)
  expect_equal(ev$start[1:2], c("1990-P51", "1991-P02"))
  x <- ts(v, start = 1990, frequency = 1)
  expect_equal(drought_events(x)$start[1:2], c("1990", "1993"))
  # non-integer frequency: decimal times
  x <- ts(v, start = 1990, frequency = 0.5)
  expect_equal(drought_events(x)$start[1:2], c(1990, 1996))
})

test_that("Several series", {
  m <- cbind(a = v, b = -v)
  ev <- drought_events(m)
  expect_equal(names(ev)[1], "site")
  expect_equal(ev$site, c(rep("a", 5), "b"))
  expect_equal(ev[ev$site == "a", -1], drought_events(v))
  expect_equal(ev$start[ev$site == "b"], 11L)
  expect_equal(ev$end[ev$site == "b"], 13L)
  expect_equal(drought_events(unname(m))$site, c(rep(1L, 5), 2L))
  # a multivariate ts
  mts <- ts(m, start = c(2000, 1), frequency = 12)
  ev <- drought_events(mts)
  expect_equal(ev$start[1], "2000-01")
  expect_equal(ev$start[6], "2000-11")
  # a one-column matrix: no site column
  expect_false("site" %in% names(drought_events(m[, 1, drop = FALSE])))
})

test_that("spei objects", {
  data(wichita)
  pet <- thornthwaite(wichita$TMED, 37.6475, verbose = FALSE)
  bal <- ts(wichita$PRCP - pet, end = c(2011, 10), frequency = 12)
  s12 <- spei(bal, 12, verbose = FALSE)
  ev <- drought_events(s12)
  expect_identical(ev, drought_events(s12$fitted))
  expect_equal(ev$start[1], "1980-12")
  expect_equal(ev$end[nrow(ev)], "2011-10")
  expect_true(all(ev$severity < 0))
  expect_true(all(ev$peak <= -1))
  # run theory with threshold 0, as in issue #41
  ev0 <- drought_events(s12, threshold = 0)
  expect_equal(ev0$end[1], "1982-05")
  expect_equal(ev0$duration[1], 18)
  # several series in a spei object
  s <- spei(cbind(a = bal, b = bal), 12, verbose = FALSE)
  ev2 <- drought_events(s)
  expect_equal(ev2[ev2$site == "b", -1], ev, check.attributes = FALSE)
})

test_that("Argument checks", {
  expect_error(drought_events(array(0, c(5, 2, 2))), "3-d array")
  expect_error(drought_events("a"), "`x`")
  expect_error(drought_events(v, type = "wetter"), "`type`")
  expect_error(drought_events(v, threshold = c(0, 1)), "`threshold`")
  expect_error(drought_events(v, min.duration = 0), "`min.duration`")
  expect_error(drought_events(v, pool = -1), "`pool`")
  expect_error(drought_events(v, pool = 1.5), "`pool`")
})
