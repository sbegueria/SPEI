context("kern")


test_that("example w/ defaults", {
  k12Out <- readRDS("data/k12_Out.rds")
  expect_equal(k12Out, kern(12))
})


test_that("example w/ defaults", {
  k12GaussOut <- readRDS("data/k12_gauss_Out.rds")
  expect_equal(k12GaussOut, kern(12, "gaussian"))
})


test_that("plot works without errors/warnings", {
  pdf(NULL) # Stops Rplots.pdf from being created
  expect_error(kern.plot(12), NA)
})


test_that("type error", {
  expect_error(
    kern(12, "gaussiann"),
    "type must be one of: rectangular, triangular, circular, gaussian"
  )
})

test_that("shift lower than scale", {
  expect_error(
    kern(12, "gaussian", shift = 13),
    'Parameter "shift" must be lower than "scale"'
  )
})

test_that("negative shift", {
  expect_error(
    kern(12, "gaussian", shift = -13),
    'Parameter "shift" cannot have a negative value'
  )
})

test_that("kern() with scale 1 returns 1 for any type (O9)", {
  for (k in c("rectangular", "triangular", "circular", "gaussian")) {
    expect_equal(kern(1, k), 1)
  }
})

test_that("kernels have length `scale` and sum to 1 (O17)", {
  for (k in c("rectangular", "triangular", "circular", "gaussian")) {
    for (s in c(2, 6, 12, 24)) {
      w <- kern(s, k)
      expect_length(w, s)
      expect_equal(sum(w), 1)
      expect_true(all(w > 0))
    }
  }
})

test_that("kernel shapes (O17)", {
  expect_equal(kern(4), rep(0.25, 4))
  expect_equal(kern(4, "triangular"), (4:1) / 10)
  expect_equal(kern(3, "circular"), c(9, 6, 1) / 16)
  expect_true(all(diff(kern(12, "gaussian")) < 0))
})

test_that("shift moves the kernel peak (O17)", {
  for (k in c("triangular", "circular", "gaussian")) {
    for (h in 1:3) {
      w <- kern(12, k, shift = h)
      expect_length(w, 12)
      expect_equal(sum(w), 1)
      expect_equal(which.max(w), h + 1)
      # symmetric around the peak
      expect_equal(w[(h + 1) - (1:h)], w[(h + 1) + (1:h)])
    }
  }
})
