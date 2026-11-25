# Flexible ("auto") standardization: choice of the distribution for each
# series and position in the year.
#
# Adapted from FlexDroughtIndex (https://github.com/lcsc/FlexDroughtIndex,
# S.M. Vicente-Serrano, F. Reig, S. Beguería; GPL >= 2 use agreed by the
# authors), the code accompanying Vicente-Serrano et al. (2025), Int. J.
# Climatol. 45(10): e8906, doi:10.1002/joc.8906. The six candidate
# distributions, their L-moment fit with package `lmom` and the AIC on the
# CDF (`auto_aic()`) follow that code. The default selection criterion here
# is the Shapiro-Wilk W of the standardized values in the reference period.

# Candidate distributions, in the order used for the `dist` code stored in
# the coefficients (1 = gev, ..., 6 = wei). Ties go to the first one.
auto_dists <- c("gev", "glo", "gpa", "ln3", "pe3", "wei")

# Fit candidate `code` (1-6) to L-moments `lm3` = c(l1, l2, t3).
# Returns the three parameters (location, scale, shape) or NULL.
auto_pel <- function(code, lm3) {
  p <- tryCatch(
    switch(code,
      lmom::pelgev(lm3), lmom::pelglo(lm3), lmom::pelgpa(lm3),
      lmom::pelln3(lm3), lmom::pelpe3(lm3), lmom::pelwei(lm3)
    ),
    error = function(e) NULL
  )
  if (is.null(p) || length(p) != 3 || anyNA(p)) NULL else unname(p)
}

# CDF of candidate `code` with parameters `para` at `x`.
auto_cdf <- function(x, code, para) {
  switch(code,
    lmom::cdfgev(x, para), lmom::cdfglo(x, para), lmom::cdfgpa(x, para),
    lmom::cdfln3(x, para), lmom::cdfpe3(x, para), lmom::cdfwei(x, para)
  )
}

# Standardized values (z) of `x` for candidate `code`, with an optional
# probability of zero `pze` (NULL for no zero handling). With
# `upper = FALSE` (SPI) the data are bounded below by zero: p = pze +
# (1 - pze) F(x). With `upper = TRUE` (SEDI) they are bounded above by zero:
# p = (1 - pze) F(x) for x < 0, and p = 1 - pze for x >= 0 (the mirror of
# the SPI convention, where zeros get p = pze).
auto_z <- function(x, code, para, pze = NULL, upper = FALSE) {
  p <- auto_cdf(x, code, para)
  if (!is.null(pze)) {
    if (upper) {
      p <- (1 - pze) * p
      p[x >= 0] <- 1 - pze
    } else {
      p <- pze + (1 - pze) * p
    }
  }
  qnorm(p)
}

# Pseudo-AIC on the CDF of FlexDroughtIndex (selec_distrib_AIC()): the
# empirical and the theoretical probabilities of the intervals between
# consecutive sorted values. Lower is better; k = 3 for all candidates.
auto_aic <- function(x, code, para) {
  xs <- sort(x)
  eps <- 1e-10
  p_emp <- pmax(diff(stats::ecdf(x)(xs)), eps)
  p_the <- pmax(diff(auto_cdf(xs, code, para)), eps)
  2 * 3 - 2 * length(xs) * sum(p_emp * log(p_the))
}

# Choose the candidate for one series and position in the year.
# x.fit: values used for fitting (positive values only when pze is used);
# x.all: all the values in the reference period (zeros included), on which
# the normality of the standardized values is assessed;
# lm3: L-moments of x.fit, c(l1, l2, t3); pze: probability of zero or NULL;
# upper: TRUE if the data are bounded above by zero (SEDI).
# Returns c(code, location, scale, shape), or NULL if no candidate fits.
#
# Criterion "shapiro" (default): fewest non-finite standardized values in
# the reference period, then largest Shapiro-Wilk W of the finite ones.
# Criterion "aic": smallest AIC on the CDF, as in FlexDroughtIndex.
auto_select <- function(x.fit, x.all, lm3, pze = NULL, upper = FALSE,
                        criterion = "shapiro") {
  best <- NULL
  best_score <- c(Inf, Inf)
  for (code in seq_along(auto_dists)) {
    para <- auto_pel(code, lm3)
    if (is.null(para)) next
    if (criterion == "aic") {
      score <- c(0, tryCatch(auto_aic(x.fit, code, para),
        error = function(e) NA
      ))
    } else {
      z <- auto_z(x.all, code, para, pze, upper)
      zf <- z[is.finite(z)]
      if (length(zf) > 5000) zf <- zf[seq_len(5000)]
      w <- if (length(zf) >= 3) {
        tryCatch(stats::shapiro.test(zf)$statistic,
          error = function(e) NA
        )
      } else {
        NA
      }
      score <- c(sum(!is.finite(z)), -w)
    }
    if (is.na(score[2])) next
    if (score[1] < best_score[1] ||
      (score[1] == best_score[1] && score[2] < best_score[2])) {
      best <- c(code, para)
      best_score <- score
    }
  }
  best
}
