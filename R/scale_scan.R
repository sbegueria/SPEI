#' @name Scale-scan
#' @title Find the time scale and the time of the year at which a drought
#' index best correlates with an annual series.
#' @aliases scale_scan scale_extract
#' @description
#' \code{scale_scan} correlates an annual series (for example tree-ring
#' widths, crop yields or reservoir levels) with the values of a drought
#' index (SPEI or SPI) at every position of the year (month, week, etc.) and
#' at every time scale, optionally also in previous years, and finds the
#' combination of position and time scale (the 'window') with the strongest
#' positive and negative correlation. \code{scale_extract} extracts the
#' annual series of the index for one window.
#' @usage
#' scale_scan(
#'  data,
#'  y,
#'  scales = NULL,
#'  index = 'spei',
#'  lag = 0,
#'  method = 'pearson',
#'  p.adjust = 'holm',
#'  alpha = 0.05,
#'  min.n = 10,
#'  detrend = FALSE,
#'  ...)
#'
#' scale_extract(x, cycle = NULL, scale = NULL, lag = 0, which = 'positive')
#'
#' \method{print}{scale_scan}(x, ...)
#' \method{summary}{scale_scan}(object, ...)
#' \method{plot}{scale_scan}(x, sig = 'mark', main = NULL, ...)
#' @param data a univariate or multivariate time series (\code{ts}) object.
#' Its frequency (12 for monthly data, 52 for weekly data, etc.) and the
#' function \code{\link{cycle}} determine the position of each value in the
#' year. If \code{index} is \code{'spei'} or \code{'spi'}, a univariate
#' series of the climatic water balance (for the SPEI) or of precipitation
#' (for the SPI). If \code{index} is \code{'precomputed'}, a series of index
#' values with one column per time scale.
#' @param y the annual series. Either a time series with frequency 1, with
#' one or more columns; or a data frame or a numeric matrix whose first
#' column has the years and whose other columns have the values. Years can
#' be repeated. All the values of the same year (several columns, several
#' rows, or both) are averaged into one value. See 'Several annual series'.
#' @param scales a vector of positive integers with the time scales. If
#' \code{index} is \code{'spei'} or \code{'spi'}, the index is computed at
#' each of these scales; defaults to \code{1:24}. If \code{index} is
#' \code{'precomputed'}, the time scale of each column of \code{data};
#' defaults to the column names of \code{data} if they are numbers (with
#' any non-numeric prefix, such as 'SPEI-' or 'X', removed), and to
#' \code{1:ncol(data)} otherwise.
#' @param index one of \code{'spei'} (default), \code{'spi'} or
#' \code{'precomputed'}. See \code{data}.
#' @param lag a vector of non-negative integers: the years before the year
#' of the annual series in which the index is also scanned. Defaults to 0
#' (the same year only). For example, \code{lag = 0:1} also correlates the
#' annual series of year Y with the index of year Y-1. In
#' \code{scale_extract}, a single value.
#' @param method the correlation coefficient, one of \code{'pearson'}
#' (default), \code{'spearman'} or \code{'kendall'}. Passed to
#' \code{\link[stats]{cor.test}}.
#' @param p.adjust the method used to adjust the p-values for multiple
#' testing, one of \code{\link[stats]{p.adjust.methods}}. Defaults to
#' \code{'holm'}. Use \code{'none'} for no adjustment.
#' @param alpha the significance level. Defaults to 0.05.
#' @param min.n the minimum number of years with values of both the annual
#' series and the index needed to compute a correlation. Cells with fewer
#' years are set to \code{NA}. Defaults to 10.
#' @param detrend logical, remove a linear trend from the annual series
#' (after averaging) before computing the correlations. Defaults to
#' \code{FALSE}.
#' @param ... further options passed to \code{\link{spei}} or
#' \code{\link{spi}} (e.g. \code{distribution}, \code{kernel},
#' \code{ref.start}, \code{ref.end}, \code{na.rm}). \code{verbose} defaults
#' to \code{FALSE}. Not used if \code{index} is \code{'precomputed'}.
#' @param x an object of class \code{scale_scan}.
#' @param cycle the position in the year (1 to the frequency of \code{data})
#' of the window.
#' @param scale the time scale of the window. Must be one of the scales used
#' in the scan.
#' @param which used when \code{cycle} and \code{scale} are not given: the
#' best window to extract, either \code{'positive'} (default) or
#' \code{'negative'}. Its lag is used, and the argument \code{lag} ignored.
#' @param object an object of class \code{scale_scan}.
#' @param sig how to show the significance of the correlations in the plot:
#' \code{'mark'} (default), \code{'mask'} or \code{'none'}. See 'Value'.
#' @param main optional, a character string with the plot title.
#'
#'
#' @details
#' Many annual variables (tree growth, crop yields, river flow, reservoir
#' storage) respond to the water balance of a particular part of the year
#' and integrated over a particular length of time. Because the SPEI and the
#' SPI are multi-scalar, they can be used to find both: the value of the
#' index at time scale \code{s} in month \code{m} summarizes the water balance
#' of the \code{s} months ending in month \code{m} (see the 'Time scales'
#' section in \code{\link{spei}}). \code{scale_scan} correlates the annual
#' series with the index at each position in the year (rows) and each time
#' scale (columns), in the spirit of Vicente-Serrano et al. (2012).
#'
#' Each value of the index is assigned to a year and a position within the
#' year using \code{\link{time}} and \code{\link{cycle}}, so any frequency
#' can be used (monthly, weekly, etc.). For each position and scale, the
#' annual series of year Y is correlated with the value of the index at that
#' position in year Y - \code{lag}, for each value of \code{lag}. Long time
#' scales also reach into the previous year (e.g. a 12-month scale in March
#' covers April of the previous year to March).
#'
#' The rows of the results are the combinations of lag and position in the
#' year, in chronological order (the earliest year first). They are labelled
#' with the month (\code{'Jan'}, \code{'Feb'}, ...), the week (\code{'W1'},
#' \code{'W2'}, ...), the day (\code{'D1'}, ...) or the period (\code{'P1'},
#' ...), followed by \code{'-1'}, \code{'-2'}, ... for the previous years:
#' with \code{lag = 0:1} and monthly data, the rows are \code{'Jan-1'} to
#' \code{'Dec-1'} and then \code{'Jan'} to \code{'Dec'}.
#'
#' Infinite index values are treated as missing. Cells with fewer than
#' \code{min.n} pairs of values, or where one of the two series is constant,
#' get \code{NA}. The p-values come from \code{\link[stats]{cor.test}}, and
#' are adjusted with \code{\link[stats]{p.adjust}} across all the cells of
#' the scan (all positions, scales and lags), since all of them are tested to
#' choose the best window. The default (Holm's method) controls the
#' family-wise error rate as the Bonferroni correction does but is less
#' conservative. Note that the cells are strongly correlated with each other
#' (neighbouring scales and positions share most of their data), so both
#' methods are conservative.
#'
#' The best windows are the cells with the highest (positive) and the lowest
#' (negative) correlation among those with an adjusted p-value not larger
#' than \code{alpha}. If no cell is significant with the given sign, the
#' corresponding row of \code{best} is \code{NA}.
#'
#'
#' @section Preparing the annual series:
#' The annual series is used as given, except for the averaging of values
#' of the same year and the optional removal of a linear trend
#' (\code{detrend = TRUE}). Series with other non-climatic signals (e.g. age
#' trends in tree-ring widths, or technological trends in crop yields)
#' should be detrended or standardized beforehand with the appropriate
#' methods. Trends in the index itself are not removed.
#'
#'
#' @section Several annual series:
#' If \code{y} has several columns, or several rows for the same year, all
#' its values of each year are averaged (ignoring \code{NA}) into a single
#' annual series, which is then correlated with the index. This is the usual
#' way of building a tree-ring chronology from several trees, and it
#' strengthens the common signal. It only makes sense if all the series
#' share the climate in \code{data}, respond to it in a similar way, and are
#' on comparable scales (standardize them first otherwise). Differences
#' between the series are lost. The alternative is to call
#' \code{scale_scan} once for each series, which gives a window for each,
#' at the cost of more tests and noisier estimates. A tree-ring data frame
#' with years as row names (as in the \code{dplR} package) can be passed as
#' \code{ts(rwl, start = as.numeric(rownames(rwl))[1])}.
#'
#'
#' @return
#' \code{scale_scan} returns an object of class \code{scale_scan}, a list
#' with the following components:
#'
#' \itemize{
#'   \item call: the function call.
#'   \item cor, p, p.adj, n: matrices with one row per combination of lag
#'   and position in the year (see 'Details') and one column per time scale,
#'   with the correlation coefficients, the p-values, the adjusted p-values
#'   and the number of years used.
#'   \item rows: a data frame with the \code{lag}, \code{cycle} and
#'   \code{label} of each row of the matrices.
#'   \item best: a data frame with the best positive and negative windows,
#'   with columns \code{sign}, \code{lag}, \code{cycle}, \code{label},
#'   \code{scale}, \code{r}, \code{p}, \code{p.adj} and \code{n}.
#'   \item index: a multivariate time series with the index values, one
#'   column per time scale.
#'   \item y: the annual series used (after averaging and detrending, if
#'   requested), as a time series with frequency 1.
#'   \item y.count: a time series with the number of values averaged in
#'   each year of \code{y}.
#'   \item index.name, lag, method, p.adjust, alpha, min.n, detrend: the
#'   options used.
#' }
#'
#' \code{print} and \code{summary} report the options and the best windows.
#' \code{plot} draws a heatmap of the correlations (a \code{ggplot2}
#' object), with the positions in the year in chronological order and
#' vertical lines between years. With \code{sig = 'mark'} the significant
#' cells are marked with a dot; with \code{sig = 'mask'} the non-significant
#' cells are left blank; with \code{sig = 'none'} all cells are drawn the
#' same way. The best windows are outlined.
#'
#' \code{scale_extract} returns a time series with frequency 1 with the
#' values of the index at the given window, aligned with the annual series:
#' the value for year Y is the index at position \code{cycle} of year
#' Y - \code{lag}. It covers all the years in \code{data} (shifted by
#' \code{lag}), and has attributes \code{cycle}, \code{scale} and
#' \code{lag}.
#'
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J. Lorenzo-Lacruz, J.J. Camarero,
#' J.I. López-Moreno, C. Azorin-Molina, J. Revuelto, E. Morán-Tejeda,
#' A. Sánchez-Lorenzo. 2012. Performance of drought indices for ecological,
#' agricultural, and hydrological applications. \emph{Earth Interactions}
#' \bold{16}: 1-27, DOI: 10.1175/2012EI000434.1.
#'
#'
#' @author Santiago Beguería
#'
#'
#' @seealso
#' \code{\link{spei}} and \code{\link{spi}} for computing the indices.
#' \code{vignette("scale_scan")} for a worked example.
#'
#'
#' @examples
#' # Load data and compute the climatic water balance
#' data(wichita)
#' wichita$PET <- thornthwaite(wichita$TMED, 37.6475, verbose = FALSE)
#' bal <- ts(wichita$PRCP - wichita$PET, start = c(1980, 1), frequency = 12)
#'
#' # A SYNTHETIC annual series, built for illustration only: the water
#' # balance of June to August (i.e., three months ending in August) plus
#' # random noise. The best window should be close to August at a time
#' # scale of three months.
#' set.seed(1)
#' jja <- tapply(
#'   bal[cycle(bal) %in% 6:8],
#'   floor(time(bal))[cycle(bal) %in% 6:8], sum
#' )
#' yield <- ts(as.numeric(jja) + rnorm(length(jja), sd = sd(jja)),
#'   start = 1980, frequency = 1
#' )
#'
#' # Scan the SPEI at time scales 1 to 12 months
#' sc <- scale_scan(bal, yield, scales = 1:12)
#' sc
#' plot(sc)
#' plot(sc, sig = "mask")
#'
#' # Also scan the previous year
#' sc2 <- scale_scan(bal, yield, scales = 1:12, lag = 0:1)
#' plot(sc2)
#'
#' # The annual SPEI series at the best positive window, and at another one
#' best <- scale_extract(sc)
#' plot(as.numeric(best), as.numeric(yield))
#' scale_extract(sc, cycle = 7, scale = 6, lag = 1)
#'
#' # Using already computed index values, one column per time scale
#' idx <- sapply(c(1, 3, 6), function(s) spei(bal, s, verbose = FALSE)$fitted)
#' idx <- ts(idx, start = c(1980, 1), frequency = 12)
#' colnames(idx) <- c("SPEI-1", "SPEI-3", "SPEI-6")
#' scale_scan(idx, yield, index = "precomputed")
#'
#' @export
#'
scale_scan <- function(data, y, scales = NULL, index = "spei", lag = 0,
                       method = "pearson", p.adjust = "holm", alpha = 0.05,
                       min.n = 10, detrend = FALSE, ...) {
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -

  check <- makeAssertCollection()

  if (!is.character(index) || length(index) != 1 ||
    !index %in% c("spei", "spi", "precomputed")) {
    stop("Argument `index` must be one of `spei`, `spi` or `precomputed`.")
  }
  if (!is.ts(data) || !is.numeric(data)) {
    check$push("Argument `data` must be a numeric time series (`ts`) object.")
  } else if (frequency(data) < 1 || frequency(data) %% 1 != 0) {
    check$push("The frequency of `data` must be a positive integer.")
  } else if (index != "precomputed" && NCOL(data) != 1) {
    check$push(paste0(
      "Argument `data` must be a univariate time series when `index` is `",
      index, "`."
    ))
  }
  if (!is.null(scales)) {
    if (!is.numeric(scales) || length(scales) < 1 || anyNA(scales) ||
      any(scales < 1) || any(scales %% 1 != 0)) {
      check$push("Argument `scales` must be a vector of positive integers.")
    } else if (anyDuplicated(scales)) {
      check$push("Argument `scales` can not have repeated values.")
    } else if (index == "precomputed" && is.ts(data) &&
      length(scales) != NCOL(data)) {
      check$push(paste0(
        "Argument `scales` must have one value per column of `data` when ",
        "`index` is `precomputed`."
      ))
    }
  }
  if (!is.numeric(lag) || length(lag) < 1 || anyNA(lag) || any(lag < 0) ||
    any(lag %% 1 != 0) || anyDuplicated(lag)) {
    check$push(paste0(
      "Argument `lag` must be a vector of non-negative integers without ",
      "repeated values."
    ))
  }
  if (!is.character(method) || length(method) != 1 ||
    !method %in% c("pearson", "spearman", "kendall")) {
    check$push(paste0(
      "Argument `method` must be one of `pearson`, `spearman` or ",
      "`kendall`."
    ))
  }
  if (!is.character(p.adjust) || length(p.adjust) != 1 ||
    !p.adjust %in% stats::p.adjust.methods) {
    check$push(paste0(
      "Argument `p.adjust` must be one of ",
      paste0("`", stats::p.adjust.methods, "`", collapse = ", "), "."
    ))
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
    alpha <= 0 || alpha >= 1) {
    check$push("Argument `alpha` must be a single number between 0 and 1.")
  }
  if (!is.numeric(min.n) || length(min.n) != 1 || is.na(min.n) ||
    min.n < 3) {
    check$push("Argument `min.n` must be a single number not lower than 3.")
  }
  if (!is_flag(detrend)) {
    check$push("Argument `detrend` must be either TRUE or FALSE.")
  }
  ann <- annual_series(y)
  if (is.character(ann)) check$push(ann)

  if (!check$isEmpty()) {
    stop(paste(check$getMessages(), collapse = " "))
  }


  ### Index values, one column per scale - - - - - - - - - - - - - - - - -

  if (index == "precomputed") {
    idx <- data
    if (is.null(scales)) {
      cn <- colnames(idx)
      num <- suppressWarnings(as.numeric(sub("^[^0-9]*", "", cn)))
      scales <- if (!is.null(cn) && !anyNA(num) && !anyDuplicated(num) &&
        all(num >= 1 & num %% 1 == 0)) {
        num
      } else {
        seq_len(NCOL(idx))
      }
    }
  } else {
    if (is.null(scales)) scales <- 1:24
    fun <- if (index == "spei") spei else spi
    opts <- list(...)
    if (is.null(opts$verbose)) opts$verbose <- FALSE
    idx <- do.call(cbind, lapply(scales, function(s) {
      as.numeric(do.call(fun, c(list(data = data, scale = s), opts))$fitted)
    }))
  }
  idx <- ts(matrix(as.numeric(idx), ncol = length(scales)),
    start = start(data), frequency = frequency(data)
  )
  colnames(idx) <- scales
  idx[!is.finite(idx)] <- NA


  ### Correlations - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  freq <- frequency(idx)
  idx_year <- index_years(idx)
  idx_cycle <- as.integer(cycle(idx))

  # Annual series, optionally detrended
  y_count <- ann$count
  ann <- ann$y
  yv <- as.numeric(ann)
  yr <- as.integer(round(time(ann)))
  if (detrend) {
    ok <- !is.na(yv)
    yv[ok] <- stats::residuals(stats::lm(yv[ok] ~ yr[ok]))
    ann <- ts(yv, start = yr[1], frequency = 1)
  }

  # Rows: lag and position in the year, in chronological order
  lag <- sort(as.integer(lag), decreasing = TRUE)
  rows <- data.frame(
    lag = rep(lag, each = freq),
    cycle = rep(seq_len(freq), length(lag))
  )
  rows$label <- position_label(rows$cycle, rows$lag, freq)

  dn <- list(position = rows$label, scale = scales)
  r <- p <- matrix(NA_real_, nrow(rows), length(scales), dimnames = dn)
  n <- matrix(0L, nrow(rows), length(scales), dimnames = dn)

  for (i in seq_len(nrow(rows))) {
    w <- which(idx_cycle == rows$cycle[i])
    yi <- yv[match(idx_year[w] + rows$lag[i], yr)]
    for (j in seq_along(scales)) {
      xi <- idx[w, j]
      ok <- !is.na(xi) & !is.na(yi)
      n[i, j] <- sum(ok)
      if (n[i, j] < min.n || stats::sd(xi[ok]) == 0 ||
        stats::sd(yi[ok]) == 0) {
        next
      }
      ct <- suppressWarnings(stats::cor.test(xi[ok], yi[ok],
        method = method, exact = FALSE
      ))
      r[i, j] <- ct$estimate
      p[i, j] <- ct$p.value
    }
  }
  p_adj <- p
  p_adj[] <- stats::p.adjust(p, method = p.adjust)


  ### Best windows - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  best_cell <- function(sign) {
    rr <- sign * r
    rr[is.na(p_adj) | p_adj > alpha | rr <= 0] <- NA
    if (all(is.na(rr))) {
      return(data.frame(
        lag = NA_integer_, cycle = NA_integer_, label = NA_character_,
        scale = NA_real_, r = NA_real_, p = NA_real_, p.adj = NA_real_,
        n = NA_integer_
      ))
    }
    k <- which.max(rr)
    ij <- arrayInd(k, dim(r))
    data.frame(
      lag = rows$lag[ij[1]], cycle = rows$cycle[ij[1]],
      label = rows$label[ij[1]], scale = scales[ij[2]], r = r[k], p = p[k],
      p.adj = p_adj[k], n = n[k]
    )
  }
  best <- cbind(
    sign = c("positive", "negative"),
    rbind(best_cell(1), best_cell(-1))
  )


  ### Output - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  z <- list(
    call = match.call(expand.dots = FALSE),
    cor = r, p = p, p.adj = p_adj, n = n, rows = rows, best = best,
    index = idx, y = ann, y.count = y_count,
    index.name = switch(index,
      spei = "SPEI",
      spi = "SPI",
      precomputed = "Index"
    ),
    lag = lag, method = method, p.adjust = p.adjust, alpha = alpha,
    min.n = min.n, detrend = detrend
  )
  class(z) <- "scale_scan"
  return(z)
}


#' @rdname Scale-scan
#' @export
#'
scale_extract <- function(x, cycle = NULL, scale = NULL, lag = 0,
                          which = "positive") {
  if (!inherits(x, "scale_scan")) {
    stop("Argument `x` must be a `scale_scan` object.")
  }
  if (is.null(cycle) != is.null(scale)) {
    stop("Arguments `cycle` and `scale` must be given together.")
  }
  if (is.null(cycle)) {
    if (!is.character(which) || length(which) != 1 ||
      !which %in% c("positive", "negative")) {
      stop("Argument `which` must be either `positive` or `negative`.")
    }
    b <- x$best[x$best$sign == which, ]
    if (is.na(b$cycle)) {
      stop(paste0(
        "There is no significant ", which, " window; give `cycle` ",
        "and `scale`."
      ))
    }
    cycle <- b$cycle
    scale <- b$scale
    lag <- b$lag
  }
  freq <- frequency(x$index)
  if (!is.numeric(cycle) || length(cycle) != 1 || is.na(cycle) ||
    !cycle %in% seq_len(freq)) {
    stop(paste0("Argument `cycle` must be an integer from 1 to ", freq, "."))
  }
  j <- match(scale, as.numeric(colnames(x$index)))
  if (!is.numeric(scale) || length(scale) != 1 || is.na(j)) {
    stop(paste0(
      "Argument `scale` must be one of the scales in the scan: ",
      paste(colnames(x$index), collapse = ", "), "."
    ))
  }
  if (!is.numeric(lag) || length(lag) != 1 || is.na(lag) || lag < 0 ||
    lag %% 1 != 0) {
    stop("Argument `lag` must be a single non-negative integer.")
  }
  yrs <- index_years(x$index) + lag
  w <- which(as.integer(stats::cycle(x$index)) == cycle)
  all_yrs <- seq(min(yrs), max(yrs))
  v <- rep(NA_real_, length(all_yrs))
  v[match(yrs[w], all_yrs)] <- x$index[w, j]
  out <- ts(v, start = all_yrs[1], frequency = 1)
  attr(out, "cycle") <- cycle
  attr(out, "scale") <- scale
  attr(out, "lag") <- lag
  out
}


#' @rdname Scale-scan
#' @export
#'
print.scale_scan <- function(x, ...) {
  freq <- frequency(x$index)
  yrs <- as.integer(round(time(x$y)))
  cat("Correlation scan of an annual series with the ", x$index.name,
    "\n",
    sep = ""
  )
  cat("  Positions in the year: ", freq, " (", cycle_unit(freq), "s)",
    if (any(x$lag > 0)) {
      paste0("; lags: ", paste(sort(x$lag), collapse = ", "), " years")
    },
    "\n",
    sep = ""
  )
  cat("  Time scales: ", paste(colnames(x$index), collapse = ", "), "\n",
    sep = ""
  )
  cat("  Years of the annual series: ", min(yrs), "-", max(yrs),
    if (x$detrend) " (detrended)", "\n",
    sep = ""
  )
  cnt <- x$y.count[x$y.count > 0]
  if (length(cnt) > 0 && max(cnt) > 1) {
    cat("  Values averaged per year: ", min(cnt), " to ", max(cnt), "\n",
      sep = ""
    )
  }
  cat("  Correlation: ", x$method, "; p-value adjustment: ", x$p.adjust,
    "; alpha = ", x$alpha, "\n",
    sep = ""
  )
  cat("  Significant cells: ", sum(x$p.adj <= x$alpha, na.rm = TRUE),
    " of ", sum(!is.na(x$cor)), "\n\n",
    sep = ""
  )
  cat("Best windows:\n")
  b <- x$best
  b$r <- round(b$r, 3)
  b$p <- signif(b$p, 3)
  b$p.adj <- signif(b$p.adj, 3)
  print(b, row.names = FALSE)
  invisible(x)
}


#' @rdname Scale-scan
#' @export
#'
summary.scale_scan <- function(object, ...) {
  print(object, ...)
}


#' @rdname Scale-scan
#' @export
#'
plot.scale_scan <- function(x, sig = "mark", main = NULL, ...) {
  if (!is.character(sig) || length(sig) != 1 ||
    !sig %in% c("mark", "mask", "none")) {
    stop("Argument `sig` must be one of `mark`, `mask` or `none`.")
  }
  freq <- frequency(x$index)
  scales <- as.numeric(colnames(x$index))
  rows <- x$rows
  nr <- nrow(rows)
  kk <- data.frame(
    pos = rep(seq_len(nr), length(scales)),
    scale = rep(scales, each = nr),
    r = as.numeric(x$cor),
    signif = as.numeric(x$p.adj) <= x$alpha
  )
  kk$signif[is.na(kk$signif)] <- FALSE
  if (sig == "mask") kk$r[!kk$signif] <- NA
  lim <- suppressWarnings(max(abs(x$cor), na.rm = TRUE))
  if (!is.finite(lim)) lim <- 1

  g <- ggplot(kk, aes(.data[["pos"]], .data[["scale"]])) +
    geom_tile(aes(fill = .data[["r"]]), color = "grey90") +
    scale_fill_gradient2(
      low = "tomato", mid = "white", high = "cyan4",
      midpoint = 0, limits = c(-lim, lim), na.value = "grey95"
    )
  if (sig == "mark" && any(kk$signif)) {
    g <- g + geom_point(data = kk[kk$signif, ], size = 0.6, alpha = 0.5)
  }
  b <- x$best[!is.na(x$best$cycle), ]
  if (nrow(b) > 0) {
    b$pos <- match(paste(b$lag, b$cycle), paste(rows$lag, rows$cycle))
    g <- g + geom_tile(data = b, fill = NA, color = "black")
  }
  if (length(x$lag) > 1) {
    g <- g + geom_vline(
      xintercept = freq * seq_len(length(x$lag) - 1) + 0.5,
      color = "grey40"
    )
  }
  # Thin the labels of the x axis to about 24
  k <- ceiling(nr / 24)
  brk <- which((rows$cycle - 1) %% k == 0)
  g <- g + scale_x_continuous(
    breaks = brk, labels = rows$label[brk], expand = c(0, 0)
  )
  sub <- paste(vapply(seq_len(nrow(x$best)), function(i) {
    bi <- x$best[i, ]
    if (is.na(bi$cycle)) {
      paste0("no significant ", bi$sign, " window")
    } else {
      paste0(
        "best ", bi$sign, ": r = ", round(bi$r, 2), " (",
        bi$label, ", scale ", bi$scale, ")"
      )
    }
  }, character(1)), collapse = "; ")
  g <- g +
    scale_y_continuous(breaks = int_breaks, expand = c(0, 0)) +
    xlab(paste0("Position in the year (", cycle_unit(freq), ")")) +
    ylab(paste0("Time scale (", cycle_unit(freq), "s)")) +
    labs(fill = paste0(
      switch(x$method,
        pearson = "Pearson's r",
        spearman = "Spearman's rho",
        kendall = "Kendall's tau"
      )
    ), subtitle = sub) +
    theme_bw() +
    theme(panel.grid = element_blank())
  if (length(brk) > 12) {
    g <- g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  }
  if (!is.null(main)) {
    g <- g + ggtitle(main)
  }
  g
}


# Internal helpers - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Year of each value of a ts: the cycle-th value of year Y has time
# Y + (cycle - 1) / frequency, so half a period is added before rounding
# down to avoid floating point errors.
index_years <- function(x) {
  as.integer(floor(as.numeric(time(x)) + 0.5 / frequency(x)))
}

# Integer axis breaks
int_breaks <- function(lim) {
  b <- pretty(lim)
  b[b %% 1 == 0 & b >= 1]
}

# Name of the sub-annual period for a given frequency
cycle_unit <- function(freq) {
  if (freq == 12) {
    "month"
  } else if (freq %in% c(52, 53)) {
    "week"
  } else if (freq %in% c(365, 366)) {
    "day"
  } else {
    "period"
  }
}

# Label of a position in the year and lag: 'Jan', 'Jan-1', 'W1-1', ...
position_label <- function(cycle, lag, freq) {
  base <- if (freq == 12) {
    month.abb[cycle]
  } else {
    paste0(toupper(substr(cycle_unit(freq), 1, 1)), cycle)
  }
  ifelse(lag > 0, paste0(base, "-", lag), base)
}

# Convert the annual series to a ts with frequency 1, averaging all the
# values of each year, or return an error message. Returns a list with the
# averaged series (y) and the number of values averaged (count).
annual_series <- function(y) {
  msg <- paste0(
    "Argument `y` must be a time series with frequency 1, or a data ",
    "frame or matrix with years in the first column and values in the ",
    "others."
  )
  if (is.ts(y)) {
    if (frequency(y) != 1 || !is.numeric(y)) {
      return(msg)
    }
    yrs <- round(as.numeric(time(y)))
    vals <- as.matrix(y)
  } else if (is.data.frame(y) || (is.matrix(y) && is.numeric(y))) {
    y <- as.data.frame(y)
    if (ncol(y) < 2 || !all(vapply(y, is.numeric, logical(1))) ||
      anyNA(y[[1]]) || any(y[[1]] %% 1 != 0)) {
      return(msg)
    }
    yrs <- y[[1]]
    vals <- as.matrix(y[, -1, drop = FALSE])
  } else {
    return(msg)
  }
  all_yrs <- seq(min(yrs), max(yrs))
  f <- factor(rep(yrs, ncol(vals)), levels = all_yrs)
  v <- as.numeric(vals)
  ok <- !is.na(v)
  s <- tapply(v[ok], f[ok], sum)
  cnt <- tapply(v[ok], f[ok], length)
  s[is.na(s)] <- 0
  cnt[is.na(cnt)] <- 0
  m <- ifelse(cnt > 0, s / pmax(cnt, 1), NA_real_)
  list(
    y = ts(as.numeric(m), start = all_yrs[1], frequency = 1),
    count = ts(as.integer(cnt), start = all_yrs[1], frequency = 1)
  )
}
