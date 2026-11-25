# Internal helpers

# Is `x` a single TRUE or FALSE value?
is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

# Check the `dates` argument of the daily PET functions (`penman()`,
# `hargreaves()`). `dates` must be a `Date` vector, or a character vector
# that `as.Date()` can convert, with one element per time step, no missing
# values and in strictly increasing order (gaps are allowed). Problems are
# pushed to the assert collection `check`. Returns `dates` as a `Date`
# vector, or NULL if it is not a valid `Date` vector.
check_dates <- function(dates, n_times, check) {
  if (is.character(dates)) {
    dates <- tryCatch(as.Date(dates), error = function(e) NULL)
  }
  if (!inherits(dates, "Date")) {
    check$push(paste(
      "Argument `dates` must be a `Date` vector, or a character vector that",
      "can be converted with `as.Date()`."
    ))
    return(NULL)
  }
  if (length(dates) != n_times) {
    check$push(paste0(
      "Argument `dates` must have one element per time step (", n_times,
      "), but it has ", length(dates), "."
    ))
    return(NULL)
  }
  if (anyNA(dates)) {
    check$push("Argument `dates` must not contain NA values.")
    return(NULL)
  }
  if (any(diff(as.numeric(dates)) <= 0)) {
    check$push(paste(
      "Argument `dates` must be in strictly increasing order",
      "(sorted, without repeated dates)."
    ))
    return(NULL)
  }
  dates
}

# Format the output of the daily PET functions. `x` is the internal
# (time x site x site) array of daily ET0, `out_type` the type of input
# ("vector", "matrix" or "array"; time series input is "vector" or
# "matrix"), `dates` the checked `Date` vector and `names` the dimnames of
# the input. Vector and matrix input give a `zoo` object indexed by
# `dates` (keeping the column names of a matrix); 3-d array input gives an
# array whose first dimnames are the dates in ISO format.
daily_output <- function(x, out_type, dates, names) {
  n_times <- length(dates)
  if (out_type == "vector") {
    return(zoo(as.vector(x), order.by = dates))
  }
  if (out_type == "matrix") {
    x <- matrix(x, nrow = n_times)
    colnames(x) <- names[[2]]
    return(zoo(x, order.by = dates))
  }
  dn <- vector("list", 3)
  if (!is.null(names)) {
    dn[seq_along(names)] <- names
  }
  dn[[1]] <- format(dates, "%Y-%m-%d")
  dimnames(x) <- dn
  x
}

# Check the `start` and `frequency` arguments of `spei()` / `spi()` (and
# `start` of the monthly PET functions, where `frequency` is always 12).
# They have the meaning of `ts()`: `start` is a year, or c(year, period),
# and `frequency` the number of periods per year. Problems are pushed to
# the assert collection `check`. Returns TRUE if both are valid (or NULL).
check_start <- function(start, frequency, check) {
  ok <- TRUE
  if (!is.null(frequency)) {
    if (!is.numeric(frequency) || length(frequency) != 1 ||
      is.na(frequency) || frequency < 1 || frequency %% 1 != 0) {
      check$push(paste(
        "Argument `frequency` must be a single positive integer (the",
        "number of periods per year, e.g. 12 for monthly data)."
      ))
      ok <- FALSE
    }
  }
  if (!is.null(start)) {
    if (!is.numeric(start) || !length(start) %in% c(1, 2) ||
      anyNA(start) || any(start %% 1 != 0)) {
      check$push(paste(
        "Argument `start` must be a year, or a vector c(year, period) of",
        "two whole numbers, as in `ts()`."
      ))
      ok <- FALSE
    } else if (ok && length(start) == 2) {
      f <- if (is.null(frequency)) 12 else frequency
      if (start[2] < 1 || start[2] > f) {
        check$push(paste0(
          "The period in `start` (", start[2], ") must be between 1 and ",
          "the number of periods per year (", f, ")."
        ))
        ok <- FALSE
      }
    }
  }
  ok
}

# Labels for the `n` time steps of a regular series starting at `start` (a
# year, or c(year, period)) with `frequency` periods per year: "YYYY-MM"
# for monthly data (frequency 12), "YYYY" for annual data (frequency 1),
# and "YYYY-Pnn" otherwise, with the period nn zero-padded to the number of
# digits of the frequency (e.g. "1961-P01" to "1961-P48").
time_labels <- function(start, frequency, n) {
  if (length(start) == 1) start <- c(start, 1)
  idx <- (start[2] - 1) + seq_len(n) - 1
  year <- formatC(start[1] + idx %/% frequency,
    width = 4, flag = "0", format = "d"
  )
  per <- idx %% frequency + 1
  if (frequency == 12) {
    paste0(year, "-", formatC(per, width = 2, flag = "0", format = "d"))
  } else if (frequency == 1) {
    year
  } else {
    paste0(
      year, "-P",
      formatC(per, width = nchar(frequency), flag = "0", format = "d")
    )
  }
}

# The inverse of `time_labels()`: if `labels` are the labels of a regular
# series with `frequency` periods per year, return its start as
# c(year, period); otherwise return NULL. Monthly labels can also be dates
# ("YYYY-MM-DD", any day of the month, e.g. the mid-month dates of NetCDF
# files), as long as there is one per month and the months are
# consecutive.
parse_time_labels <- function(labels, frequency) {
  if (!is.character(labels) || length(labels) == 0 || anyNA(labels)) {
    return(NULL)
  }
  if (frequency == 12) {
    pattern <- "^([0-9]{4})-([0-9]{2})(-[0-9]{2})?$"
  } else if (frequency == 1) {
    pattern <- "^([0-9]{4})$"
  } else {
    pattern <- "^([0-9]{4})-P([0-9]+)$"
  }
  if (!all(grepl(pattern, labels))) {
    return(NULL)
  }
  year <- as.numeric(sub(pattern, "\\1", labels))
  per <- if (frequency == 1) 1 else as.numeric(sub(pattern, "\\2", labels))
  if (any(per < 1 | per > frequency)) {
    return(NULL)
  }
  full <- nchar(labels) == 10
  if (any(full) && anyNA(as.Date(labels[full], optional = TRUE))) {
    return(NULL)
  }
  idx <- year * frequency + per - 1
  if (any(diff(idx) != 1)) {
    return(NULL)
  }
  c(year[1], per[1])
}

# Handle the `start` argument of the monthly PET functions, for the main
# input `x` (`Tave` or `Tmin`). `start` can not be used with a time series
# or with daily data (`daily`, i.e. `dates` given). A vector or a matrix is
# converted to a monthly time series; a 3-d array keeps its shape. Returns
# a list with the (converted) input `x`, and `start`, which is NULL unless
# `x` is a 3-d array with valid time information.
pet_start <- function(x, start, daily, check) {
  if (is.null(start)) {
    return(list(x = x, start = NULL))
  }
  if (daily) {
    check$push(paste(
      "Argument `start` can not be used with daily data (`dates`), which",
      "carry their own dates."
    ))
  } else if (is.ts(x)) {
    check$push(paste(
      "Argument `start` can not be used when the input data are a time",
      "series (`ts`), which already has its own time attributes."
    ))
  } else if (check_start(start, 12, check)) {
    if (is.data.frame(x)) {
      x <- data.matrix(x)
    }
    if (is.numeric(x) && (is.vector(x) || is.matrix(x))) {
      return(list(x = ts(x, start = start, frequency = 12), start = NULL))
    }
    return(list(x = x, start = start))
  }
  list(x = x, start = NULL)
}

# Set the time (first) dimnames of the 3-d array `x` to `labels`, keeping
# the other dimnames in `names` (the dimnames of the input, or NULL).
set_time_dimnames <- function(x, names, labels) {
  dn <- vector("list", 3)
  if (!is.null(names)) {
    dn[seq_along(names)] <- names
  }
  dn[[1]] <- labels
  dimnames(x) <- dn
  x
}
