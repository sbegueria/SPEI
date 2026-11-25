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
