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
