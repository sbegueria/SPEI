# Internal helpers

# Is `x` a single TRUE or FALSE value?
is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}
