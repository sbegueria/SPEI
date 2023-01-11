#' @title Computation of potential evapotranspiration.
#' @description See hargreaves
#' @details See hargreaves
#' @return A time series with the values of monthly potential or reference
#' evapotranspiration, in mm. If the input is a matrix or a multivariate time
#' series each column will be treated as independent data (e.g., diferent
#' observatories), and the output will be a multivariate time series.
#'
#' @rdname Potential-evapotranspiration
#' @export
#'
thornthwaite <- function(Tave, lat, na.rm = FALSE, verbose = TRUE) {
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Determine which combinations of inputs were passed and check their
  # validity, and check that all the inputs have the same dimensions

  # Instantiate two new 'ArgCheck' objects to collect errors and warnings
  check <- makeAssertCollection()
  warn <- makeAssertCollection()

  # A list of computation options
  using <- list(na.rm = FALSE)

  # Check optional inputs
  if (na.rm != TRUE && na.rm != FALSE) {
    check$push("Argument `na.rm` must be set to either TRUE or FALSE.")
  } else if (na.rm) {
    warn$push("Missing values (`NA`) will not be considered in the calculation.")
  } else {
    warn$push("Checking for missing values (`NA`): all the data must be complete.")
  }

  # Check for missing values in inputs
  if (!na.rm && (anyNA(Tave))) {
    check$push("`Tave` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  if (!na.rm && anyNA(lat)) {
    check$push("`lat` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  # Determine input dimensions and compute internal dimensions (int_dims)
  tmin_dims <- dim(Tave)
  if (is.null(tmin_dims) || length(tmin_dims) == 1) {
    # vector input (single-site)
    int_dims <- c(length(Tave), 1, 1)
  } else if (length(tmin_dims) == 2) {
    # matrix input (multi-site)
    int_dims <- c(tmin_dims, 1)
  } else if (length(tmin_dims) == 3) {
    # 3D array input (gridded data)
    int_dims <- tmin_dims
  } else {
    int_dims <- tmin_dims
    check$push("Input data can not have more than three dimensions")
  }
  n_sites <- prod(int_dims[[2]], int_dims[[3]])
  n_times <- int_dims[[1]]

  # Determine output data shape
  if (is.ts(Tave)) {
    if (is.matrix(Tave)) {
      out_type <- "tsmatrix"
    } else {
      out_type <- "tsvector"
    }
  } else if (is.vector(Tave)) {
    out_type <- "vector"
  } else if (is.matrix(Tave)) {
    out_type <- "matrix"
  } else { # is.array; default
    out_type <- "array"
  }
  warn$push(paste0("Input type is ", out_type, "."))

  # Save column names for later
  names <- dimnames(Tave)

  # Determine dates: month length and mid-month day-within-year
  if (is.ts(Tave)) {
    ts_freq <- frequency(Tave)
    ts_start <- start(Tave)
    cyc <- cycle(Tave)
    if (ts_freq != 12) {
      check$push("Input data needs to be have a frequency of 12 if provided as a time series (i.e., a monthly time series).")
    }
    ym <- as.yearmon(time(Tave))
    warn$push(paste0("Time series spanning ", ym[1], " to ", ym[n_times], "."))
    date <- as.Date.yearmon(ym)
    mlen_array <- array(as.numeric(lubridate::days_in_month(date)), dim = int_dims)
    msum_array <- array(yday(date) + round((mlen_array / 2) - 1), dim = int_dims)
  } else {
    mlen <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    msum <- cumsum(mlen) - mlen + 15
    cyc <- array(c(1:12), dim = int_dims[1])
    mlen_array <- array(mlen, dim = int_dims)
    msum_array <- array(msum, dim = int_dims)
    warn$push("Assuming the data are monthly time series starting in January, all regular (non-leap) years.")
  }

  # Verify the length of each input variable
  input_len <- prod(int_dims)
  if (sum(lengths(lat)) != n_sites) {
    check$push("`lat` has incorrect length.")
  }

  # Create uniformly-dimensioned arrays from input
  Tave <- array(data.matrix(Tave), int_dims)
  lat <- aperm(array(data.matrix(lat), int_dims[c(2, 3, 1)]), c(3, 1, 2))

  # Return errors and halt execution (if any)
  if (!check$isEmpty()) {
    stop(paste(check$getMessages(), collapse = " "))
  }

  # Show a warning with computation options
  if (verbose) {
    print(paste(warn$getMessages(), collapse = " "))
  }


  ### Computation of PET - - - - - - - - - - - - - - - - - - - - - - - - -

  # Initialize PET
  PET <- Tave * NA
  # Monthly correction factor, depending on latitude (K)
  tanLat <- tan(lat / 57.2957795)
  # mean solar declination angle for each month (Delta)
  Delta <- 0.4093 * sin(((2 * pi * msum_array) / 365) - 1.405)
  # hourly angle of sun rising (omega)
  tanDelta <- tan(Delta)
  tanLatDelta <- tanLat * tanDelta
  tanLatDelta <- ifelse(tanLatDelta < (-1), -1, tanLatDelta)
  tanLatDelta <- ifelse(tanLatDelta > 1, 1, tanLatDelta)
  omega <- acos(-tanLatDelta)
  # mean daily daylight hours for each month (N)
  N <- 24 / pi * omega
  # which leads to K
  K <- N / 12 * mlen_array / 30

  # Annual temperature efficiency index (J)
  Tt <- apply(Tave, c(2, 3), function(x) {
    y <- aggregate(x, by = list(cyc), mean, na.rm = na.rm)
    y[, 2]
  })
  Tt[Tt < 0] <- 0
  J <- apply(Tt, c(2, 3), function(x) {
    sum((x / 5)^1.514, na.rm = na.rm)
  })
  J <- aperm(array(data.matrix(J), int_dims[c(2, 3, 1)]), c(3, 1, 2))

  # Empirical exponent (q)
  J2 <- J * J
  J3 <- J2 * J
  q <- 0.000000675 * J3 - 0.0000771 * J2 + 0.01792 * J + 0.49239

  # Potential evapotranspiration series (PE)
  Tave[Tave < 0] <- 0
  if (sum(J) != 0) {
    PET <- K * 16 * (10 * Tave / J)^q
  } else {
    PET <- K * 0
  }

  ### Format output and return - - - - - - - - - - - - - - - - - - - - - - -

  if (out_type == "tsmatrix") {
    PET <- matrix(PET, nrow = n_times)
    PET <- ts(PET, frequency = ts_freq, start = ts_start)
    colnames(PET) <- rep("PET_tho", ncol(PET))
  } else if (out_type == "tsvector") {
    PET <- as.vector(PET)
    PET <- ts(PET, frequency = ts_freq, start = ts_start)
  } else if (out_type == "vector") {
    PET <- as.vector(PET)
  } else if (out_type == "matrix") {
    PET <- matrix(PET, nrow = n_times)
    colnames(PET) <- rep("PET_tho", ncol(PET))
  } else { # array, default
    colnames(PET) <- rep("PET_tho", ncol(PET))
  }
  dimnames(PET) <- names

  return(PET)
}
