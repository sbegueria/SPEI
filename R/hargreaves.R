#' @name Potential-evapotranspiration
#' @title Computation of potential and reference evapotranspiration.
#' @aliases thornthwaite penman
#' @usage
#' thornthwaite(Tave, lat, na.rm = FALSE, verbose=TRUE)
#'
#' hargreaves(Tmin, Tmax, Ra = NULL, lat = NULL, Pre = NULL, na.rm = FALSE,
#' verbose=TRUE)
#'
#' penman(Tmin, Tmax, U2, Ra = NULL, lat = NULL, Rs = NULL, tsun = NULL,
#'        CC = NULL, ed = NULL, Tdew = NULL, RH = NULL, P = NULL, P0 = NULL,
#'        CO2 = NULL, z = NULL, crop='short', na.rm = FALSE, method='ICID',
#'        verbose=TRUE)
#'
#'
#' @description
#' Potential evapotranspiration (PET) is the amount of evaporation and
#' transpiration that would occur if a sufficient water source were available.
#' Reference evapotranspiration (ETo) is the amount of evaporation and
#' transpiration from a reference vegetation of grass. They are usually
#' considered equivalent. This set of functions calculate PET or ETo according
#' to the Thornthwaite, Hargreaves or Penman-Monteith equations.
#'
#'
#' @param Tave   a numeric vector, tsvector, matrix, tsmatrix, or 3-d array of
#' monthly mean temperatures, ºC.
#' @param lat   a numeric vector or matrix with the latitude of the site or
#' sites, in degrees.
#' @param na.rm   optional, a logical value indicating whether NA values should
#' be stripped from the computations.
#' @param Tmax   a numeric vector, tsvector, matrix, tsmatrix, or 3-d array of
#' monthly mean daily maximum temperatures, ºC.
#' @param Tmin   a numeric vector, tsvector, matrix, tsmatrix, or 3-d array of
#' monthly mean daily minimum temperatures, ºC.
#' @param Ra   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean daily external radiation, MJ m-2 d-1.
#' @param Pre   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array  of monthly total precipitation, mm.
#' @param U2   a numeric vector, tsvector, matrix, tsmatrix, or 3-d array of
#' monthly mean daily wind speeds at 2 m height, m s-1.
#' @param Rs   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean daily incoming solar radiation, MJ m-2 d-1.
#' @param tsun   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean daily bright sunshine hours, h.
#' @param CC   optional, numeric a vector, matrix or time series of monthly
#' mean cloud cover, \%.
#' @param ed   optional, numeric a vector, matrix or time series of monthly
#' mean actual vapor pressure at 2 m height, kPa.
#' @param Tdew   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean daily dewpoint temperature (used for
#' estimating ed), ºC.
#' @param RH   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean relative humidity (used for estimating ed), \%.
#' @param P   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean atmospheric pressure at surface, kPa.
#' @param P0   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean atmospheric pressure at sea level (used for
#' estimating P), kPa.
#' @param CO2   optional, a single numeric value, a numeric vector,
#' or a tsvector of monthly mean CO2 atmospheric concentration, ppm.
#' @param z   optional, a numeric vector or matrix of the elevation of the
#' site or sites, m above sea level.
#' @param crop   optional, character string, type of reference crop. Either
#' one of 'short' (default) or 'tall'.
#' @param method   optional, character string, Penman-Monteith calculation
#' method. Either one of 'ICID' (default), 'FAO', or 'ASCE'.
#' @param verbose   optional, logical, report the computation options during
#' calculation. Either 'TRUE' (default) or 'FALSE'.
#'
#'
#' @details \code{thornthwaite} computes the monthly potential
#' evapotranspiration (PE) according to the Thornthwaite (1948) equation.
#' It is the simplest of the three methods, and can be used when only
#' mean temperature data are available.
#'
#'
#' \code{hargreaves} computes the monthly reference evapotranspiration (ETo)
#' of a grass crop based on the original Hargreaves equation (1994). However,
#' if precipitation data \code{Pre} is provided a modified form due to Droogers
#' and Allen (2002) will be used; this equation corrects ETo using
#' the amount of rain of each month as a proxy for irradiation The Hargreaves
#' method requires data on the mean external radiation, \code{Ra}. If \code{Ra} is not available it
#' can be estimated from the latitude \code{lat} and the month of the year.
#'
#'
#' \code{penman} calculates the monthly reference evapotranspiration (ETo) of
#' a hypothetical reference crop according to the Penman-Monteith equation
#' (Monteith, 1965). This is widely considered the most accurate method for
#' estimating ETo, and is the method recommended by the UN Food and Agriculture
#' Organization (FAO). There are several methods which simplify the original
#' Penman-Monteith equation and are used in practice. Here we follow by default
#' the procedure described in Allen et al. (1994), aka the `ICID` method.
#' However, other versions are also implemented and can be used by setting the
#' \code{method} parameter to the appropriate value. The FAO-56 method
#' (Allen et al., 1998) can be used by setting \code{method} to `FAO`,
#' while the variation of the American Society of Civil Engineers (Walter et
#' al., 2002 is used when setting it to `ASCE`.
#' By default the original parameterization corresponding to a short reference
#' crop of 0.12 m height is used, although the parameterization for a tall
#' reference crop of 0.5 m height (Walter et al. (2002) can also be used,
#' by setting the \code{crop} parameter to 'tall'.
#' The method requires data on the incoming solar radiation, \code{Rs};
#' since this is seldom available, the code will estimate it from data on the
#' bright sunshine duration \code{tsun}, or alternatively from data on the
#' percent cloud cover \code{CC}. Similarly, if data on the saturation water
#' pressure \code{ed} are not available, it is possible to estimate it from
#' the dew point temperature \code{Tdew}, from the relative humidity \code{RH}
#' or even from the minimum temperature \code{Tmin} (sorted from least to most
#' uncertain method). Similarly, the atmospheric surface pressure \code{P}
#' required for computing the psychrometric constant can be calculated from the
#' atmospheric pressure at sea level \code{P0} and the elevation \code{z},
#' or else it will be assumed to be constant (101.3 kPa). If no wind speed
#' data \code{U2} are available, a constant value of 2 m per second is used.
#' Custom CO2 atmospheric concentration \code{CO2} can also be provided,
#' following Yang et al. (2019).
#'
#' The function will produce an error message if a valid combination of input
#' parameters is not provided.
#' If \code{verbose} is `TRUE` (the default value), a message will be produced
#' informing on the computation options selected.
#'
#' The function accepts data in a variety of formats, including 1-D vectors,
#' 2-D matrices and 3-D arrays.
#' The input data format will determine the output format.
#' Vector input can be used for single-station data. Matrix input can be used
#' to produce output for a number of locations, where each column in the matrix
#' will be considered as one location. 3-D arrays can be used to process
#' gridded data, with the first dimension being time and the two other
#' dimensions representing spatial coordinates. See the examples below for a
#' guidance on how to use these options.
#'
#' If the main input object (\code{Tave}, \code{Tmin}, \code{Tmax}) is a time
#' series then the function \code{\link{cycle}} will be used to determine the
#' position of each observation within the
#' year (month), allowing the data to start in a month different than January.
#' If no time information is provided, then the input data will be treated as
#' a sequence of monthly values starting in January.
#'
#'
#' @return A vector, matrix or 3-d array with the values of monthly potential
#' or reference evapotranspiration, in mm.
#'
#'
#' @references
#' Thornthwaite, C. W., 1948. An approach toward a rational classification of
#' climate.
#' \emph{Geographical Review} \bold{38}: 55–94. DOI:10.2307/2107309.
#'
#' Hargreaves G.H., 1994. Defining and using reference evapotranspiration.
#' \emph{Journal of Irrigation and Drainage Engineering} \bold{120}: 1132–1139.
#'
#' Droogers P., Allen R. G., 2002. Estimating reference evapotranspiration
#' under inaccurate data conditions. \emph{Irrigation and Drainage Systems}
#' \bold{16}: 33–45.
#'
#' Monteith, J. L., 1965. Evaporation and environment. \emph{Symposia of the
#' Society for Experimental Biology} \bold{19}: 205–234.
#'
#' Allen R. G., Smith M., Pereira L. S., Perrier A., 1994. An update for the
#' calculation of reference evapotranspiration. \emph{ICID Bulletin of the
#' International Commission on Irrigation and Drainage}, 35–92.
#'
#' Allen R.G., Pereira L.S.,Raes D., Smith, M., 1998. \emph{J. Crop
#' evapotranspiration - Guidelines for computing crop water requirements - FAO
#' Irrigation and drainage paper 56}. FAO, Rome. ISBN 92-5-104219-5.
#'
#' Walter I.A. and 14 co-authors, 2002. \emph{The ASCE standardized reference
#' evapotranspiration equation.} Rep. Task Com. on Standardized Reference
#' Evapotranspiration July 9, 2002, EWRI–Am. Soc. Civil Engr., Reston, VA,
#' 57 pp.
#'
#' Yang, Y., Roderick, M.L., Zhang, S. McVicar, T., Donohue, R.J., 2019.
#' Hydrologic implications of vegetation response to elevated CO2 in climate
#' projections. \emph{Nature Climate Change} \bold{9}: 44–48.
#'
#'
#' @author Santiago Beguería
#'
#'
#' @examples
#' # Load data for Tampa, lat=37.6475N, elevation=402.6 m. a.s.l.
#' # Data consists on monthly values since January 1980
#'
#' data(wichita)
#' attach(wichita)
#' names(wichita)
#'
#' # PET following Thornthwaite
#' tho <- thornthwaite(TMED, 37.6475)
#'
#' # ETo by Hargreaves
#' har <- hargreaves(TMIN, TMAX, lat = 37.6475)
#'
#' # ETo by Penman, based on sun hours, ignore NAs
#' pen <- penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE)
#'
#' # Penman, based on cloud cover
#' pen2 <- penman(TMIN, TMAX, AWND, CC = ACSH, lat = 37.6475, z = 402.6, na.rm = TRUE)
#'
#' # Penman, with constant wind
#' pen3 <- penman(TMIN, TMAX, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE)
#'
#' # Plot them together
#' plot(ts(cbind(tho, har, pen, pen2, pen3), fr = 12))
#'
#' # Compare between methods
#' pairs(cbind(tho, har, pen, pen2, pen3))
#'
#' # Input data as a time series vector; note that only the first parameter
#' # needs to be a `ts` object.
#'
#' thornthwaite(ts(TMED, start = c(1980, 1), frequency = 12), 37.6475)
#' hargreaves(ts(TMIN, start = c(1980, 1), frequency = 12), TMAX, lat = 37.6475)
#' penman(ts(TMIN, start = c(1980, 1), frequency = 12), TMAX, AWND,
#'   tsun = TSUN,
#'   lat = 37.6475, z = 402.6, na.rm = TRUE
#' )
#'
#' # Input data as a time series. Consider the data started in June 1980
#' thornthwaite(ts(TMED, start = c(1980, 6), frequency = 12), 37.6475)
#'
#' # Comparison with example from Allen et al. (1998), p. 69, fig. 18:
#' # Data from Cabinda, Angola (-5.33S, 12.11E, 20 m a.s.l.)
#'
#' data(cabinda)
#' pen.cab <- penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2,
#'   Rs = cabinda$Rs, tsun = cabinda$tsun, RH = cabinda$RH, lat = -5.33, z = 20
#' )
#' plot(cabinda$ET0, pen.cab)
#' abline(0, 1, lt = "dashed")
#' summary(lm(pen.cab ~ cabinda$ET0))$r.squared
#'
#' # Matrix input (data from several stations)
#' # Replicating Wichita data twice to simulate data at two locations.
#'
#' tmin <- cbind(TMIN, TMIN + 1.5)
#' tmax <- cbind(TMAX, TMAX + 1.5)
#' lat <- c(37.6475, 35.000)
#' har <- hargreaves(tmin, tmax, lat = lat, na.rm = TRUE)
#' plot(har)
#' abline(0, 1)
#' plot(ts(har, fr = 12))
#'
#' # Array input (gridded data)
#' # Replicating Wichita data to simulate data from a grid. Note that the time
#' # dimension (`nt`) comes first. Latitude is provided as a 2-d array.
#'
#' nt <- length(TMIN)
#' tmin <- array(TMIN, dim = c(nt, 2, 2))
#' tmax <- array(TMAX, dim = c(nt, 2, 2))
#' lat <- array(c(40, 30, 40, 30), dim = c(2, 2))
#' har <- hargreaves(tmin, tmax, lat = lat, na.rm = TRUE)
#' dim(har)
#'
#' # Different Penman-Monteith flavors
#'
#' pen_icid <- penman(TMIN, TMAX, AWND,
#'   tsun = TSUN, lat = 37.6475, z = 402.6,
#'   na.rm = TRUE, method = "ICID"
#' )
#' pen_asce <- penman(TMIN, TMAX, AWND,
#'   tsun = TSUN, lat = 37.6475, z = 402.6,
#'   na.rm = TRUE, method = "ASCE"
#' )
#' pen_fao <- penman(TMIN, TMAX, AWND,
#'   tsun = TSUN, lat = 37.6475, z = 402.6,
#'   na.rm = TRUE, method = "FAO"
#' )
#' plot(ts(cbind(pen_icid, pen_asce, pen_fao), fr = 12))
#'
#' # Different CO2 concentrations
#'
#' # Default (300 ppm)
#' pen_300 <- penman(TMIN, TMAX, AWND,
#'   tsun = TSUN, lat = 37.6475, z = 402.6,
#'   na.rm = TRUE
#' )
#' # Increased to 450 ppm
#' pen_450 <- penman(TMIN, TMAX, AWND,
#'   tsun = TSUN, lat = 37.6475, z = 402.6,
#'   CO2 = 450, na.rm = TRUE
#' )
#' plot(pen_450, pen_300)
#' abline(0, 1)
#' # Increasing from 300 to 450
#' co2 <- seq(300, 450, length.out = length(TMIN))
#' pen_co2 <- penman(TMIN, TMAX, AWND,
#'   tsun = TSUN, lat = 37.6475, z = 402.6,
#'   CO2 = co2, na.rm = TRUE
#' )
#' plot(ts(cbind(pen_300, pen_co2), fr = 12))
#'
#' @export
#'
hargreaves <- function(Tmin, Tmax, Ra = NULL, lat = NULL, Pre = NULL,
                       na.rm = FALSE, verbose = TRUE) {
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Determine which combinations of inputs were passed and check their
  # validity, and check that all the inputs have the same dimensions

  # Instantiate two objects to collect errors and warnings
  check <- makeAssertCollection()
  warn <- makeAssertCollection()

  # Report on the method being used
  warn$push("Calculating reference evapotranspiration using the Hargreaves method.")

  # A list of computation options
  using <- list(Ra = FALSE, lat = FALSE, Pre = FALSE, na.rm = FALSE)

  # Check optional inputs
  if (!is.null(Ra)) {
    using$Ra <- TRUE
    warn$push("Using user-provided extraterrestrial radiation (`Ra`) data.")
  } else if (!is.null(lat)) {
    using$lat <- TRUE
    warn$push("Using latitude (`lat`) to estimate extraterrestrial radiation.")
  } else {
    check$push("One of `Ra` or `lat` must be provided.")
  }

  if (!is.null(Pre)) {
    using$Pre <- TRUE
    warn$push("Using precipitation data, following Droogers and Allen (2002).")
  }

  if (na.rm != TRUE && na.rm != FALSE) {
    check$push("Argument `na.rm` must be set to either TRUE or FALSE.")
  } else if (na.rm) {
    warn$push("Missing values (`NA`) will not be considered in the calculation.")
  } else {
    warn$push("Checking for missing values (`NA`): all the data must be complete.")
  }

  if (!is.logical(verbose)) {
    check$push("Argument `verbose` must be set to either TRUE or FALSE.")
  }

  # Check for missing values in inputs
  if (!na.rm && (anyNA(Tmin) || anyNA(Tmax))) {
    check$push("`Tmin` and `Tmax` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  if (!na.rm && (anyNA(Ra))) {
    check$push("`Ra` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  if (!na.rm && (anyNA(Pre))) {
    check$push("`Pre` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  if (using$lat && anyNA(lat)) {
    check$push("`lat` cannot be missing.")
  }

  # Determine input dimensions and compute internal dimensions (int_dims)
  tmin_dims <- dim(Tmin)
  if (is.null(tmin_dims) || length(tmin_dims) == 1) {
    # vector input (single-site)
    int_dims <- c(length(Tmin), 1, 1)
  } else if (length(tmin_dims) == 2) {
    # matrix input (multi-site)
    int_dims <- c(tmin_dims, 1)
  } else if (length(tmin_dims) == 3) {
    # 3D array input (gridded data)
    int_dims <- tmin_dims
  } else {
    int_dims <- tmin_dims
    check$push("Input data can not have more than three dimensions.")
  }
  n_sites <- prod(int_dims[[2]], int_dims[[3]])
  n_times <- int_dims[[1]]

  # Determine output data shape
  if (is.ts(Tmin)) {
    if (is.matrix(Tmin)) {
      out_type <- "tsmatrix"
    } else {
      out_type <- "tsvector"
    }
  } else if (is.vector(Tmin)) {
    out_type <- "vector"
  } else if (is.matrix(Tmin)) {
    out_type <- "matrix"
  } else { # is.array; default
    out_type <- "array"
  }
  warn$push(paste0("Input type is ", out_type, "."))

  # Save column names for later
  names <- dimnames(Tmin)

  # Determine dates: month length and mid-month day-within-year
  if (is.ts(Tmin)) {
    ts_freq <- frequency(Tmin)
    ts_start <- start(Tmin)
    if (ts_freq != 12) {
      check$push("Input data needs to be have a frequency of 12 if provided as a time series (i.e., a monthly time series).")
    }
    ym <- as.yearmon(time(Tmin))
    warn$push(paste0("Time series spanning ", ym[1], " to ", ym[n_times], "."))
    date <- as.Date.yearmon(ym)
    mlen_array <- array(as.numeric(lubridate::days_in_month(date)), dim = int_dims)
    msum_array <- array(yday(date) + round((mlen_array / 2) - 1), dim = int_dims)
  } else {
    mlen <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    msum <- cumsum(mlen) - mlen + 15
    mlen_array <- array(mlen, dim = int_dims)
    msum_array <- array(msum, dim = int_dims)
    warn$push("Assuming the data are monthly time series starting in January, all regular (non-leap) years.")
  }

  # Verify the length of each input variable
  input_len <- prod(int_dims)
  if (sum(lengths(Tmin)) != input_len || sum(lengths(Tmax)) != input_len) {
    check$push("`Tmin` and `Tmax` cannot have different lengths.")
  }
  if (using$Ra && sum(lengths(Ra)) != input_len) {
    check$push("`Ra` has incorrect length.")
  }
  if (using$lat && sum(lengths(lat)) != n_sites) {
    check$push("`lat` has incorrect length.")
  }
  if (using$Pre && sum(lengths(Pre)) != input_len) {
    check$push("`Pre` has incorrect length.")
  }

  # Create uniformly-dimensioned arrays from input
  Tmin <- array(data.matrix(Tmin), int_dims)
  Tmax <- array(data.matrix(Tmax), int_dims)
  if (using$Ra) {
    Ra <- array(data.matrix(Ra), int_dims)
  }
  if (using$lat) {
    lat <- aperm(array(data.matrix(lat), int_dims[c(2, 3, 1)]), c(3, 1, 2))
  }
  if (using$Pre) {
    Rs <- array(data.matrix(Pre), int_dims)
  }

  # Return errors and halt execution (if any)
  if (!check$isEmpty()) {
    stop(paste(check$getMessages(), collapse = " "))
  }

  # Show a warning with computation options
  if (verbose) {
    print(paste(warn$getMessages(), collapse = " "))
  }


  ### Computation of ETo - - - - - - - - - - - - - - - - - - - - - - - - -

  # Mean temperature
  Tmean <- (Tmin + Tmax) / 2

  # Temperature range, ºC
  Tr <- Tmax - Tmin
  Tr <- pmax(0, Tr)

  # Initialize ET0
  ET0 <- Tmin * NA

  # 1. External radiation, MJ m-2 d-1
  if (!using$Ra) {
    # estimate Ra, following Allen et al. (1994)
    # number of day in the year
    J <- msum_array
    # solar declination, rad (1 rad = 57.2957795 deg)
    delta <- 0.409 * sin(0.0172 * J - 1.39)
    # relative distance Earth-Sun, []
    dr <- 1 + 0.033 * cos(0.0172 * J)
    # sunset hour angle, rad
    latr <- lat / 57.2957795
    sset <- -tan(latr) * tan(delta)
    omegas <- sset * 0
    omegas[abs(sset) <= 1] <- acos(sset[abs(sset) <= 1])
    # correction for high latitudes
    omegas[sset < (-1)] <- max(omegas)
    # Ra, MJ m-2 d-1
    Ra <- 37.6 * dr *
      (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
    Ra <- ifelse(Ra < 0, 0, Ra)
  }

  # 2. Daily ET0, mm day-1
  if (!using$Pre) {
    # Use original Hargreaves (1948)
    ET0 <- 0.0023 * 0.408 * Ra * (Tmean + 17.8) * Tr^0.5
  } else {
    # Use modified method (Droogers and Allen, 2002)
    ab <- Tr - 0.0123 * Pre
    ET0 <- 0.0013 * 0.408 * Ra * (Tmean + 17.0) * ab^0.76
    ET0[is.nan(ab^0.76)] <- 0
  }
  ET0 <- ifelse(ET0 < 0, 0, ET0)

  # Transform ET0 to mm month-1
  ET0 <- ET0 * mlen_array


  ### Format output and return - - - - - - - - - - - - - - - - - - - - - - -

  if (out_type == "tsmatrix") {
    ET0 <- matrix(ET0, nrow = n_times)
    ET0 <- ts(ET0, frequency = ts_freq, start = ts_start)
    colnames(ET0) <- rep("ET0_har", ncol(ET0))
  } else if (out_type == "tsvector") {
    ET0 <- as.vector(ET0)
    ET0 <- ts(ET0, frequency = ts_freq, start = ts_start)
  } else if (out_type == "vector") {
    ET0 <- as.vector(ET0)
  } else if (out_type == "matrix") {
    ET0 <- matrix(ET0, nrow = n_times)
    colnames(ET0) <- rep("ET0_har", ncol(ET0))
  } else { # array, default
    colnames(ET0) <- rep("ET0_har", ncol(ET0))
  }

  dimnames(ET0) <- names

  return(ET0)
}
