spei <- function(x, y, ...) UseMethod("spei")

#' @name Drought-indices
#' @title Calculation of the Standardized Precipitation-Evapotranspiration
#' Index (SPEI) and the Standardized Precipitation Index (SPI).
#' @aliases spei, spi
#' @description
#' Given a time series of the climatic water balance (precipitation minus
#' potential evapotranspiration), gives a time series of the Standardized
#' Precipitation-Evapotranspiration Index (SPEI).
#' @usage
#' spei(
#'  data,
#'  scale,
#'  kernel = list(type = 'rectangular', shift = 0),
#'  distribution = 'log-Logistic',
#'  fit = 'ub-pwm',
#'  na.rm = FALSE,
#'  ref.start=NULL,
#'  ref.end=NULL,
#'  keep.x=FALSE,
#'  params=NULL,
#'  verbose=TRUE,
#'  ...)
#'
#' spi(
#'  data,
#'  scale,
#'  kernel = list(type = 'rectangular', shift = 0),
#'  distribution = 'Gamma',
#'  fit = 'ub-pwm',
#'  na.rm = FALSE,
#'  ref.start=NULL,
#'  ref.end=NULL,
#'  keep.x=FALSE,
#'  params=NULL,
#'  verbose=TRUE,
#'  ...)
#' @param data a vector, matrix or data frame with time ordered values
#' of precipitation (for the SPI) or of the climatic balance
#' precipitation minus potential evapotranspiration (for the SPEI).
#' @param scale an integer, representing the time scale at which
#' the SPEI / SPI will be computed.
#' @param kernel optional, a list defining the type of kernel used
#' for computing the SPEI / SPI at scales higher than one. Defaults
#' to unshifted rectangular kernel.
#' @param distribution optional, name of the distribution function
#' to be used for computing the SPEI / SPI (one of 'log-Logistic',
#' 'Gamma' and 'PearsonIII'). Defaults to 'log-Logistic' for \code{spei},
#' and to 'Gamma' for \code{spi}.
#' @param fit optional, name of the method used for computing the
#' distribution function parameters (one of 'ub-pwm', 'pp-pwm' and
#' 'max-lik'). Defaults to 'ub-pwm'.
#' @param na.rm optional, a logical value indicating whether NA values
#' should be stripped from the computations. Defaults to FALSE, i.e.
#' no NA are allowed in the data.
#' @param ref.start optional, starting point of the reference period
#' used for computing the index. Defaults to NULL, indicating that the
#' first value in data will be used as starting point.
#' @param ref.end optional, ending point of the reference period used
#' for computing the index. Defaults to NULL, indicating that the last
#' value in data will be used as ending point.
#' @param keep.x optional, a logical value indicating whether the data used
#' for fitting the model should be kept. Defaults to FALSE.
#' @param params optional, an array of parameters for computing the
#' spei. This option overrides computation of fitting parameters.
#' @param verbose optional, logical, report the computation options during
#' calculation. Either 'TRUE' (default) or 'FALSE'.
#' @param ... other possible parameters.
#'
#'
#' @details
#' The \code{spei} and \code{spi} functions allow computing the SPEI
#' and the SPI indices. These are climatic proxies widely used for drought
#' quantification and monitoring. Both functions are identical (in fact,
#' \code{spi} is just a wrapper for \code{spei}), but they are kept
#' separated for clarity. Basically, the functions standardize a variable
#' following a log-Logistic (or Gamma, or PearsonIII) distribution function
#' (i.e., they transform it to a standard Gaussian variate with zero mean
#' and standard deviation of one).
#'
#'
#' @section Input data:
#' The input variable is a time ordered series of precipitation values
#' for \code{spi}, or a series of the climatic water balance (precipitation
#' minus potential evapotranspiration) for \code{spei}. When used with the
#' default options, it would yield values of both indices exactly as defined
#' in the references given below.
#'
#' The SPEI and the SPI were defined for monthly data. Since the PDFs of the
#' data are not homogenous from month to month, the data is split into twelve
#' series (one for each month) and independent PDFs are fit to each series. If
#' \code{data} is a vector or a matrix it will be treated as a sequence of
#' monthly values starting in January. If it is a (univariate or multivariate)
#' time series then the function \code{\link{cycle}} will be used to determine
#' the position of each observation within the year (month), allowing the data
#' to start in a month other than January.
#'
#'
#' @section Time scales:
#' An important advantage of the SPEI and the SPI is that they can be computed
#' at different time scales. This way it is possible to incorporate the
#' influence of the past values of the variable in the computation enabling
#' the index to adapt to the memory of the system under study. The magnitude of
#' this memory is controlled by parameter \code{scale}. For example, a value of
#' six would imply that data from the current month and of the past five months
#' will be used for computing the SPEI or SPI value for a given month. By
#' default all past data will have the same weight in computing the index, as
#' it was originally proposed in the references below. Other kernels, however,
#' are available through parameter \code{kernel}. The parameter \code{kernel}
#' is a list defining the shape of the kernel and a time shift. These
#' parameters are then passed to the function \code{\link{kern}}.
#'
#'
#' @section Probability distributions:
#' Following the original definitions \code{spei} uses a log-Logistic
#' distribution by default, and \code{spi} uses a Gamma distribution. This
#' behavior can be modified, however, through parameter \code{distribution}.
#'
#'
#' @section Fitting methods:
#' The default method for parameter fitting is based on unbiased Probability
#' Weighted Moments ('ub-pwm'), but other methods can be used through parameter
#' \code{fit}. A valid alternative is the plotting-position PWM ('pp-pwm')
#' method. For the log-Logistic distribution, also the maximum likelihood
#' method ('max-lik') is available.
#'
#'
#' @section User-provided parameters:
#' An option exists to override parameter fitting and provide user default
#' parameters. This is activated with the parameter \code{params}. The exact
#' values provided to this parameter depend on the distribution function being
#' used. For log-Logistic and PearsonIII it should be a three-dimensional array
#' with dimensions (3,number of series in data,12), containing twelve parameter
#' triads (xi, alpha, kappa) for each data series, one for each month. For
#' Gamma, a three-dimensional array with dimensions (2,number of series
#' in data,12),  containing twelve parameter pairs (alpha, beta). It is a good
#' idea to look at the coefficients slot of a previously fit \code{spei} spei
#' object in order to understand the structure of the parameter array. The
#' parameter \code{distribution} is still used under this option in order to
#' know what distribution function should be used.
#'
#'
#' @section Reference period:
#' The default behavior of the functions is using all the values provided in
#' \code{data} for parameter fitting. However, this can be modified with help
#' of parameters \code{ref.start}  and \code{ref.end}. These parameters allow
#' defining a subset of values that will be used  for parameter fitting, i.e.
#' a reference period. The functions, however, will compute the  values of the
#' indices for the whole data set. For these options to work it is necessary
#' that \code{data} will be a time series object. The starting and ending
#' points of the reference period  will then be defined as pairs of year and
#' month values, e.g. c(1900,1).
#'
#'
#' @section Processing large datasets:
#' It is possible to use the \code{spei} and \code{spi} functions for
#' processing multivariate datasets at once. If a matrix or data frame is
#' supplied as \code{data}, with time series of precipitation or precipitation
#' minus potential evapotranspiration arranged in columns, the result would be
#' a matrix (data frame) of spi or spei series. This makes processing large
#' datasets extremely easy, since no loops need to be used.
#'
#'
#' @return
#' Functions \code{spei} and \code{spi} return an object of class \code{spei}.
#' The generic functions \code{print} and \code{summary} can be used to obtain
#' summaries of the results. The generic accessor functions \code{coefficients}
#' and \code{fitted} extract useful features of the object.
#'
#' An object of class \code{spei} is a list containing at least the
#' following components:
#'
#' \itemize{
#'   \item call: the call to \code{spei} or \code{spi} used to generate the
#'   object.
#'   \item fitted: time series with the values of the Standardized
#'   Precipitation-Evapotranspiration Index (SPEI) or the Standardized
#'   Precipitation Index (SPI). If data consists of several columns the
#'   function will treat each column as independent data, and the result will
#'   be a multivariate time series. The names of the columns in \code{data}
#'   will be used as column names in fitted.
#'   \item coefficients: an array with the values of the coefficients of the
#'   distribution function fitted to the data. The first dimension of the
#'   array contains the three (or two) coefficients, the second dimension will
#'   typically consist of twelve values corresponding to each month, and the
#'   third dimension will be equal to the number of columns (series) in
#'   \code{data}. If a time scale greater than one has been used then the
#'   first elements will have NA value since the kernel can not be applied.
#'   The first element with valid data will be the one corresponding to the
#'   time scale chosen.
#'   \item scale: the \code{scale} parameter used to generate the object.
#'   \item kernel: the parameters and values of the kernel used to generate
#'   the object.
#'   \item distribution: the distribution function used to generate the object.
#'   \item fit: the fitting method used to generate the object.
#'   \item na.action: the value of the na.action parameter used.
#'   \item data: if requested, the input data used.
#' }
#'
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar
#' drought index sensitive
#' to global warming: The Standardized Precipitation Evapotranspiration
#' Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696,
#' DOI: 10.1175/2009JCLI2909.1.
#'
#' S. Beguería, S.M Vicente-Serrano, F. Reig, B. Latorre. 2014. Standardized
#' precipitation evapotranspiration index (SPEI) revisited: parameter fitting,
#' evapotranspiration models, tools, datasets and drought monitoring.
#' \emph{International Journal of Climatology} \bold{34}(10): 3001-3023.
#'
#' \url{http://spei.csic.es}
#'
#'
#' @author Santiago Beguería and Sergio M. Vicente-Serrano. Maintainer:
#' Santiago Beguería.
#'
#'
#' @seealso
#' \code{\link{kern}} for different kernel functions available.
#' \code{\link{thornthwaite}},
#' \code{\link{hargreaves}} and \code{\link{penman}} for ways of calculating
#' potential evapotranspiration.
#' \code{\link{summary.spei}} and \code{\link{print.spei}} for summaries of
#' \code{spei} objects.
#' \code{\link{plot.spei}} for plotting \code{spei} objects.
#'
#'
#' @examples
#' \donttest{
#' # Load data
#' data(wichita)
#'
#' # Compute potential evapotranspiration (PET) and climatic water
#' # balance (BAL).
#' wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
#' wichita$BAL <- wichita$PRCP - wichita$PET
#'
#' # Convert to a ts (time series) for convenience
#' wichita <- ts(wichita[, -c(1, 2)], end = c(2011, 10), frequency = 12)
#' plot(wichita)
#'
#' # One and twelve-months SPEI
#' spei1 <- spei(wichita[, "BAL"], 1)
#' spei12 <- spei(wichita[, "BAL"], 12)
#' class(spei1)
#' plot(spei1)
#' plot(spei12)
#'
#' # Extract information from `spei` object: summary, call function,
#' # fitted values, and coefficients
#' summary(spei1)
#' names(spei1)
#' spei1$call
#' spei1$fitted
#' spei1$coefficients
#'
#' # Plot `spei` object
#' par(mfrow = c(2, 1))
#' plot(spei1, main = "Wichita, SPEI-1")
#' plot(spei12, main = "Wichita, SPEI-12")
#'
#' # One and tvelwe-months SPI
#' spi_1 <- spi(wichita[, "PRCP"], 1)
#' spi_12 <- spi(wichita[, "PRCP"], 12)
#'
#' par(mfrow = c(2, 1))
#' plot(spi_1, "Wichita, SPI-1")
#' plot(spi_12, "Wichita, SPI-12")
#'
#' # Time series not starting in January
#' plot(spei(ts(wichita[, "BAL"], freq = 12, start = c(1980, 6)), 12))
#'
#' # Using a particular reference period (1980-2000) for computing the
#' # parameters. This may result in unexpected values (Inf, NaN) if data
#' # outside the reference period are way higher or lower than those within
#' # the reference period.
#' plot(spei(ts(wichita[, "BAL"], freq = 12, start = c(1980, 6)), 12,
#'   ref.start = c(1980, 1), ref.end = c(2000, 1)
#' ))
#'
#' # Using different kernels
#' spei24 <- spei(wichita[, "BAL"], 24)
#' spei24_gau <- spei(wichita[, "BAL"], 24,
#'   kernel = list(type = "gaussian", shift = 0)
#' )
#' par(mfrow = c(2, 1))
#' plot(spei24)
#' plot(spei24_gau)
#' dev.off()
#'
#' # Using different methods (distributions)
#' spi_gam <- spi(wichita[, "PRCP"], 12, distribution = "Gamma")
#' spi_pe3 <- spi(wichita[, "PRCP"], 12, distribution = "PearsonIII")
#' plot(spi_gam$fitted, spi_pe3$fitted)
#' grid()
#'
#' # Using custom (user provided) parameters
#' coe <- spei1$coefficients
#' dim(coe)
#' spei(wichita[, "BAL"], 1, params = coe)
#'
#' # Matrix input (computing data from several stations at one)
#' # Dataset `balance` contains time series of the climatic water balance at
#' # 12 locations. Note that input must be provided as matrix.
#' data(balance)
#' head(balance)
#' bal_spei12 <- spei(as.matrix(balance), 12)
#' plot(bal_spei12)
#'
#' # 3-d array input (computing data from a gridded spatio-temporal dataset)
#' # Dataset cruts4 contains monthly time series of the climatic water balance
#' # at six locations, in a gridded format (3-d array).
#' data(cruts4)
#' dim(cruts4)
#' spei_12 <- spei(cruts4, 12)
#' dim(spei_12$fitted)
#'
#' # Modding the plot
#' # Since plot.spei() returns a ggplot object, it is possible to add or tweak
#' # parts of the plot.
#' require(ggplot2)
#' plot(spei(wichita[, "BAL"], 12)) +
#'   ggtitle("SPEI1 at Wichita") +
#'   scale_fill_manual(values = c("blue", "red")) + # classic SPEI look
#'   scale_color_manual(values = c("blue", "red")) + # classic SPEI look
#'   theme_classic() +
#'   theme(legend.position = "bottom")
#'}
#'
#' @export
#'
spei <- function(data, scale, kernel = list(type = "rectangular", shift = 0),
                 distribution = "log-Logistic", fit = "ub-pwm", na.rm = FALSE,
                 ref.start = NULL, ref.end = NULL, keep.x = FALSE, params = NULL,
                 verbose = TRUE, ...) {
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Determine which combinations of inputs were passed and check their
  # validity, and check that all the inputs have the same dimensions

  # Instantiate two objects to collect errors and warnings
  check <- makeAssertCollection()
  warn <- makeAssertCollection()

  # A list of computation options
  using <- list(
    na.rm = FALSE, ref.start = FALSE, ref.end = FALSE, keep.x = FALSE,
    params = FALSE
  )

  # Check compulsory inputs

  if (!is.numeric(scale)) {
    check$push("Argument `scale` must be numeric.")
  } else if (length(scale) != 1) {
    check$push("Argument `scale` must be a single value")
  } else {
    warn$push(paste0(
      "Calculating the Standardized Precipitation ",
      "Evapotranspiration Index (SPEI) at a time scale of ",
      scale, "."
    ))
  }

  # Check optional inputs

  if (!is.list(kernel)) {
    check$push("Argument `kernel` must be a list.")
  } else if (length(kernel) != 2 | names(kernel)[1] != "type" |
    names(kernel)[2] != "shift") {
    check$push("Argument `kernel` must be a list with components `type` and `shift`.")
  } else if (!inherits(kernel$type, "character") | length(kernel$type) != 1) {
    check$push("Element `type` of `kernel` must be a single valued character vector")
  } else if (!inherits(kernel$shift, "numeric") | length(kernel$shift) != 1) {
    check$push("Element `shift` of `kernel` must be a single valued numeric.")
  } else {
    warn$push(paste0(
      "Using kernel type '", kernel$type, "'", ", with ",
      kernel$shift, " shift."
    ))
  }

  if (is.null(params)) {
    # fit distribution
    if (!is.character(distribution) | length(distribution) != 1 |
      !distribution %in% c("log-Logistic", "Gamma", "PearsonIII")) {
      check$push(paste0(
        "Argument `distribution` must be one of `log-Logistic`,",
        " `Gamma` or `PearsonIII`."
      ))
    } else {
      warn$push(paste0("Fitting the data to a ", distribution, " distribution."))
    }
    if (!is.character(fit) | length(fit) != 1 |
      !fit %in% c("ub-pwm", "pp-pwm", "max-lik")) {
      check$push(paste0(
        "Argument `fit` must be one of `ub-pwm`, `pp-pwm` ",
        "or `max-lik`."
      ))
    } else {
      warn$push(paste0("Using the ", fit, " parameter fitting method."))
    }
  } else {
    # do not fit distribution; note that additional checks must be performed to
    # guarantee that the user-provided object conforms to what is expected
    using$params <- TRUE
    if (!is.character(distribution) | length(distribution) != 1 |
      !distribution %in% c("log-Logistic", "Gamma", "PearsonIII")) {
      check$push(paste0(
        "Argument `distribution` must be one of `log-Logistic`,",
        " `Gamma` or `PearsonIII`."
      ))
    } else {
      warn$push(paste0(
        "Using the ", distribution, " distribution with ",
        "user-specified distribution parameters."
      ))
    }
  }

  if (!is.logical(na.rm)) {
    check$push("Argument `na.rm` must be set to either TRUE or FALSE.")
  } else if (na.rm) {
    warn$push("Missing values (`NA`) will not be considered in the calculation.")
  } else {
    using$na.rm <- TRUE
    warn$push("Checking for missing values (`NA`): all the data must be complete.")
  }

  # note: additional checks must be performed on both ref to see they are
  # within the data limits
  if (!is.null(ref.start) | !is.null(ref.end)) {
    if (!is.null(ref.start)) {
      if (!is.numeric(ref.start) | length(ref.start) != 2) {
        check$push("Argument `ref.start` must be a numeric vector of length two.")
      } else {
        using$ref.start <- TRUE
      }
    }
    if (!is.null(ref.end)) {
      if (!is.null(ref.end) && (!is.numeric(ref.end) | length(ref.end) != 2)) {
        check$push("Argument `ref.end` must be a numeric vector of length two.")
      } else {
        using$ref.end <- TRUE
      }
    }
    warn$push("Using a user-specified reference period.")
  } else {
    warn$push("Using the whole time series as reference period.")
  }

  if (!is.logical(keep.x)) {
    check$push("Argument `keep.x` must be set to either TRUE or FALSE.")
  } else if (keep.x) {
    using$keep.x <- TRUE
    warn$push("Storing the input data in the returned spei object.")
  }

  if (!is.logical(verbose)) {
    check$push("Argument `verbose` must be set to either TRUE or FALSE.")
  }

  # Check for missing values in inputs
  if (!na.rm && anyNA(data)) {
    check$push("`data` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  # Determine input dimensions and compute internal dimensions (int_dims)
  data_dims <- dim(data)
  if (is.null(data_dims) || length(data_dims) == 1) {
    # vector input (single-site)
    int_dims <- c(length(data), 1, 1)
  } else if (length(data_dims) == 2) {
    # matrix input (multi-site)
    int_dims <- c(data_dims, 1)
  } else if (length(data_dims) == 3) {
    # 3D array input (gridded data)
    int_dims <- data_dims
  } else {
    int_dims <- data_dims
    check$push("Input data can not have more than three dimensions.")
  }
  n_sites <- prod(int_dims[[2]], int_dims[[3]])
  n_times <- int_dims[[1]]
  input_len <- prod(int_dims)

  # Determine input data shape
  if (is.ts(data)) {
    if (is.matrix(data)) {
      out_type <- "tsmatrix"
    } else {
      out_type <- "tsvector"
    }
  } else if (is.vector(data)) {
    out_type <- "vector"
  } else if (is.matrix(data)) {
    out_type <- "matrix"
  } else if (is.array(data)) {
    out_type <- "array"
  } else {
    check$push("Bad data type: input must be a vector, tsvector, matrix, tsmatrix, or 3-d array.")
    out_type <- NULL
  }
  warn$push(paste0("Input type is ", out_type, "."))

  # Determine time properties
  if (is.ts(data)) {
    ts_freq <- frequency(data)
    ts_start <- start(data)
    ts_end <- end(data)
    ym <- as.yearmon(time(data))
    warn$push(paste0(
      "Time series spanning ", ym[1], " to ", ym[n_times],
      ", with frequency = ", ts_freq, "."
    ))
  } else {
    ts_freq <- 12
    ts_start <- 1
    ts_end <- n_times
    warn$push("No time information provided, assuming a monthly time series.")
  }

  # Verify the dimensions of the parameters array
  dim_params <- ifelse(distribution == "Gamma", 2, 3)
  if (using$params) {
    if (dim(params)[1] != dim_params | dim(params)[2] != n_sites |
      dim(params)[3] != ts_freq) {
      check$push(paste0(
        "Parameters array should have dimensions (",
        dim_params, ", ", n_sites, ", ", ts_freq, ")"
      ))
    }
  }

  # Return errors and halt execution (if any)
  if (!check$isEmpty()) {
    stop(paste(check$getMessages(), collapse = " "))
  }

  # Show a warning with computation options
  if (verbose) {
    print(paste(warn$getMessages(), collapse = " "))
  }


  ### Computation of SPEI - - - - - - - - - - - - - - - - - - - - - - - - -

  # Instantiate an object to store the distribution coefficients
  # ADD PZE TO GAMMA AND PEARSONIII
  coef <- switch(distribution,
    "Gamma" = array(
      NA, c(2, n_sites, ts_freq),
      list(
        par = c("alpha", "beta"), colnames(data),
        NULL
      )
    ),
    "PearsonIII" = coef <- array(
      NA, c(3, n_sites, ts_freq),
      list(
        par = c("mu", "sigma", "gamma"),
        colnames(data), NULL
      )
    ),
    "log-Logistic" = array(
      NA, c(3, n_sites, ts_freq),
      list(
        par = c("xi", "alpha", "kappa"),
        colnames(data), NULL
      )
    ),
    "GEV" = array(
      NA, c(3, n_sites, ts_freq),
      list(
        par = c("xi", "alpha", "kappa"),
        colnames(data), NULL
      )
    )
  )

  # Create uniformly-dimensioned ts-matrices from input for internal use (acu)
  if (out_type == "vector" | out_type == "tsvector") {
    acu <- as.matrix(data)
  } else if (out_type == "matrix" | out_type == "tsmatrix") {
    acu <- data
  } else if (out_type == "array") {
    acu <- matrix(data, ncol = n_sites)
  } else {
    stop("There was an error while creating `acu`. Please, report the bug.")
  }

  # Apply rolling (weighted) sum if scale > 1
  if (scale > 1) {
    wgt <- kern(scale, kernel$type, kernel$shift) * scale
    acu <- rollapply(acu, scale,
      fill = NA, FUN = function(x) sum(x * rev(wgt)),
      align = "right"
    )
  }

  # Convert to time series
  if (!is.ts(acu)) {
    acu <- ts(acu, start = ts_start, frequency = ts_freq)
  }

  # Trim data set to reference period for fitting (acu.ref)
  if (using$ref.start | using$ref.end) {
    acu.ref <- suppressWarnings(window(acu, ref.start, ref.end))
  } else {
    acu.ref <- acu
  }

  # Instantiate an object to store the standardized data
  spei <- acu * NA

  # Loop through series (columns in data)
  for (s in 1:n_sites) {
    x <- acu[, s]
    x.ref <- acu.ref[, s]

    # Loop through the months or whatever time period used
    for (c in (1:ts_freq)) {
      # Filter month m, excluding NAs (x.mon)
      f <- which(cycle(x.ref) == c)
      f <- f[!is.na(x.ref[f])]
      ff <- which(cycle(x) == c)
      ff <- ff[!is.na(x[ff])]
      x.mon <- x.ref[f]

      # Escape if there are no data
      if (length(x.mon) == 0) {
        spei[f] <- NA
        next()
      }

      # Probability of zero (pze)
      if (distribution != "log-Logistic") {
        pze <- sum(x.mon == 0) / length(x.mon)
        x.mon <- x.mon[x.mon > 0]
      }

      ## Compute coefficients - - - - - - - - - - - - - -

      # Distribution parameters (f_params)
      if (!using$params) {
        # Fit distribution parameters
        x.mon_sd <- sd(x.mon, na.rm = TRUE)

        # Early stopping
        if (is.na(x.mon_sd) || (x.mon_sd == 0)) {
          spei[f] <- NA
          next()
        }
        if (length(x.mon) < 4) {
          spei[ff, s] <- NA
          coef[, s, c] <- NA
          next()
        }

        # Calculate probability weighted moments based on `lmomco` or
        # `TLMoments`
        pwm <- switch(fit,
          "pp-pwm" = pwm.pp(x.mon, -0.35, 0, nmom = 3, sort = TRUE),
          "ub-pwm" = PWM(x.mon, order = 0:2)
        )

        # Check L-moments validity
        lmom <- pwm2lmom(pwm)
        if (!are.lmom.valid(lmom) || anyNA(lmom[[1]]) ||
          any(is.nan(lmom[[1]]))) {
          next()
        }

        # `lmom` fortran functions need specific inputs L1, L2, T3
        # This is handled internally by `lmomco` with `lmorph`
        fortran_vec <- c(lmom$lambdas[1:2], lmom$ratios[3])

        # Calculate parameters based on distribution with `lmom`, then `lmomco`
        f_params <- switch(distribution,
          "log-Logistic" = tryCatch(pelglo(fortran_vec),
            error = function(e) {
              parglo(lmom)$para
            }
          ),
          "Gamma" = tryCatch(pelgam(fortran_vec),
            error = function(e) {
              pargam(lmom)$para
            }
          ),
          "PearsonIII" = tryCatch(pelpe3(fortran_vec),
            error = function(e) {
              parpe3(lmom)$para
            }
          )
        )

        # Adjust if user chose `log-Logistic` and `max-lik`
        if (distribution == "log-Logistic" && fit == "max-lik") {
          f_params <- parglo.maxlik(x.mon, f_params)$para
        }
      } else {
        # User-provided distribution parameters

        f_params <- as.vector(params[, s, c])
      }

      # Store the coefficients
      coef[, s, c] <- f_params

      ## Standardize - - - - - - - - - - - - - -

      # Calculate CDF on `x` using `f_params`
      cdf_res <- switch(distribution,
        "log-Logistic" = lmom::cdfglo(x[ff], f_params),
        "Gamma" = lmom::cdfgam(x[ff], f_params),
        "PearsonIII" = lmom::cdfpe3(x[ff], f_params)
      )
      # Adjust for `pze` if distribution is Gamma or PearsonIII
      if (distribution == "Gamma" | distribution == "PearsonIII") {
        spei[ff, s] <- qnorm(pze + (1 - pze) * pnorm(spei[ff, s]))
      }

      # Store the standardized values
      spei[ff, s] <- qnorm(cdf_res)
    } # next c (month)
  } # next s (series)


  ### Format output and return - - - - - - - - - - - - - - - - - - - - - - -

  if (out_type == "tsmatrix") {
  } else if (out_type == "tsvector") {
    spei <- as.vector(spei)
    spei <- ts(spei, frequency = ts_freq, start = ts_start)
  } else if (out_type == "vector") {
    spei <- as.vector(spei)
  } else if (out_type == "matrix") {
    spei <- matrix(spei, nrow = n_times, dimnames = list(NULL, colnames(spei)))
  } else { # array
    spei <- array(spei, dim = int_dims, dimnames = dimnames(data))
  }

  z <- list(
    call = match.call(expand.dots = FALSE),
    info = paste(warn$getMessages(), collapse = " "),
    fitted = spei,
    coefficients = coef,
    scale = scale,
    kernel = list(
      type = kernel$type, shift = kernel$shift,
      values = kern(scale, kernel$type, kernel$shift)
    ),
    distribution = distribution,
    fit = fit,
    na.action = na.rm
  )
  if (using$ref.start | using$ref.end) {
    z$ref.period <-
      rbind(ref.start, ref.end)
  }
  if (using$keep.x) z$data <- data

  class(z) <- "spei"
  return(z)
}


#' @name Generic-methods-for-spei-objects
#'
#' @title Generic methods for \code{spei} objects.
#'
#' @aliases plot.spei summary.spei
#'
#' @description
#' Generic methods for extracting information and plotting \code{spei} objects.
#'
#' @usage
#' \method{print}{spei}(x, ...)
#' \method{summary}{spei}(object, ...)
#' \method{plot}{spei}(x, ...)
#'
#' @param x an object of class \code{spei}.
#' @param object an object of class \code{spei}.
#' @param ... additional parameters, not used at present.
#'
#'
#' @details These functions allow extracting information and plotting
#' \code{spei} objects. \code{print} yields the fitted values, i.e. a time
#' series of SPEI or SPI values. \code{summary} reports the function call,
#' the parameters of the PDF used, and the time series of SPEI or SPI values.
#' \code{plot} produces a plot of the time series of SPEI or SPI values, with
#' blue and red colors for positive and negative values, respectively. If a
#' reference period was used in the function call it is shown by a shaded area.
#' In the event that NA or Inf values were produced, these are shown by
#' circles.
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar
#' drought index sensitive to global warming: The Standardized Precipitation
#' Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696,
#' DOI: 10.1175/2009JCLI2909.1.
#'
#'
#' @author Santiago Beguería
#'
#' @examples
#' # See examples of use in the help page of the spei() function.
#'
#' @export
#'
print.spei <- function(x, ...) {
  print(x$fitted)
}

#'
#' @title summary of spei/spi
#' @description See print.spei
#' @details See print.spei
#' @rdname Generic-methods-for-spei-objects
#' @export
#'
summary.spei <- function(object, ...) {
  x <- object
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  for (i in 1:dim(x$coeff)[2]) {
    cat("\t", dimnames(x$coeff)[[2]][i], ":\n", sep = "")
    tab <- cbind(t(x$coeff[, i, ]))
    rownames(tab) <- 1:dim(x$coeff)[3]
    print(tab)
    cat("\nFitted:\n")
    print(x$fitted)
  }
}

#'
#' @title plot spei/spi
#' @description See print.spei
#' @details See print.spei
#' @rdname Generic-methods-for-spei-objects
#'
#' @import ggplot2
#' @importFrom zoo na.trim
#' @importFrom reshape melt
#' @importFrom graphics abline grid lines par plot points polygon
#'
#' @export
#'
plot.spei <- function(x, ...) {
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -

  if (!inherits(x, "spei")) {
    stop("Data must be an `spei` object resulting from a call to spi() or spei() functions.")
  }


  ### Make the plot - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # A workaround to avoid R CMD check warning about no visible binding for
  # global variables
  # utils::globalVariables(na, value)

  # Label
  if (grepl("spei", x$call[1])) {
    label <- "SPEI (z-values)"
  } else if (grepl("spi", x$call[1])) {
    label <- "SPI (z-values)"
  } else {
    stop("Label could not be determined. Please, report the error.")
  }

  # Extract the data
  data <- x$fitted

  # Determine input data shape
  if (is.ts(data)) {
    if (is.matrix(data)) {
      out_type <- "tsmatrix"
    } else {
      out_type <- "tsvector"
    }
  } else if (is.vector(data)) {
    out_type <- "vector"
  } else if (is.matrix(data)) {
    out_type <- "matrix"
  } else if (is.array(data)) {
    out_type <- "array"
  }

  # Determine time properties
  if (is.ts(data)) {
    ts_freq <- frequency(data)
    ts_start <- start(data)
  } else {
    ts_freq <- 12
    ts_start <- 1
  }

  # Create uniformly-dimensioned ts-matrices from input for internal use (acu)
  if (out_type == "vector" | out_type == "tsvector") {
    data <- as.matrix(data)
  } else if (out_type == "matrix" | out_type == "tsmatrix") {
    data <- data
  } else if (out_type == "array") {
    data <- matrix(data, ncol = dim(data)[2] * dim(data)[3])
  }

  # Check on the dimensions; default max. is 10 sites
  if (ncol(data) > 10) {
    data <- data[, 1:10]
    warning("Maximum allowed sites is ten. Plotting the first ten sites in the data.")
  }

  # Convert to time series
  if (!is.ts(data)) {
    data <- ts(data, start = ts_start, frequency = ts_freq)
  }

  # Determine reference period
  if (!is.null(x$ref.period)) {
    if (grepl("ref.start", paste(row.names(x$ref.period), collapse = " "))) {
      ref1 <- x$ref.period["ref.start", 1] + (x$ref.period["ref.start", 2] - 0.5) /
        12
    } else {
      ref1 <- start(data)[1] + (start(data)[2] - 0.5) / 12
    }
    if (grepl("ref.end", paste(row.names(x$ref.period), collapse = " "))) {
      ref2 <- x$ref.period["ref.end", 1] + (x$ref.period["ref.end", 2] - 0.5) / 12
    } else {
      ref2 <- end(data)[1] + (end(data)[2] - 0.5) / 12
    }
  } else {
    ref1 <- ref2 <- NULL
  }

  # Remove leading / ending NAs
  data <- na.trim(data)

  # Melt
  kk <- as.data.frame(data)
  kk$time <- as.character(time(data))
  kk <- melt(kk, id.vars = "time")
  kk$time <- as.numeric(kk$time)

  # Add NAs
  kk$na <- as.numeric(ifelse(is.na(kk$value), 0, NA))

  # Add SPI / SPEI categories
  kk$cat <- ifelse(kk$value > 0, "neg", "pos")

  # To do: cut the plot horizontally by drought classes; can be done with
  # stacked bars, using geom_bar(stat='identity')
  # kk$cat <- cut(kk$value, breaks=c(-Inf, -2, -1.5, -1, -0.5, 0, 0.5, 1,
  #     1.5, 2, Inf))
  # # go class by class
  # w <- which(kk$cat == '(-1,-0.5]')
  # kk <- rbind(kk, kk[w,])
  # kk$value[w] <- -0.5
  # kk$cat[w] <- '(-0.5,0]'
  # #
  # w <- which(kk$cat == '(-1,-0.5]')
  # kk <- rbind(kk, kk[w,])
  # kk$value[w] <- -0.5
  # kk$cat[w] <- '(-0.5,0]'

  # Plot it
  g <- ggplot(kk, aes(.data[["time"]], .data[["value"]],
    fill = "cat",
    color = "cat"
  ))
  # reference period (if different than whole series)
  if (!is.null(x$ref.period)) {
    g <- g +
      annotate("rect", xmin = ref1, xmax = ref2, ymin = -Inf, ymax = Inf, alpha = 0.2) +
      geom_vline(xintercept = c(ref1, ref2), color = "grey", alpha = 0.4)
  }
  # add the bars with the SPEI values
  g <- g +
    geom_bar(stat = "identity") + # color='white' helps separate between values
    #    scale_fill_manual(values=c('blue','red')) +  # classic SPEI look
    #    scale_color_manual(values=c('blue','red')) + # classic SPEI look
    scale_fill_manual(values = c("cyan3", "tomato")) + # new look
    scale_color_manual(values = c("cyan3", "tomato")) # new look
  # add NAs
  g <- g +
    geom_point(aes(.data[["time"]], .data[["na"]]),
      shape = 21, fill = "white",
      color = "black"
    )
  # add other parts and options
  g <- g +
    geom_hline(yintercept = 0, color = "grey") +
    facet_wrap(~variable, ncol = 1) +
    scale_y_continuous(breaks = seq(-2, 2, 0.5)) +
    ylab(label) +
    xlab("Time") +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0)
    )

  return(g)
}
