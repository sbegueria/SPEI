
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
#'  start=NULL,
#'  frequency=NULL,
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
#'  start=NULL,
#'  frequency=NULL,
#'  ...)
#' @param data a vector, matrix (one series per column), 3-d array (time x
#' longitude x latitude) or time series (\code{ts}) with time ordered values
#' of precipitation (for the SPI) or of the climatic balance
#' precipitation minus potential evapotranspiration (for the SPEI).
#' @param scale a positive integer, representing the time scale at which
#' the SPEI / SPI will be computed. It can not be larger than the length of
#' the series.
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
#' calculation. Either 'TRUE' (default) or 'FALSE'. The report is issued as
#' a message, so it can also be silenced with \code{suppressMessages()}.
#' @param start optional, the time of the first observation of \code{data},
#' when \code{data} is not a time series: a single year, or a vector
#' \code{c(year, period)} with the year and the period within the year
#' (e.g. the month), as in \code{\link{ts}}. Defaults to NULL, i.e. the data
#' start in the first period of year 1 (but see Time information for 3-d
#' arrays with time dimnames). Can not be used if \code{data} is a time
#' series. See Time information.
#' @param frequency optional, the number of periods per year of \code{data}
#' (a positive integer, e.g. 12 for monthly, 48 or 52 for weekly data), when
#' \code{data} is not a time series, as in \code{\link{ts}}. Defaults to
#' NULL, i.e. 12 (monthly data). Can not be used if \code{data} is a time
#' series. See Time information.
#' @param ... not used.
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
#' \code{data} is a (univariate or multivariate) time series then the
#' function \code{\link{cycle}} will be used to determine the position of
#' each observation within the year (month), allowing the data to start in a
#' month other than January, and to have other frequencies (e.g. weekly
#' data). See Time information for data that are not time series.
#'
#'
#' @section Time information:
#' The position of each observation within the year, and the dates of the
#' reference period, come from the time attributes of \code{data} when it
#' is a time series (\code{ts}). Vectors, matrices and 3-d arrays have no
#' time attributes; their time information can be given with the arguments
#' \code{start} and \code{frequency}, which have the same meaning as in
#' \code{\link{ts}}. A vector or a matrix with \code{start} and/or
#' \code{frequency} is equivalent to \code{ts(data, start = start,
#' frequency = frequency)}, and the result is a time series. A 3-d array
#' (which can't be a time series) keeps its shape, and the result is a 3-d
#' array whose time (first) \code{dimnames} label each period: "YYYY-MM" for
#' monthly data (frequency 12), "YYYY" for annual data (frequency 1) and
#' "YYYY-Pnn" otherwise, where nn is the period within the year,
#' zero-padded (e.g. "1961-P01" to "1961-P48" for frequency 48). The
#' other \code{dimnames} of \code{data} are kept. Since these labels are
#' \code{dimnames}, they are kept when the array is subset, e.g.
#' \code{x$fitted["2019-12", , ]}.
#'
#' If \code{start} and \code{frequency} are not given and \code{data} is a
#' 3-d array whose time \code{dimnames} are the consecutive months of a
#' monthly series, as "YYYY-MM" or "YYYY-MM-DD" (any day of the month, e.g.
#' the mid-month dates of many NetCDF files), the start of the series is
#' taken from them, with a frequency of 12 (this is reported in the
#' message issued when \code{verbose = TRUE}); the \code{dimnames} of the
#' result are those of \code{data}.
#'
#' Otherwise (no \code{start}, \code{frequency} or time \code{dimnames}),
#' the data are treated as a sequence of monthly values starting in January
#' of year 1, as in previous versions of the package.
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
#' The Gamma and PearsonIII distributions are fitted to positive values
#' only, and zeros are accounted for through the probability of zero (pze).
#' They are meant for non-negative data such as precipitation; negative
#' values (e.g. in a climatic water balance) produce a warning and get the
#' lowest possible index value.
#'
#'
#' @section Fitting methods:
#' The default method for parameter fitting is based on unbiased Probability
#' Weighted Moments ('ub-pwm'), but other methods can be used through parameter
#' \code{fit}. A valid alternative is the plotting-position PWM ('pp-pwm')
#' method. For the log-Logistic distribution, also the maximum likelihood
#' method ('max-lik') is available, using the 'ub-pwm' estimates as starting
#' values. For the Gamma and PearsonIII distributions 'max-lik' is not
#' available, and 'ub-pwm' is used instead (with a warning).
#'
#'
#' @section User-provided parameters:
#' An option exists to override parameter fitting and provide user default
#' parameters. This is activated with the parameter \code{params}. The exact
#' values provided to this parameter depend on the distribution function being
#' used. For log-Logistic it should be a three-dimensional array
#' with dimensions (3,number of series in data,12), containing twelve parameter
#' triads (xi, alpha, kappa) for each data series, one for each month. For
#' PearsonIII it should have dimensions (4,number of series in data,12),
#' containing the parameters (mu, sigma, gamma) and the probability of zero
#' (pze) estimated in the reference period. For Gamma, it should have
#' dimensions (3,number of series in data,12), containing the parameters
#' (alpha, beta) and pze. Arrays without the pze row (as produced by SPEI
#' versions before 1.9) are still accepted, but then pze is estimated from
#' \code{data} within the reference period, with a warning. It is a good
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
#' indices for the whole data set. The starting and ending points of the
#' reference period are defined as pairs of year and period (e.g. month)
#' values, e.g. c(1900,1), in the time of \code{data}: that of the time
#' series, or the one given by \code{start} and \code{frequency} (see Time
#' information). Without time information, the years are counted from year
#' 1 at the start of \code{data}. If \code{ref.start} or \code{ref.end} fall
#' outside the time span of \code{data} a warning is issued, and the
#' reference period is limited to the data (e.g. a \code{ref.start} before
#' the first observation is taken as the first observation).
#'
#'
#' @section Processing large datasets:
#' It is possible to use the \code{spei} and \code{spi} functions for
#' processing multivariate datasets at once. If a matrix or data frame is
#' supplied as \code{data}, with time series of precipitation or precipitation
#' minus potential evapotranspiration arranged in columns, the result would be
#' a matrix (data frame) of spi or spei series. Gridded data can be given as
#' a 3-d array (time x longitude x latitude), and the result is an array of
#' the same dimensions. This makes processing large datasets extremely easy,
#' since no loops need to be used.
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
#'   will be used as column names in fitted. The result has the same form
#'   as \code{data} (vector, matrix, time series or 3-d array), except that
#'   a vector or a matrix given with \code{start} or \code{frequency}
#'   gives a time series. For a 3-d array with \code{start} or
#'   \code{frequency}, the time \code{dimnames} label each period (see Time
#'   information).
#'   \item coefficients: an array with the values of the coefficients of the
#'   distribution function fitted to the data. The first dimension of the
#'   array contains the coefficients (xi, alpha, kappa for log-Logistic;
#'   alpha, beta, pze for Gamma; mu, sigma, gamma, pze for PearsonIII, where
#'   pze is the probability of zero in the reference period), the second
#'   dimension will be equal to the number of series in \code{data}, and the
#'   third dimension will typically consist of twelve values corresponding to
#'   each month (or the frequency of \code{data}).
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
#' \url{https://spei.csic.es}
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
#' \code{\link[=plot.spei]{plot}} for plotting \code{spei} objects.
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
#' plot(spi_1, main = "Wichita, SPI-1")
#' plot(spi_12, main = "Wichita, SPI-12")
#'
#' # Time series not starting in January
#' plot(spei(ts(wichita[, "BAL"], freq = 12, start = c(1980, 6)), 12))
#'
#' # Using a particular reference period (June 1980 to January 2000) for
#' # computing the parameters. This may result in unexpected values (Inf,
#' # NaN) if data outside the reference period are way higher or lower than
#' # those within the reference period. A reference period outside the time
#' # span of the data gives a warning.
#' plot(spei(ts(wichita[, "BAL"], freq = 12, start = c(1980, 6)), 12,
#'   ref.start = c(1980, 6), ref.end = c(2000, 1)
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
#' # The data start in January 1900: with `start`, the reference period can
#' # be given in calendar years, and the time dimension of the result is
#' # labelled
#' spei_12 <- spei(cruts4, 12,
#'   start = c(1900, 1),
#'   ref.start = c(1961, 1), ref.end = c(1990, 12)
#' )
#' spei_12$fitted["2019-12", , ]
#'
#' # Non-monthly data, e.g. weekly (48 periods per year), with `frequency`
#' spei_4w <- spei(cruts4[1:480, , ], 4, start = c(2000, 1), frequency = 48)
#' dim(spei_4w$coefficients)
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
                 verbose = TRUE, start = NULL, frequency = NULL, ...) {
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
    check$push("Argument `scale` must be a single value.")
  } else if (is.na(scale) || scale < 1 || scale %% 1 != 0) {
    check$push("Argument `scale` must be a positive integer.")
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
  } else if (length(kernel) != 2 || is.null(names(kernel)) ||
    !setequal(names(kernel), c("type", "shift"))) {
    check$push("Argument `kernel` must be a list with components `type` and `shift`.")
  } else if (!is.character(kernel$type) || length(kernel$type) != 1) {
    check$push("Element `type` of `kernel` must be a single valued character vector.")
  } else if (!is.numeric(kernel$shift) || length(kernel$shift) != 1) {
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
    } else if (fit == "max-lik" && distribution != "log-Logistic") {
      warning(
        "Fitting method `max-lik` is only available for the log-Logistic ",
        "distribution; using `ub-pwm` instead.",
        call. = FALSE
      )
      fit <- "ub-pwm"
      warn$push(paste0("Using the ", fit, " parameter fitting method."))
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

  if (!is_flag(na.rm)) {
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

  if (!is_flag(keep.x)) {
    check$push("Argument `keep.x` must be set to either TRUE or FALSE.")
  } else if (keep.x) {
    using$keep.x <- TRUE
    warn$push("Storing the input data in the returned spei object.")
  }

  if (!is_flag(verbose)) {
    check$push("Argument `verbose` must be set to either TRUE or FALSE.")
  }

  # Time information for data that are not a time series (`start`,
  # `frequency`, with the meaning of `ts()`). A vector or a matrix is
  # converted to a time series; a 3-d array keeps its shape, and gets the
  # time information internally.
  using_start <- !is.null(start) || !is.null(frequency)
  if (using_start) {
    if (is.ts(data)) {
      check$push(paste(
        "Arguments `start` and `frequency` can not be used when `data` is",
        "a time series (`ts`), which already has its own time attributes."
      ))
      using_start <- FALSE
    } else if (!check_start(start, frequency, check)) {
      using_start <- FALSE
    } else {
      if (is.null(start)) start <- c(1, 1)
      if (is.null(frequency)) frequency <- 12
      if (is.numeric(data) && (is.vector(data) || is.matrix(data))) {
        data <- ts(data, start = start, frequency = frequency)
        using_start <- FALSE
      }
    }
  }

  # Check for missing values in inputs
  if (isFALSE(na.rm) && anyNA(data)) {
    check$push("`data` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  # Gamma and PearsonIII are fitted to positive values only
  if (distribution %in% c("Gamma", "PearsonIII") && is.numeric(data) &&
    any(data < 0, na.rm = TRUE)) {
    warning(
      "`data` contains negative values, but the ", distribution,
      " distribution is fitted to positive values only (zeros are handled ",
      "through the probability of zero). Negative values get the lowest ",
      "possible index value. For data that can be negative, such as the ",
      "climatic water balance, use the log-Logistic distribution."
    )
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
  if (is.numeric(scale) && length(scale) == 1 && !is.na(scale) &&
    scale > n_times) {
    check$push("Argument `scale` can not be larger than the length of the series.")
  }

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

  # Determine time properties. A 3-d array can't be a time series: its time
  # information comes from `start` and `frequency` or, if they are not
  # given, from its time dimnames (only monthly labels, "YYYY-MM" or
  # "YYYY-MM-DD", one per month)
  time_inferred <- FALSE
  if (identical(out_type, "array") && !using_start) {
    inferred <- parse_time_labels(dimnames(data)[[1]], 12)
    if (!is.null(inferred)) {
      start <- inferred
      frequency <- 12
      using_start <- TRUE
      time_inferred <- TRUE
    }
  }
  if (is.ts(data) || using_start) {
    tt <- if (is.ts(data)) {
      data
    } else {
      ts(seq_len(n_times), start = start, frequency = frequency)
    }
    ts_freq <- stats::frequency(tt)
    ts_start <- stats::start(tt)
    if (ts_freq %% 1 != 0) {
      check$push(paste0(
        "The frequency of `data` must be an integer number of periods ",
        "per year (", ts_freq, " was given)."
      ))
    }
    if (time_inferred) {
      warn$push(paste(
        "Time information taken from the time dimnames of `data`",
        "(a monthly series)."
      ))
    }
    ym <- as.yearmon(time(tt))
    warn$push(paste0(
      "Time series spanning ", ym[1], " to ", ym[n_times],
      ", with frequency = ", ts_freq, "."
    ))
  } else {
    ts_freq <- 12
    ts_start <- 1
    warn$push("No time information provided, assuming a monthly time series.")
  }

  # Verify the dimensions of the parameters array
  # (Gamma and PearsonIII carry an extra row with the probability of zero,
  # pze; arrays without it, from SPEI < 1.9, are still accepted)
  dim_params <- ifelse(distribution == "Gamma", 2, 3)
  has_pze <- distribution %in% c("Gamma", "PearsonIII")
  dim_coef <- dim_params + has_pze
  params_pze <- FALSE
  if (using$params) {
    if (!is.array(params) || length(dim(params)) != 3) {
      check$push(paste0(
        "Argument `params` must be a 3-d array with dimensions (",
        dim_coef, ", ", n_sites, ", ", ts_freq, "), as returned in the ",
        "`coefficients` element of a previous call."
      ))
    } else if (!(dim(params)[1] %in% c(dim_params, dim_coef)) |
      dim(params)[2] != n_sites | dim(params)[3] != ts_freq) {
      check$push(paste0(
        "Parameters array should have dimensions (",
        dim_coef, ", ", n_sites, ", ", ts_freq, ")"
      ))
    } else if (has_pze) {
      params_pze <- dim(params)[1] == dim_coef
      if (!params_pze) {
        warning(
          "Argument `params` has no row for the probability of zero (pze), ",
          "as produced by SPEI < 1.9. pze will be estimated from `data` ",
          "within the reference period.",
          call. = FALSE
        )
      }
    }
  }

  # Return errors and halt execution (if any)
  if (!check$isEmpty()) {
    stop(paste(check$getMessages(), collapse = " "))
  }

  # Report the computation options
  if (verbose) {
    message(paste(warn$getMessages(), collapse = " "))
  }


  ### Computation of SPEI - - - - - - - - - - - - - - - - - - - - - - - - -

  # Series names: column names only make sense for matrix input; for 3-d
  # arrays the series are all the cells of the grid
  site_names <- if (length(int_dims) == 3 && int_dims[[3]] == 1) {
    colnames(data)
  } else {
    NULL
  }

  # Instantiate an object to store the distribution coefficients
  # (Gamma and PearsonIII include the probability of zero, pze)
  coef <- switch(distribution,
    "Gamma" = array(
      NA, c(3, n_sites, ts_freq),
      list(
        par = c("alpha", "beta", "pze"), site_names,
        NULL
      )
    ),
    "PearsonIII" = array(
      NA, c(4, n_sites, ts_freq),
      list(
        par = c("mu", "sigma", "gamma", "pze"),
        site_names, NULL
      )
    ),
    "log-Logistic" = array(
      NA, c(3, n_sites, ts_freq),
      list(
        par = c("xi", "alpha", "kappa"),
        site_names, NULL
      )
    ),
    "GEV" = array(
      NA, c(3, n_sites, ts_freq),
      list(
        par = c("xi", "alpha", "kappa"),
        site_names, NULL
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

  # Warn if the reference period falls outside the time span of the data
  # (the data limits are then used instead, as before)
  if (using$ref.start | using$ref.end) {
    span <- tsp(acu)[1:2]
    eps <- 1e-6 / ts_freq
    outside <- function(r) {
      t <- r[1] + (r[2] - 1) / ts_freq
      t < span[1] - eps || t > span[2] + eps
    }
    bad <- c(
      ref.start = using$ref.start && outside(ref.start),
      ref.end = using$ref.end && outside(ref.end)
    )
    if (any(bad)) {
      ym <- as.yearmon(span)
      warning(
        paste0("`", names(bad)[bad], "`", collapse = " and "),
        ifelse(sum(bad) > 1, " fall", " falls"),
        " outside the time span of `data` (", ym[1], " to ", ym[2],
        ", frequency ", ts_freq, "); the reference period is limited to ",
        "the data. Check the time information of `data` (see `start` and ",
        "`frequency`).",
        call. = FALSE
      )
    }
  }

  # Trim data set to reference period for fitting (acu.ref)
  if (using$ref.start | using$ref.end) {
    acu.ref <- suppressWarnings(window(acu, ref.start, ref.end))
  } else {
    acu.ref <- acu
  }

  # Instantiate an object to store the standardized data
  spei <- acu * NA

  # Count of maximum likelihood fits that did not converge, and of periods
  # that could not be fitted
  maxlik_failed <- 0
  n_skipped <- 0

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
        spei[ff, s] <- NA
        coef[, s, c] <- NA
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

        # Early stopping: too few (non-zero) values, or constant values
        if (length(x.mon) < 4 || is.na(x.mon_sd) || x.mon_sd == 0) {
          spei[ff, s] <- NA
          coef[, s, c] <- NA
          n_skipped <- n_skipped + 1
          next()
        }

        # Calculate probability weighted moments based on `lmomco` or
        # `TLMoments`
        pwm <- switch(fit,
          "pp-pwm" = pwm.pp(x.mon, A = -0.35, B = 0, nmom = 3, sort = TRUE),
          "ub-pwm" = ,
          "max-lik" = PWM(x.mon, order = 0:2)
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
          ml <- parglo.maxlik(x.mon, f_params)
          if (ml$conv == 0) {
            f_params <- ml$para
          } else {
            maxlik_failed <- maxlik_failed + 1
          }
        }
      } else {
        # User-provided distribution parameters

        f_params <- as.vector(params[1:dim_params, s, c])
        if (params_pze) {
          pze <- params[dim_coef, s, c]
        }
      }

      # Store the coefficients
      if (has_pze) {
        coef[, s, c] <- c(f_params, pze)
      } else {
        coef[, s, c] <- f_params
      }

      ## Standardize - - - - - - - - - - - - - -

      # Calculate CDF on `x` using `f_params`
      cdf_res <- switch(distribution,
        "log-Logistic" = lmom::cdfglo(x[ff], f_params),
        "Gamma" = lmom::cdfgam(x[ff], f_params),
        "PearsonIII" = lmom::cdfpe3(x[ff], f_params)
      )
      # Store the standardized values
      spei[ff, s] <- qnorm(cdf_res)

      # Adjust for `pze` if distribution is Gamma or PearsonIII
      if (distribution == "Gamma" | distribution == "PearsonIII") {
        spei[ff, s] <- qnorm(pze + (1 - pze) * pnorm(spei[ff, s]))
      }
    } # next c (month)
  } # next s (series)

  if (n_skipped > 0) {
    warning(
      "In ", n_skipped, " case(s) (series and period of the year), fewer ",
      "than 4 ", ifelse(has_pze, "non-zero ", ""), "values or only constant ",
      "values were available in the reference period, so the distribution ",
      "could not be fitted and the index was set to NA.",
      call. = FALSE
    )
  }
  if (maxlik_failed > 0) {
    warning(
      "Maximum likelihood estimation did not converge in ", maxlik_failed,
      " case(s); the `ub-pwm` estimates were used instead."
    )
  }


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
    # Label each period, if the time information was given with `start` /
    # `frequency` (time dimnames it was taken from are kept as they are)
    if (using_start && !time_inferred) {
      spei <- set_time_dimnames(
        spei, dimnames(data),
        time_labels(ts_start, ts_freq, n_times)
      )
    }
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
    na.action = na.rm,
    index = "SPEI"
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
#' \method{plot}{spei}(x, main = NULL, ...)
#'
#' @param x an object of class \code{spei}.
#' @param object an object of class \code{spei}.
#' @param main optional, a character string with the plot title.
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
#' circles. The plot is a \code{ggplot2} object, so it can be further
#' modified by adding \code{ggplot2} layers or themes.
#'
#' These are S3 methods: call them as \code{print(x)}, \code{summary(x)}
#' and \code{plot(x)}. The method functions themselves (e.g.
#' \code{plot.spei}) are not exported.
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
  }
  cat("\nFitted:\n")
  print(x$fitted)
}

#'
#' @title plot spei/spi
#' @description See print.spei
#' @details See print.spei
#' @rdname Generic-methods-for-spei-objects
#'
#' @import ggplot2
#' @importFrom zoo na.trim
#'
#' @export
#'
plot.spei <- function(x, main = NULL, ...) {
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -

  if (!inherits(x, "spei")) {
    stop("Data must be an `spei` object resulting from a call to spi() or spei() functions.")
  }


  ### Make the plot - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # A workaround to avoid R CMD check warning about no visible binding for
  # global variables
  # utils::globalVariables(na, value)

  # Label (objects from SPEI < 1.9 have no `index` element)
  index <- x$index
  if (is.null(index)) {
    fun <- deparse(x$call[[1]])
    index <- if (identical(fun, "spi")) "SPI" else "SPEI"
  }
  label <- paste0(index, " (z-values)")

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
  # (the time labels of a 3-d array, see time_labels(), give its start; its
  # frequency is the number of periods in the coefficients array)
  if (is.ts(data)) {
    ts_freq <- frequency(data)
    ts_start <- start(data)
  } else {
    ts_freq <- 12
    ts_start <- 1
    if (out_type == "array") {
      freq <- dim(x$coefficients)[3]
      first <- parse_time_labels(dimnames(data)[[1]], freq)
      if (!is.null(first)) {
        ts_freq <- freq
        ts_start <- first
      }
    }
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
  # A period (year, cycle) is drawn as a bar centred at
  # year + (cycle - 1) / frequency, so the shading goes from half a period
  # before the first bar to half a period after the last one.
  if (!is.null(x$ref.period)) {
    rp <- x$ref.period
    first <- if ("ref.start" %in% rownames(rp)) rp["ref.start", ] else start(data)
    last <- if ("ref.end" %in% rownames(rp)) rp["ref.end", ] else end(data)
    ref1 <- first[1] + (first[2] - 1.5) / ts_freq
    ref2 <- last[1] + (last[2] - 0.5) / ts_freq
  } else {
    ref1 <- ref2 <- NULL
  }

  # Remove leading / ending NAs
  data <- na.trim(data)

  # Reshape to long format: one row per time and series
  wide <- as.data.frame(data)
  kk <- data.frame(
    time = rep(as.numeric(time(data)), ncol(wide)),
    variable = factor(rep(names(wide), each = nrow(wide)),
      levels = names(wide)
    ),
    value = unlist(wide, use.names = FALSE)
  )

  # Add NAs
  kk$na <- as.numeric(ifelse(is.na(kk$value), 0, NA))

  # Add SPI / SPEI categories
  kk$cat <- ifelse(kk$value >= 0, "pos", "neg")

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
    fill = .data[["cat"]],
    color = .data[["cat"]]
  ))
  # reference period (if different than whole series)
  if (!is.null(x$ref.period)) {
    g <- g +
      annotate("rect", xmin = ref1, xmax = ref2, ymin = -Inf, ymax = Inf, alpha = 0.2) +
      geom_vline(xintercept = c(ref1, ref2), color = "grey", alpha = 0.4)
  }
  # add the bars with the SPEI values
  g <- g +
    geom_col(data = kk[!is.na(kk$value), ]) +
      #scale_fill_manual(values=c('blue','red')) +  # classic SPEI look
      #scale_color_manual(values=c('blue','red'))   # classic SPEI look
      scale_fill_manual(values = c(pos = "cyan3", neg = "tomato")) +
      scale_color_manual(values = c(pos = "cyan3", neg = "tomato"))
  # add NAs
  g <- g +
    geom_point(
      data = kk[is.na(kk$value), ],
      aes(.data[["time"]], .data[["na"]]),
      shape = 21, fill = "white",
      color = "black"
    )
  # add other parts and options
  g <- g +
    geom_hline(yintercept = 0, color = "grey") +
    facet_wrap(~variable, ncol = 1) +
    scale_y_continuous(breaks = function(lim) {
      seq(ceiling(lim[1] * 2) / 2, floor(lim[2] * 2) / 2, 0.5)
    }) +
    ylab(label) +
    xlab("Time") +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0)
    )

  if (!is.null(main)) {
    g <- g + ggtitle(main)
  }

  return(g)
}
