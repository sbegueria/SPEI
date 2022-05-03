spei <- function(x, y,...) UseMethod('spei')


#' @name Drought-indices
#' 
#' @aliases spi
#' 
#' @title Calculation of the Standardized Precipitation-Evapotranspiration 
#' Index (SPEI) and the Standardized Precipitation Index (SPI).
#' 
#' 
#' @description
#' Given a time series of the climatic water balance (precipitation minus 
#' potential evapotranspiration), gives a time series of the Standardized 
#' Precipitation-Evapotranspiration Index (SPEI).
#' 
#' 
#' @usage 
#' spei(data, scale, kernel = list(type = 'rectangular', shift = 0),
#' distribution = 'log-Logistic', fit = 'ub-pwm', na.rm = FALSE,
#' ref.start=NULL, ref.end=NULL, x=FALSE, params=NULL, ...)
#'
#' 
#' 
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
#' @param x optional, a logical value indicating whether the data used 
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
#' \code{data} is a vector or a matrix it will be treated as a sequence of monthly 
#' values starting in January. If it is a (univariate or multivariate) time series
#' then the function \code{\link{cycle}} will be used to determine the position of 
#' each observation within the year (month), allowing the data to start in a month 
#' other than January.
#' 
#' 
#' @section Time scales:
#' An important advantage of the SPEI and the SPI is that they can be computed at 
#' different time scales. This way it is possible to incorporate the influence of 
#' the past values of the variable in the computation enabling the index to adapt 
#' to the memory of the system under study. The magnitude of this memory is 
#' controlled by parameter \code{scale}. For example, a value of six would imply 
#' that data from the current month and of the past five months will be used for 
#' computing the SPEI or SPI value for a given month. By default all past data will 
#' have the same weight in computing the index, as it was originally proposed in the 
#' references below. Other kernels, however, are available through parameter \code{kernel}. 
#' The parameter \code{kernel} is a list defining the shape of the kernel and a time shift. 
#' These parameters are then passed to the function \code{\link{kern}}.
#' 
#' 
#' @section Probability distributions:
#' Following the original definitions \code{spei} uses a log-Logistic distribution 
#' by default, and \code{spi} uses a Gamma distribution. This behaviour can be modified, 
#' however, through parameter \code{distribution}.
#' 
#' 
#' @section Fitting methods:
#' The default method for parameter fitting is based on unbiased Probability Weighted 
#' Moments ('ub-pwm'), but other methods can be used through parameter \code{fit}. 
#' A valid alternative is the plotting-position PWM ('pp-pwm') method. For the log-Logistic 
#' distribution, also the maximum likelihood method ('max-lik') is available.
#' 
#' 
#' @section User-provided parameters:
#' An option exists to override parameter fitting and provide user default parameters. 
#' This is activated with the parameter \code{params}. The exact values provided to this 
#' parameter depend on the distribution function being used. For log-Logistic and PearsonIII 
#' it should be a three-dimensional array with dimensions (3,number of series in data,12), 
#' containing twelve parameter triads (xi, alpha, kappa) for each data series, one for each
#' month. For Gamma, a three-dimensional array with dimensions (2,number of series in data,12), 
#' containing twelve parameter pairs (alpha, beta). It is a good idea to look at the 
#' coefficients slot of a previously fit \code{spei} spei object in order to understand the 
#' structure of the parameter array. The parameter \code{distribution} is still used under 
#' this option in order to know what distribution function should be used.
#' 
#' 
#' @section Reference period:
#' The default behaviour of the functions is using all the values provided in \code{data} 
#' for parameter fitting. However, this can be modified with help of parameters \code{ref.start} 
#' and \code{ref.end}. These parameters allow defining a subset of values that will be used 
#' for parameter fitting, i.e. a reference period. The functions, however, will compute the 
#' values of the indices for the whole data set. For these options to work it is necessary that 
#' \code{data} will be a time series object. The starting and ending points of the reference period 
#' will then be defined as pairs of year and month values, e.g. c(1900,1).
#' 
#' 
#' @section Processing large datasets:
#' It is possible to use the \code{spei} and \code{spi} functions for processing multivariate 
#' datasets at once. If a matrix or data frame is supplied as \code{data}, with time series of 
#' precipitation or precipitation minus potential evapotranspiration arranged in columns, the 
#' result would be a matrix (data frame) of spi or spei series. This makes processing large datasets 
#' extremely easy, since no loops need to be used.
#' 
#' 
#' @return 
#' Functions \code{spei} and \code{spi} return an object of class \code{spei}. The generic 
#' functions \code{print} and \code{summary} can be used to obtain summaries of the results. 
#' The generic accessor functions \code{coefficients} and \code{fitted} extract useful features 
#' of the object.
#' 
#' An object of class \code{spei} is a list containing at least the following components:
#' 
#' \itemize{
#'   \item call: the call to \code{spei} or \code{spi} used to generate the object.
#'   \item fitted: time series with the values of the Standardized Precipitation-Evapotranspiration 
#'   Index (SPEI) or the Standardized Precipitation Index (SPI). If data consists of several columns 
#'   the function will treat each column as independent data, and the result will be a multivariate 
#'   time series. The names of the columns in \code{data} will be used as column names in fitted.
#'   \item coefficients: an array with the values of the coefficients of the distribution function 
#'   fitted to the data. The first dimension of the array contains the three (or two) coefficients, 
#'   the second dimension will typically consist of twelve values corresponding to each month, and 
#'   the third dimension will be equal to the number of columns (series) in \code{data}. If a time 
#'   scale greater than one has been used then the first elements will have NA value since the kernel 
#'   can not be applied. The first element with valid data will be the one corresponding to the time 
#'   scale chosen.
#'   \item scale: the \code{scale} parameter used to generate the object.
#'   \item kernel: the parameters and values of the kernel used to generate the object.
#'   \item distribution: the distribution function used to generate the object.
#'   \item fit: the fitting method used to generate the object.
#'   \item na.action: the value of the na.action parameter used.
#'   \item data: if requested, the input data used.
#' }
#' 
#' 
#' @references 
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive 
#' to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. 
#' \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
#' 
#' S. Beguería, S.M Vicente-Serrano, F. Reig, B. Latorre. 2014. Standardized precipitation
#' evapotranspiration index (SPEI) revisited: parameter fitting, evapotranspiration models, tools,
#' datasets and drought monitoring. \emph{International Journal of Climatology}
#' \bold{34}(10): 3001-3023.
#' 
#' \url{http://spei.csic.es}
#' 
#' 
#' @author Santiago Beguería and Sergio M. Vicente-Serrano. Maintainer: Santiago Beguería.
#' 
#' 
#' @seealso 
#' \code{\link{kern}} for different kernel functions available. \code{\link{thornthwaite}}, 
#' \code{\link{hargreaves}} and \code{\link{penman}} for ways of calculating potential evapotranspiration. 
#' \code{\link{summary.spei}} and \code{\link{print.spei}} for summaries of \code{spei} objects. 
#' \code{\link{plot.spei}} for plotting \code{spei} objects.
#' 
#' 
#' @examples 
#' # Load data
#' data(wichita)
#' 
#' # Compute potential evapotranspiration (PET) and climatic water balance (BAL)
#' wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
#' wichita$BAL <- wichita$PRCP-wichita$PET
#' 
#' # Convert to a ts (time series) object for convenience
#' wichita <- ts(wichita[,-c(1,2)], end=c(2011,10), frequency=12)
#' plot(wichita)
#' 
#' # One and tvelwe-months SPEI
#' spei1 <- spei(wichita[,'BAL'], 1)
#' spei12 <- spei(wichita[,'BAL'], 12)
#' class(spei1)
#' 
#' # Extract information from spei object: summary, call function, fitted values, and coefficients
#' summary(spei1)
#' names(spei1)
#' spei1$call
#' spei1$fitted
#' spei1$coefficients
#' 
#' # Plot spei object
#' par(mfrow=c(2,1))
#' plot(spei1, main='Wichita, SPEI-1')
#' plot(spei12, main='Wichita, SPEI-12')
#' 
#' # One and tvelwe-months SPI
#' spi_1 <- spi(wichita[,'PRCP'], 1)
#' spi_12 <- spi(wichita[,'PRCP'], 12)
#' 
#' par(mfrow=c(2,1))
#' plot(spi_1, 'Wichita, SPI-1')
#' plot(spi_12, 'Wichita, SPI-12')
#' 
#' # Time series not starting in January
#' par(mfrow=c(1,1))
#' plot(spei(ts(wichita[,'BAL'], freq=12, start=c(1980,6)), 12))
#' 
#' # Using a particular reference period (1980-2000) for computing the parameters
#' plot(spei(ts(wichita[,'BAL'], freq=12, start=c(1980,6)), 12,
#' 	ref.start=c(1980,1), ref.end=c(2000,1)))
#' 
#' # Using different kernels
#' spei24 <- spei(wichita[,'BAL'],24)
#' spei24_gau <- spei(wichita[,'BAL'], 24, kernel=list(type='gaussian', shift=0))
#' par(mfrow=c(2,1))
#' plot(spei24, main='SPEI-24 with rectangular kernel')
#' plot(spei24_gau, main='SPEI-24 with gaussian kernel')
#' 
#' # Computing several time series at a time
#' # Dataset balance contains time series of the climatic water balance at 12 locations
#' data(balance)
#' head(balance)
#' bal_spei12 <- spei(balance, 12)
#' plot(bal_spei12)
#' 
#' # Using custom (user provided) parameters
#' coe <- spei1$coefficients
#' dim(coe)
#' spei(wichita[,'BAL'], 1, params=coe)
#' 
#' @importFrom stats cycle ts frequency start is.ts pnorm qnorm window embed sd
#' @importFrom lmomco pwm.pp pwm2lmom are.lmom.valid parglo pargam parpe3 cdfgam cdfpe3
#' @importFrom lmom pelglo pelgam pelpe3
#' @importFrom TLMoments PWM
#' 
#' @export
#' 
spei <- function(data, scale, kernel=list(type='rectangular', shift=0),
                 distribution='log-Logistic', fit='ub-pwm', na.rm=FALSE, 
                 ref.start=NULL, ref.end=NULL, x=FALSE, params=NULL,
                 verbose=TRUE, ...) {
  
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which combinations of inputs were passed and check their
  # validity, and check that all the inputs have the same dimensions
  
  # Instantiate two objects to collect errors and warnings
  check <- makeAssertCollection()
  warn  <- makeAssertCollection()
  
  # A list of computation options
  using <- list(na.rm=FALSE, ref.start=FALSE, ref.end=FALSE, x=FALSE,
                params=FALSE)
  
  # Check compulsory inputs
  
  if (!is.numeric(scale)) {
    check$push('Argument `scale` must be numeric.')
  } else if (length(scale) != 1) {
    check$push('Argument `scale` must be a single value')
  } else {
    warn$push(paste0('Calculating the Standardized Precipitation ',
                     'Evapotranspiration Index (SPEI) at a time scale of ', scale, '.'))
  }
  
  # Check optional inputs
  
  if (!is.list(kernel) | length(kernel) != 2 | names(kernel)[1] != 'type' |
      names(kernel)[2] != 'shift') {
    check$push('Argument `kernel` must be a list with components `type` and `shift`.')
  } else if (class(kernel$type) != 'character' | length(kernel$type) != 1) {
    check$push('Element `type` of `kernel` must be a single valued numeric.')
  } else if (class(kernel$shift) != 'numeric' | length(kernel$shift) != 1) {
    check$push('Element `shift` of `kernel` must be a single valued numeric.')
  } else {
    warn$push(paste0('Using kernel type \'', kernel$type, '\'', ', with ',
                     kernel$shift, ' shift.'))
  }
  
  if (is.null(params)) {
    # fit distribution
    if (!is.character(distribution) | length(distribution) != 1 |
        ! distribution %in% c('log-Logistic', 'Gamma' ,'PearsonIII')) {
      check$push(paste0('Argument `distribution` must be one of `log-Logistic`,',
                        ' `Gamma` or `PearsonIII`.'))
    } else {
      warn$push(paste0('Fitting the data to a ', distribution, ' distribution.'))
    }
    if (!is.character(fit) | length(fit) != 1 |
        ! fit %in% c('ub-pwm', 'pp-pwm' ,'max-lik')) {
      check$push(paste0('Argument `fit` must be one of `ub-pwm`, `pp-pwm` ',
                        'or `max-lik`.'))
    } else {
      warn$push(paste0('Using the ', fit, ' parameter fitting method.'))
    }
  } else {
    # do not fit distribution; note that additional checks must be performed to
    # guarantee that the user-provided object conforms to what is expected
    using$params <- TRUE
    if (!is.character(distribution) | length(distribution) != 1 |
        ! distribution %in% c('log-Logistic', 'Gamma' ,'PearsonIII')) {
      check$push(paste0('Argument `distribution` must be one of `log-Logistic`,',
                        ' `Gamma` or `PearsonIII`.'))
    } else {
      warn$push(paste0('Using the ', distribution, ' distribution with ',
                       'user-specified distribution parameters.'))
    }
  }
  
  if (!is.logical(na.rm)) {
    check$push('Argument `na.rm` must be set to either TRUE or FALSE.')
  } else if (na.rm) {
    warn$push('Missing values (`NA`) will not be considered in the calculation.')
  } else {
    using$na.rm <- TRUE
    warn$push('Checking for missing values (`NA`): all the data must be complete.')
  }
  
  # note: additional checks must be performed on both ref to see they conform to the data
  if (!is.null(ref.start) | !is.null(ref.end)) {
    if (!is.numeric(ref.start) | length(ref.start) != 2) {
      check$push('Argument `ref.start` must be a numeric vector of length two.')
    } else {
      using$ref.start <- TRUE
    }
    if (!is.numeric(ref.end) | length(ref.end) != 2) {
      check$push('Argument `ref.end` must be a numeric vector of length two.')
    } else {
      using$ref.end <- TRUE
    }
    warn$push('Using a user-specified reference period.')
  } else {
    warn$push('Using the whole time series as reference period.')
  }
  
  if (!is.logical(x)) {
    check$push('Argument `x` must be set to either TRUE or FALSE.')
  } else if (x) {
    using$x <- TRUE
    warn$push('Storing the input data in the returned spei object.')
  }
  
  if (!is.logical(verbose)) {
    check$push('Argument `verbose` must be set to either TRUE or FALSE.')
  }
  
  # Check for missing values in inputs
  
  if (!na.rm && anyNA(data)) {
    check$push('`data` must not contain NA values if argument `na.rm` is set to FALSE.')
  }
  
  # Determine input dimensions and compute internal dimensions (int_dims)
  tmin_dims <- dim(data)
  if (is.null(tmin_dims) || length(tmin_dims)==1) {
    # vector input (single-site)
    int_dims <- c(length(data), 1, 1)
  } else if (length(tmin_dims)==2) {
    # matrix input (multi-site)
    int_dims <- c(tmin_dims, 1)
  } else if (length(tmin_dims)==3) {
    # 3D array input (gridded data)
    int_dims <- tmin_dims
  } else {
    check$push('Input data can not have more than three dimensions.')
  }
  n_sites <- prod(int_dims[[2]], int_dims[[3]])
  n_times <- int_dims[[1]]
  input_len <- prod(int_dims)
  
  # Determine output data shape
  if (is.ts(data)) {
    if (is.matrix(data)) {
      out_type <- 'tsmatrix'
    } else {
      out_type <- 'tsvector'
    }
  } else if (is.vector(data)) {
    out_type <- 'vector'
  } else if (is.matrix(data)) {
    out_type <- 'matrix'
  } else { # is.array; default
    out_type <- 'array'
  }
  warn$push(paste0('Input type is ', out_type, '.'))
  
  # Save column names for later
  names <- dimnames(data)
  
  # Determine dates in data
  if (is.ts(data)) {
    ts_freq <- frequency(data)
    ts_start <- start(data)
    ts_end <- end(data)
    cyc <- cycle(data)
  } else {
    ts_freq <- 12
    ts_start <- 1
    ts_end <- n_times
    cyc  <- cycle(ts(1:n_times, frequency = 12))
  }
  
  
  # Verify the dimensions of the parameters array
  
  dim_params <- ifelse(distribution == 'Gamma', 2, 3)
  if (using$params) {
    if (dim(params)[1] != dim_params | dim(params)[2]  != n_sites |
        dim(params)[3] != ts_freq) {
      check$push(paste0('Parameters array should have dimensions (', dim_params,
                        ', ', n_sites, ', ', ts_freq, ')'))
    }
  }
  
  # Create uniformly-dimensioned arrays from input
  #  data <- array(data.matrix(data), int_dims)
  if (!is.ts(data)) {
    data <- ts(as.matrix(data), frequency = 12)
  } else {
    data <- ts(as.matrix(data), frequency=frequency(data), start=start(data))
  }
  
  # Return errors and halt execution (if any)
  if (!check$isEmpty()) {
    stop(paste(check$getMessages(), collapse=' '))
  }
  
  # Show a warning with computation options
  if (verbose) {
    print(paste(warn$getMessages(), collapse=' '))
  }
  
  
  ### Computation of ETo - - - - - - - - - - - - - - - - - - - - - - - - -
  
  coef = switch(distribution,
                "Gamma" = array(NA, c(2, n_sites, ts_freq),
                                list(par=c('alpha','beta'), colnames(data), NULL)),
                "log-Logistic" = array(NA, c(3, n_sites, ts_freq),
                                       list(par=c('xi','alpha','kappa'), colnames(data), NULL)),
                "PearsonIII" = coef <- array(NA, c(3, n_sites, ts_freq),
                                             list(par=c('mu','sigma','gamma'), colnames(data), NULL))
  )
  
  # Trim data set to reference period for fitting
  if (!using$ref.start) ref.start <- ts_start
  if (!using$ref.end) ref.end <- ts_end
  data.fit <- window(data, ref.start, ref.end)
  
  # Instantiate and object to store the standardized results
  std <- data*NA
  
  # Loop through series (columns in data) - This only works if data is a time series
  for (s in 1:n_sites) {
    
    # Cumulative series at the desired time scale (acu)
    acu <- data.fit[,s]
    acu.pred <- data[,s]
    if (scale>1) {
      wgt <- kern(scale, kernel$type, kernel$shift)
      acu[scale:length(acu)] <- rowSums(embed(acu, scale) * wgt, na.rm=na.rm)
      acu[1:(scale-1)] <- NA
      acu.pred[scale:length(acu.pred)] <- rowSums(embed(acu.pred, scale) * wgt, 
                                                  na.rm=na.rm)
      acu.pred[1:(scale-1)] <- NA
    }
    
    # Loop through the months
    for (c in (1:ts_freq)) {
      
      # Filter month m, excluding NAs
      f <- which(cycle(acu)==c)
      f <- f[!is.na(acu[f])]
      ff <- which(cycle(acu.pred)==c)
      ff <- ff[!is.na(acu.pred[ff])]
      
      # Monthly series, sorted
      month <- sort.default(acu[f], method='quick')
      
      if (length(month)==0) {
        std[f] <- NA
        next()
      }
      
      # Probability of zero (pze)
      if(distribution != 'log-Logistic'){
        pze <- sum(month==0) / length(month)
        month = month[month > 0]
      }
      
      # Distribution parameters
      if (!using$params) {
        # Fit distribution parameters
        month_sd = sd(month, na.rm=TRUE)
        
        # Early stopping
        if (is.na(month_sd) || (month_sd == 0)) {
          std[f] <- NA
          next
        }
        if(length(month) < 4){
          std[ff,s] = NA
          coef[,s,c] <- NA
          next
        }
        
        # Calculate probability weighted moments based lmomco or TLMoments
        pwm = switch(fit,
                     'pp-pwm' = pwm.pp(month,-0.35,0, nmom=3),
                     #pwm.ub(month, nmom=3)
                     'ub-pwm' = TLMoments::PWM(month, order=0:2)
        )
        
        # Check L-moments validity
        lmom <- pwm2lmom(pwm)
        if ( !are.lmom.valid(lmom) || anyNA(lmom[[1]]) ||
             any(is.nan(lmom[[1]])) ){
          next
        }
        
        # `lmom` fortran functions need specific inputs L1, L2, T3
        # This is handled internally by `lmomco` with `lmorph`
        fortran_vec = c(lmom$lambdas[1:2], lmom$ratios[3])
        
        # Calculate parameters based on distribution with `lmom`, then `lmomco`
        f_params = switch(distribution,
                          'log-Logistic' = tryCatch(lmom::pelglo(fortran_vec),
                                                    error = function(e){ parglo(lmom)$para }),
                          'Gamma' = tryCatch(lmom::pelgam(fortran_vec),
                                             error = function(e){ pargam(lmom)$para }),
                          'PearsonIII' = tryCatch(lmom::pelpe3(fortran_vec),
                                                  error = function(e){ parpe3(lmom)$para })
        )
        
        # Adjust if user chose log-Logistic and max-lik
        if(distribution == 'log-Logistic' && fit=='max-lik'){
          f_params = parglo.maxlik(month, f_params)$para
        }
      } else {
        # User-provided distribution parameters
        
        f_params = as.vector(params[,s,c])
        
      }
      
      # Calculate CDF based on distribution with `lmom`
      cdf_res = switch(distribution,
                       'log-Logistic' = lmom::cdfglo(acu.pred[ff], f_params),
                       'Gamma' = lmom::cdfgam(acu.pred[ff], f_params),
                       'PearsonIII' = lmom::cdfpe3(acu.pred[ff], f_params)				  				
      )
      
      std[ff,s] = qnorm(cdf_res)
      coef[,s,c] <- f_params
      
      # Adjust if user chose Gamma or PearsonIII
      if(distribution != 'log-Logistic'){ 
        std[ff,s] = qnorm(pze + (1-pze) * pnorm(std[ff,s]))
      }
      
    } # next c (month)
  } # next s (series)
  colnames(std) <- colnames(data)
  
  z <- list(call = match.call(expand.dots=FALSE),
            fitted = std, coefficients=coef, scale=scale,
            kernel = list(type=kernel$type,
                          shift = kernel$shift, values=kern(scale, kernel$type, kernel$shift)),
            distribution = distribution, fit=fit, na.action=na.rm)
  if (using$x) z$data <- data
  if (using$ref.start | using$ref.end) z$ref.period <- rbind(ref.start,ref.end)
  
  class(z) <- 'spei'
  return(z)
}

#' @name Generic-methods-for-spei-objects
#' 
#' @title Generic methods for \code{spei} objects.
#' 
#' @aliases print.spi plot.spei plot.spi summary.spei summary.spi
#' 
#' @description 
#' Generic methods for extracting information and plotting \code{spei} objects.
#' 
#' @usage
#' \method{print}{spei}(x, ...)
#' \method{summary}{spei}(object, ...)
#' \method{plot}{spei}(x, ttext, ...)
#' 
#' @param x an object of class \code{spei}.
#' @param object an object of class \code{spei}.
#' @param ttext text to use as part of the plot title
#' @param ... additional parameters, not used at present.
#' 
#' 
#' @details This functions allow extracting information and plotting \code{spei} 
#' objects. \code{print} yields the fitted values, i.e. a time series of SPEI or SPI values. 
#' \code{summary} reports the function call, the parameters of the PDF used, and the time 
#' series of SPEI or SPI values. \code{plot} produces a plot of the time series of SPEI or 
#' SPI values, with blue and red colors for positive and negative values, respectively. If 
#' a reference period was used in the function call it is shown by a shaded area. In the 
#' unlikely case that NA or Inf values were produced, these are shown by circles.
#' 
#' @references 
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index 
#' sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. 
#' \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
#' 
#' 
#' @author Santiago Beguería
#'  
#' 
#' @export
#' 
print.spei <- function (x, ...) {
	print(x$fitted)
}

#' 
#' @title summary of spei/spi
#' 
#' 
#' @description See print.spei
#' 
#' 
#' @details See print.spei
#' 
#' 
#' @rdname Generic-methods-for-spei-objects
#' 
#' 
#' @export
#' 
summary.spei <- function (object, ...) {
	x <- object
	cat('Call:\n')
	print(x$call)
	cat('\nCoefficients:\n')
	for (i in 1:dim(x$coeff)[2]) {
		cat('\t',dimnames(x$coeff)[[2]][i],':\n',sep='')
		tab <- cbind(t(x$coeff[,i,]))
		rownames(tab) <- 1:dim(x$coeff)[3]
		print(tab)
		cat('\nFitted:\n')
		print(x$fitted)
	}
}

#' 
#' @title plot spei/spi
#' 
#' 
#' @description See print.spei
#' 
#' 
#' @details See print.spei
#' 
#' 
#' @rdname Generic-methods-for-spei-objects
#' 
#' 
#' @importFrom stats ts frequency end 
#' @importFrom graphics plot polygon abline grid lines points par
#' 
#' 
#' @export
#' 
plot.spei <- function (x, ttext=NULL, ...) {
  label <- ifelse(as.character(x$call)[1]=='spei','SPEI','SPI')
	ser <- ts(as.matrix(x$fitted[-c(1:x$scale),]),
		end=end(x$fitted),frequency=frequency(x$fitted))
	ser[is.nan(ser-ser)] <- 0
	se <- ifelse(ser==0,ser,NA)
	if(is.null(ttext)){
	  tit <- paste(label, dimnames(x$coefficients)[2][[1]])
	} else {
	  tit = paste(label, ttext, seq_along(dim(x$coefficients)[2]))
	}
	#
	if (start(ser)[2]==1) {
		ns <- c(start(ser)[1]-1,12)
	} else {
		ns <- c(start(ser)[1],start(ser)[2]-1)	
	}
	if (end(ser)[2]==12) {
		ne <- c(end(ser)[1]+1,1)
	} else {
		ne <- c(end(ser)[1],end(ser)[2]+1)
	}
	#
	n <- ncol(ser)
	if (is.null(n)) n <- 1
	par(mar=c(4,4,2,1)+0.1)
	if (n>1 & n<5) par(mfrow=c(n,1))
	if (n>1 & n>=5) par(mfrow=c({n+1}%/%2,2))
	for (i in 1:n) {
		datt <- ts(c(0,ser[,i],0),frequency=frequency(ser),start=ns,end=ne)
		datt.pos <- ifelse(datt>0,datt,0)
		datt.neg <- ifelse(datt<=0,datt,0)
		plot(datt, type='n', xlab='', ylab=paste(label, "(z-values)"),main=tit[i])
		if (!is.null(x$ref.period)) {
			k <- ts(5,start=x$ref.period[1,],end=x$ref.period[2,],frequency=12)
			k[1] <- k[length(k)] <- -5
			polygon(k, col='light grey',border=NA,density=20)
 			abline(v=x$ref.period[1,1]+(x$ref.period[1,2]-1)/12,col='grey')
			abline(v=x$ref.period[2,1]+(x$ref.period[2,2]-1)/12,col='grey')
		}
		grid(col='black')
		polygon(datt.pos,col='blue',border=NA)
		polygon(datt.neg,col='red',border=NA)
		lines(datt,col='dark grey')
		abline(h=0)
		points(se,pch=21,col='white',bg='black')
	}
}
