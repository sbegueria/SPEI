#' @title Computation of potential evapotranspiration.
#' 
#' 
#' @description See hargreaves
#' 
#' 
#' @details See hargreaves
#' 
#' 
#' @return  A time series with the values of monthly potential or reference evapotranspiration, in mm. 
#' If the input is a matrix or a multivariate time series each column will be treated as independent 
#' data (e.g., different observatories), and the output will be a multivariate time series.
#' 
#' 
#' @rdname Potential-evapotranspiration
#'
#'
#' @importFrom stats ts cycle frequency start
#'
#'
#' @export
#'
penman_new <- function(Tmin, Tmax, U2, Ra=NULL, lat=NULL, Rs=NULL, tsun=NULL,
                       CC=NULL, ed=NULL, Tdew=NULL, RH=NULL, P=NULL, P0=NULL,
                       CO2=NULL, z=NULL, crop='short', na.rm=FALSE, 
                       method='ICID', verbose=TRUE) {
  
  ### Argument check - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which combinations of inputs were passed and check their
  # validity, and check that all the inputs have the same dimensions
  
  # Instantiate two new 'ArgCheck' objects to collect errors and warnings
  check <- newArgCheck()
  warn <- newArgCheck()
  
  # A list of computation options
  using <- list(Ra=FALSE, lat=FALSE, Rs=FALSE, tsun=FALSE, CC=FALSE,
                ed=FALSE, Tdew=FALSE, Tmin=FALSE, RH=FALSE, P=FALSE,
                P0=FALSE, CO2=FALSE, z=FALSE, na.rm=FALSE)
  
  # Check required inputs
  if(missing(Tmin)){
    addError('Argument `Tmin` is a required input.', argcheck=check)
  }
  
  if(missing(Tmax)){
    addError('Argument `Tmax` is a required input.', argcheck=check)
  }
  
  if(missing(U2)){
    addError('Argument `U2` is a required input.', argcheck=check)
  } else {
    addWarning('Using user-provided `U2` data.', argcheck=warn)
  }
  
  # Check other inputs
  
  if (!is.null(Ra)) {
    using$Ra <- TRUE
    addWarning(paste('Using user-provided extraterrestrial radiation',
                     '(`Ra`) data.'), argcheck=warn)
  } else if (!is.null(lat)) {
    using$lat <- TRUE
    addWarning(paste('Using latitude (`lat`) to estimate extraterrestrial',
                     'radiation.'), argcheck=warn)
  } else {
    addError('One of `Ra` or `lat` must be provided.', argcheck=check)
  }
  
  if (!is.null(Rs)) {
    using$Rs <- TRUE
    addWarning(paste('Using user-provided incoming solar radiation (`Rs`)',
                     'data.'), argcheck=warn)
  } else if (!is.null(tsun) && !is.null(lat)) {
    using$tsun <- TRUE
    addWarning(paste('Using bright sunshine duration data (`tsun`) to',
                     'estimate incoming solar radiation.'), argcheck=warn)
  } else if (!is.null(CC)) {
    using$CC <- TRUE
    addWarning(paste('Using fraction cloud cover (`CC`) to estimate',
                     'incoming solar radiation.'), argcheck=warn)
  } else {
    addError('One of `Rs`, the pair `tsun` and `lat`, or `CC` must be',
             'provided.', argcheck=check)
  }
  
  if (!is.null(ed)) {
    using$ed <- TRUE
    addWarning('Using user-provided actual vapour pressure (`ed`) data.',
               argcheck=warn)
  } else if (!is.null(Tdew)) {
    using$Tdew <- TRUE
    addWarning(paste('Using dewpoint temperature (`Tdew`) to estimate',
                     'actual vapour pressure.'), argcheck=warn)
  } else if (!is.null(RH)) {
    using$RH <- TRUE
    addWarning(paste('Using relative humidity (`RH`) to estimate actual',
                     'vapour pressure.'), argcheck=warn)
  } else if (!is.null(Tmin)) {
    using$Tmin <- TRUE
    addWarning(paste('Using minimum temperature (`Tmin`) to estimate',
                     'dewpoint temperature and actual vapour pressure.'),
               argcheck=warn)
  } else {
    addError('One of `ed`, `Tdew`, `RH` or `Tmin` must be provided.',
             argcheck=check)
  }
  
  if (!is.null(P)) {
    using$P <- TRUE
    addWarning(paste('Using user-provided atmospheric surface pressure',
                     '(`P`) data.'), argcheck=warn)
  } else if (!is.null(P0) && !is.null(z)) {
    using$P0 <- TRUE
    addWarning(paste('Using atmospheric pressure at sea level (`P0`) and',
                     'elevation `z` to estimate atmospheric surface pressure.'), 
               argcheck=warn)
  } else if (!is.null(z)) {
    using$z <- TRUE
    addWarning(paste('Assuming constant atmospheric surface pressure', 
                     'corresponding to elevation `z`.'), argcheck=warn)
  } else {
    addError('One of `P`, the pair `P0` and `z`, or `z` must be provided.',
             argcheck=check)
  }
  
  if (!is.null(CO2)) {
    using$CO2 <- TRUE
    addWarning(paste('Using custom CO2 concentration.'), argcheck=warn)
  }
  
  if (is.null(z)) {
    addWarning(paste('Specifying the elevation above sea level (z)',
                     'is highly recommended in order to compute the clear-sky',
                     'solar radiation.'), argcheck=warn)
  }
  
  if (crop=='short') {
    addWarning('Computing for a short crop.', argcheck=warn)
  } else if (crop=='tall') {
    addWarning('Computing for a tall crop.', argcheck=warn)
  } else {
    addError('Argument `crop` must be one of `short` or `tall`.',
             argcheck=check)
  }
  
  if (na.rm != TRUE && na.rm != FALSE) {
    addError('Argument `na.rm` must be set to TRUE or FALSE.',
             argcheck=check)
  } else if (na.rm) {
    addWarning(paste('Missing values (`NA`) will not be considered',
                     'in the calculation.'), argcheck=warn)
  } else {
    addWarning(paste('Checking for missing values (`NA`): all the data',
                     'must be complete.'), argcheck=warn)
  }
  
  # Check for missing values in inputs
  if (!na.rm && (anyNA(Tmin) || anyNA(Tmax) || anyNA(U2))) {
    addError(paste('`Tmin`, `Tmax` and `U2` must not contain NA values if',
                   'argument `na.rm` is set to FALSE.'), argcheck=check)
  }
  
  if (!na.rm &&
      ((using$Ra && anyNA(Ra)) ||
       (using$lat && anyNA(lat)) ||
       (using$Rs && anyNA(Rs)) ||
       (using$tsun && anyNA(tsun)) ||
       (using$CC && anyNA(CC)) ||
       (using$ed && anyNA(ed)) ||
       (using$Tdew && anyNA(Tdew)) ||
       (using$RH && anyNA(RH)) ||
       (using$P && anyNA(P)) ||
       (using$P0 && (anyNA(P0) || anyNA(z))) ||
       (using$CO2 && anyNA(CO2)) ||
       (using$z && anyNA(z)))) {
    addError(paste('Data must not contain NA values if argument `na.rm`',
                   'is set to FALSE.'), argcheck=check)
  }
  
  # Determine input dimensions and compute internal dimensions (int_dims)
  tmin_dims <- dim(Tmin)
  if (is.null(tmin_dims) || length(tmin_dims)==1) {
    # vector input (single-site)
    int_dims <- c(length(Tmin), 1, 1)
  } else if (length(tmin_dims)==2) {
    # matrix input (multi-site)
    int_dims <- c(tmin_dims, 1)
  } else if (length(tmin_dims)==3) {
    # 3D array input (gridded data)
    int_dims <- tmin_dims
  } else {
    addError('Input data can not have more than 3 dimensions',
             argcheck=check)
  }
  n_sites <- prod(int_dims[[2]], int_dims[[3]])
  n_times <- int_dims[[1]]
  
  # Determine output data shape
  if (is.ts(Tmin)) {
    if (is.matrix(Tmin)) {
      out_type <- 'tsmatrix'
    } else {
      out_type <- 'tsvector'
    }
  } else if (is.vector(Tmin)) {
    out_type <- 'vector'
  } else if (is.matrix(Tmin)) {
    out_type <- 'matrix'
  } else { # is.array; default
    out_type <- 'array'
  }
  addWarning(paste0('Input type is ', out_type, '.'), argcheck=warn)
  
  # Save column names for later
  names <- dimnames(Tmin)
  
  # Determine dates in data
  if (is.ts(Tmin)) {
    ts_freq <- frequency(Tmin)
    ts_start <- start(Tmin)
    cyc <- cycle(Tmin)
  } else {
    ts_freq <- 12
    ts_start <- 1
    cyc  <- cycle(ts(1:n_times, frequency = 12))
  }
  
  # Get length of each month and day of the middle of each month
  mlen <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  msum <- cumsum(mlen) - mlen + 15
  # convert month data to array
  mlen_array <- array(mlen[cyc], dim=int_dims)
  msum_array <- array(msum[cyc], dim=int_dims)
  
  # Verify the length of each input variable
  input_len <- prod(int_dims)
  if (sum(lengths(Tmin))!=input_len || sum(lengths(Tmax))!=input_len ||
      sum(lengths(U2))!=input_len) {
    addError('`Tmin`, `Tmax`, and `U2` cannot have different lengths.',
             argcheck=check)
  }
  if (using$Ra && sum(lengths(Ra))!=input_len) {
    addError('`Ra` has incorrect length.', argcheck=check)
  }
  if (using$lat && sum(lengths(lat))!=n_sites) {
    addError('`lat` has incorrect length.', argcheck=check)
  }
  if (using$Rs && sum(lengths(Rs))!=input_len) {
    addError('`Rs` has incorrect length.', argcheck=check)
  }
  if (using$tsun && sum(lengths(tsun))!=input_len) {
    addError('`tsun` has incorrect length.', argcheck=check)
  }
  if (using$CC && sum(lengths(CC))!=input_len) {
    addError('`CC` has incorrect length.', argcheck=check)
  }
  if (using$ed && sum(lengths(ed))!=input_len) {
    addError('`ed` has incorrect length.', argcheck=check)
  }
  if (using$Tdew && sum(lengths(Tdew))!=input_len) {
    addError('`Tdew` has incorrect length.', argcheck=check)
  }
  if (using$RH && sum(lengths(RRHa))!=input_len) {
    addError('`RH` has incorrect length.', argcheck=check)
  }
  if (using$P && sum(lengths(P))!=input_len) {
    addError('`P` has incorrect length.', argcheck=check)
  }
  if (using$P0 && sum(lengths(P0))!=input_len) {
    addError('`P0` has incorrect length.', argcheck=check)
  }
  if (using$CO2 && sum(lengths(CO2))!=input_len) {
    addError('`CO2` has incorrect length.', argcheck=check)
  }
  if (using$z && sum(lengths(z))!=n_sites) {
    addError('`z` has incorrect length.', argcheck=check)
  }
  
  # Create uniformly dimensioned arrays from input
  Tmin <- array(data.matrix(Tmin), int_dims)
  Tmax <- array(data.matrix(Tmax), int_dims)
  U2 <- array(data.matrix(U2), int_dims)
  if (using$Ra) {
    Ra <- array(data.matrix(Ra), int_dims)
  }
  if (using$lat || using$tsun) {
    # copy and permute into correct dimensions
    lat <- aperm(array(data.matrix(lat), int_dims[c(2, 3, 1)]), c(3, 1, 2))
  }
  if (using$Rs) {
    Rs <- array(data.matrix(Rs), int_dims)
  }
  if (using$tsun) {
    tsun <- array(data.matrix(tsun), int_dims)
  }
  if (using$CC) {
    CC <- array(data.matrix(CC), int_dims)
  }
  if (using$ed) {
    ed <- array(data.matrix(ed), int_dims)
  }
  if (using$Tdew) {
    Tdew <- array(data.matrix(Tdew), int_dims)
  }
  if (using$RH) {
    RH <- array(data.matrix(RH), int_dims)
  }
  if (using$P) {
    P <- array(data.matrix(P), int_dims)
  }
  if (using$P0) {
    P0 <- array(data.matrix(P0), int_dims)
  }
  if (using$CO2) {
    CO2 <- array(data.matrix(CO2), int_dims)
  }
  if (using$z) {
    # copy and permute into correct dimensions
    z <- aperm(array(data.matrix(z), int_dims[c(2, 3, 1)]), c(3, 1, 2))
  }
  
  # Method used
  addWarning(paste0('Calculation method is ', method, '.'), argcheck=warn)
  
  # Return errors and halt execution (if any)
  finishArgCheck(check)
  
  # Show a warning with computation options
  if (verbose) {
    finishArgCheck(warn)
  }
  
  
  ### Computation of ETo - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Mean temperature
  Tmean <- (Tmin + Tmax) / 2
  
  # Initialize ET0
  ET0 <- Tmin * NA
  
  # 1. Latent heat of vaporization, lambda (eq. 1.1)
  lambda <- 2.501 - 2.361e-3 * Tmean
  
  # 4. P: atmospheric pressure, kPa
  if (using$P) {
    # good!
  } else if (using$P0) {
    # estimate from sea level pressure (eq. 1.6)
    P <- P0 * (((293 - 0.0065 * z) / 293) ^ 5.26)
  } else if (using$z) {
    # assume a constant pressure
    P0  <- array(101.3, int_dims)
    P <- P0 * (((293 - 0.0065 * z) / 293) ^ 5.26)
  } else {
    stop(paste('An error occurred while computing the surface atmospheric',
               'pressure. Please report this error.'))
  }
  
  # 3. Psychrometric constant, gamma (eq. 1.4)
  gamma <- 1.63e-3 * P / lambda
  if (method=='FAO' | method=='ASCE') {
    # (FAO-56, eq. 8)
    gamma <- 0.665e-3 * P
  } else if (method=='ICID') {
    # (ICID, eq. 1.4)
    gamma <- 1.63e-3 * P / lambda
  }	
  
  # 6. Saturation vapour pressure, ea
  # saturation vapour pressure at tmx (ICID, eq. 1.10, p. 66)
  etmx <- 0.611 * exp((17.27 * Tmax) / (Tmax + 237.3))
  # saturation vapour pressure at tmn (ICID, eq. 1.10, p. 66)
  etmn <- 0.611 * exp((17.27 * Tmin) / (Tmin + 237.3))
  # mean saturation vapour pressure (ICID, eq. 1.11, p. 67)
  ea <- (etmx + etmn) / 2
  # FAO-56 recommends Delta calculation using et instead of ea (p. 37)
  if (method=='FAO' | method=='ASCE'){
    et <- 0.611 * exp((17.27 * Tmean) / (Tmean + 237.3))
  }
  
  # 2. Slope of the saturation vapour pressure function, Delta
  if (method=='FAO' | method=='ASCE'){
    Delta <- 4099 * et / (Tmean + 237.3) ^ 2
  } else if (method=='ICID'){
    # (ICID, eq. 1.3)
    Delta <- 4099 * ea / (Tmean + 237.3) ^ 2
  }
  
  # 7. Actual vapour pressure, ed
  if(using$ed) {
    # good!
  } else if (using$Tdew) {
    # (ICID, eq. 1.12, p. 67)
    ed <- 0.611 * exp((17.27 * Tdew) / (Tdew + 237.3))
  } else if (using$RH) {
    if (method=='FAO'){
      # (FAO-56, eq 19, p.39)
      ed <- ea * (RH / 100)
    } else if (method=='ICID'){
      # (ICID, eq. 1.16, p. 68)
      ed <- RH / ((50 / etmn) + (50 / etmx))
    } else if (method=='ASCE'){
      ed <- et * (RH / 100)
    }
  } else if (using$Tmin) {
    # (ICID, eq. 1.19, p. 69)
    ed <- etmn
    # for arid climates, FAO-56 recommends ed = etmn_2
    # etmn_2 <- 0.611*exp((17.27*(Tmin-2))/(Tmin - 2 + 237.3))
  } else {
    stop(paste('An error occurred while computing the actual vapour',
               'pressure. Please report this error.'))
  }
  
  # Sunset hour angle (needed if no radiation data is available)
  if (!using$Ra | (!using$Rs & using$tsun)) {
    # Note: For the winter months and latitudes higher than 55º the following
    # equations have limited validity (Allen et al., 1994).
    # J: number of day in the year (eq. 1.27)
    msum <- cumsum(mlen) - mlen + 15
    J <- msum[cyc]
    # delta: solar declination, rad (1 rad = 57.2957795 deg) (eq. 1.25)
    delta <- 0.409 * sin(0.0172 * J - 1.39)
    # dr: relative distance Earth-Sun, [] (eq. 1.24)
    dr <- 1 + 0.033 * cos(0.0172 * J)
    # omegas: sunset hour angle, rad (eq. 1.23)
    latr <- lat / 57.2957795
    sset <- -tan(latr) * tan(delta)
    omegas <- sset * 0
    omegas[abs(sset) <= 1] <- acos(sset[abs(sset) <= 1])
    # correction for high latitudes
    omegas[sset < (-1)] <- max(omegas)
  }
  
  # 9. Extraterrestrial radiation, Ra (MJ m-2 d-1)
  if (using$Ra) {
    # good!
  } else {
    # Estimate Ra (eq. 1.22)
    Ra <- 37.6 * dr * (omegas *sin(latr) * sin(delta) +
                         cos(latr) * cos(delta) * sin(omegas))
    Ra <- ifelse(Ra < 0, 0, Ra)
  }
  
  # 11. Net radiation, Rn (MJ m-2 d-1)
  # Net radiation is the sum of net short wave radiation Rns and net long
  # wave (incoming) radiation (Rnl).
  # Rs: daily incoming solar radiation (MJ m-2 d-1)
  if (!using$Rs) {
    # nN: relative sunshine fraction []
    if (using$tsun) {
      # Based on sunshine hours 
      # 10. Potential daylight hours (day length, h), N (eq. 1.34)
      N <- 7.64*omegas
      nN <- tsun/N
    } else if (using$CC) {
      # Based on cloud cover
      nN <- (100-CC)/100
    }
    # (eq. 1.37)
    as <- 0.25; bs <- 0.5
    Rs <- (as+bs*(nN))*Ra
  }
  # Rso: clear-sky solar radiation (eq. 1.40)
  # Note: mostly valid for z<6000 m and low air turbidity
  if (using$z) {
    Rso <- (0.75 + 2e-5 * z) * Ra
  } else {
    Rso <- (0.75+2e-5*840) * Ra
  }
  # Empirical constants
  ac <- 1.35; bc <- -0.35; a1 <- 0.34; b1 <- -0.14
  # Reference crop albedo
  alb <- 0.23
  # Rn, MJ m-2 d-1 (eq. 1.53)
  Rn <- (1-alb)*Rs - (ac*Rs/Rso+bc) * (a1+b1*sqrt(ed)) * 4.9e-9 *
    ((273.15+Tmax)^4+(273.15+Tmin)^4)/2	
  Rn[Rs==0] <- 0
  
  # Soil heat flux density, G
  # Using forward / backward differences for the first and last observations,
  # and central differences for the remaining ones.
  # TODO: if na.rm is FALSE, act similarly if exactly one neighbour is NA.
  G <- array(NA, int_dims)
  G[1,,] <- 0.14 * (Tmean[2,,] - Tmean[1,,])
  G[2:(n_times-1),,] <- 0.07 * (Tmean[3:n_times,,] - Tmean[1:(n_times-2),,])
  G[n_times,,] <- 0.14 * (Tmean[n_times,,] - Tmean[(n_times-1),,])
  
  # Wind speed at 2m, U2 (eq. 1.62)
  #U2 <- U2 * 4.85203/log((zz-0.08)/0.015)
  
  # Daily ET0 (eq. 2.18)
  if (crop=='short') {
    c1 <- 900; c2 <- 0.34 # short reference crop (e.g. clipped grass, 0.12 m)
  } else if (crop=='long') {
    c1 <- 1600; c2 <- 0.38 # tall reference crop (e.g. alfalfa, 0.5 m)
  } else {
    stop(paste('An error occurred while estimating the daily ET0',
               'sunshine fraction Please report this error.'))
  }
  if (!using$CO2) {
    ET0 <- (0.408 * Delta * (Rn - G) + gamma * (c1 / (Tmean + 273)) * U2 *
              (ea - ed)) / (Delta + gamma * (1 + c2 * U2))
  } else {
    ET0 <- (0.408 * Delta * (Rn - G) + gamma * (c1 / (Tmean + 273)) * U2 *
              (ea - ed)) / (Delta + gamma * (1 + U2 * (c2 + 0.00024 * (CO2 - 300))))
  }
  
  # Transform ET0 to mm month-1
  ET0 <- ifelse(ET0 < 0, 0, ET0) * mlen[cyc]
  
  
  ### Format output and return - - - - - - - - - - - - - - - - - - - - - - -
  if (out_type=='tsmatrix') {
    ET0 <- matrix(ET0, nrow=n_times)
    ET0 <- ts(ET0, frequency=ts_freq, start=ts_start)
    colnames(ET0) <- rep('ET0_pen', ncol(ET0))
  } else if (out_type=='tsvector') {
    ET0 <- as.vector(ET0)
    ET0 <- ts(ET0, frequency=ts_freq, start=ts_start)
    #colnames(ET0) <- rep('ET0_pen', ncol(ET0))
  } else if (out_type=='vector') {
    ET0 <- as.vector(ET0)
  } else if (out_type=='matrix') {
    ET0 <- matrix(ET0, nrow=n_times)
    colnames(ET0) <- rep('ET0_pen', ncol(ET0))
  } else { # array, default
    colnames(ET0) <- rep('ET0_pen', ncol(ET0))
  }
  
  dimnames(ET0) <- names
  
  return(ET0)
}