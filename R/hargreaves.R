#' @name Potential-evapotranspiration
#' 
#' 
#' @title Computation of potential evapotranspiration.
#' 
#' 
#' @aliases thornthwaite penman
#' 
#' 
#' @usage 
#' thornthwaite(Tave, lat, na.rm = FALSE)
#' 
#' hargreaves(Tmin, Tmax, Ra = NA, lat = NA, Pre = NA, na.rm = FALSE)
#' 
#' penman(Tmin, Tmax, U2, Ra = NA, lat = NA, Rs = NA, tsun = NA,
#'        CC = NA, ed = NA, Tdew = NA, RH = NA, P = NA, P0 = NA,
#'               z = NA, crop='short', na.rm = FALSE)
#' 
#' 
#' @description 
#' Potential evapotranspiration (PET) is the amount of evaporation and 
#' transpiration that would occur if a sufficient water source were available. Reference 
#' evapotranspiration (ET0) is the amount of evaporation and transpiration from a reference 
#' vegetation of grass. They are usually considered equivalent. This set of functions 
#' calculate PET or ET0 accordind to the Thornthwaite, Hargreaves or Penman-Monteith 
#' equations.
#'
#'
#' @param Tave   a numeric vector, matrix or time series of monthly mean temperatures, ºC. 
#' @param lat   a numeric vector with the latitude of the site or sites, in degrees. 
#' @param na.rm   optional, a logical value indicating whether NA values should be stripped from the computations. 
#' @param Tmax   a numeric vector, matrix or time series of monthly mean daily maximum temperatures, ºC. 
#' @param Tmin   a numeric vector, matrix or time series of monthly mean daily minimum temperatures, ºC. 
#' @param Ra   optional, a numeric vector, matrix or time series of monthly mean daily external radiation, MJ m-2 d-1. 
#' @param Pre   optional, a numeric vector, matrix or time series  of monthly total precipitation, mm. 
#' @param U2   a numeric vector, matrix or time series of monthly mean daily wind speeds at 2 m height, m s-1.  
#' @param Rs   optional, a numeric vector, matrix or time series of monthly mean dialy incoming solar radiation, MJ m-2 d-1.  
#' @param tsun   optional, a numeric vector, matrix or time series of monthly mean daily bright sunshine hours, h.  
#' @param CC   optional, numeric a vector, matrix or time series of monthly mean cloud cover, \%. 
#' @param ed   optional, numeric a vector, matrix or time series of monthly mean actual vapour pressure at 2 m height, kPa. 
#' @param Tdew   optional, a numeric vector, matrix or time series of monthly mean daily dewpoint temperature (used for estimating ed), ºC 
#' @param RH   optional, a numeric vector, matrix or time series of monthly mean relative humidity (used for estimating ed), \%. 
#' @param P   optional, a numeric vector, matrix or time series of monthly mean atmospheric pressure at surface, kPa. 
#' @param P0   optional, a numeric vector, matrix or time series of monthly mean atmospheric pressure at sea level (used for estimating P), kPa. 
#' @param z   optional, a numeric vector of the elevation of the site or sites, m above sea level. 
#' @param crop   optional, character string, type of reference crop. Either one of 'short' (default) or 'tall'. 
#'
#'
#' @details \code{thornthwaite} computes the monthly potential evapotranspiration (PE) according to the
#' Thornthwaite (1948) equation. It is the simplest of the three methods, and can be used when only 
#' temperature data are available.
#'
#'
#' \code{hargreaves} computes the monthly reference evapotranspiration (ET0) of a grass crop based 
#' on the original Hargreaves equation (1994). However, if precipitation data \code{Pre} is provided 
#' a modified form due to Droogers and Allen (2002) will be used; this equation corrects ET0 using 
#' the amount of rain of each month as a proxy for insolation. The Hargreaves method requires data 
#' on the mean external radiation, \code{Ra}. If such data are not available it can be estimated 
#' from the latitude \code{lat} and the month of the year.
#'
#' \code{penman} calculates the monthly reference evapotranspiration (ET0) of a hypothetical 
#' reference crop according to the FAO-56 Penman-Monteith equation described in Allen et al. (1994). 
#' This is a simplification of the original Penman-Monteith equation, and has found widespread use. 
#' By default the original parameterization of Allen et al. (1994) is used, corresponding to a short 
#' reference crop of 0.12 m height. Parameterization for a tall reference crop of 0.5 m height due 
#' to Walter et al. (2002) can also be used, by setting the \code{crop} parameter to 'tall'. The 
#' method requires data on the incoming solar radiation, \code{Rs}; since this is seldom available, 
#' the code will estimate it from data on the bright sunshine duration \code{tsun}, or alternatively 
#' from data on the percent cloud cover \code{CC}. Similarly, if data on the saturation water 
#' pressure \code{ed} are not available, it is possible to estimate it from the dewpoint temperature
#'  \code{Tdew}, from the relative humidity \code{RH} or even from the minimum temperature \code{Tmin} 
#'  (sorted from least to most uncertain method). Similarly, the atmospheric surface pressure \code{P} 
#'  required for computing the psychrometric constant can be calculated from the atmospheric pressure at 
#'  sea level \code{P0} and the elevation \code{z}, or else it will be assumed to be constant (101.3 kPa). 
#'  The code will produce an error message if a valid combination of input parameters is not provided.
#' 
#' If the main input object (\code{Tave}, \code{Tmin}, \code{Tmax}) is a vector or a matrix, data will
#' be treated as a sequence of monthly values starting in January. If it is a time series then the 
#' function \code{\link{cycle}} will be used to determine the position of each observation within the 
#' year (month), allowing the data to start in a month different than January.
#'
#'
#' @return A time series with the values of monthly potential or reference evapotranspiration, in mm. 
#' If the input is a matrix or a multivariate time series each column will be treated as independent 
#' data (e.g., diferent observatories), and the output will be a multivariate time series.
#'
#'
#' @references 
#' Thornthwaite, C. W. (1948). An approach toward a rational classification of climate. 
#' \emph{Geographical Review} \bold{38}: 55–94. doi:10.2307/2107309.
#' 
#' Hargreaves G.H. 1994. Defining and using reference evapotranspiration. 
#' \emph{Journal of Irrigation and Drainage Engineering} \bold{120}: 1132–1139.
#' 
#' Droogers P., Allen R. G., 2002. Estimating reference evapotranspiration under inaccurate data conditions. 
#' \emph{Irrigation and Drainage Systems} \bold{16}: 33–45.
#' 
#' Allen R. G., Smith M., Pereira L. S., Perrier A., 1994. An update for the calculation of reference 
#' evapotranspiration. \emph{ICID Bulletin of the International Commission on Irrigation and Drainage}, 35–92.
#' 
#' Allen R.G., Pereira L.S.,Raes D., Smith, M. 1998. \emph{JCrop evapotranspiration - Guidelines for 
#' computing crop water requirements - FAO Irrigation and drainage paper 56}. FAO, Rome. ISBN 92-5-104219-5.
#' 
#' Walter I.A. and 14 co-authors, 2002. The ASCE standardized reference evapotranspiration equation. 
#' Rep. Task Com. on Standardized Reference Evapotranspiration July 9, 2002, EWRI–Am. Soc. Civil Engr., 
#' Reston, VA, 57 pp.
#'
#'
#' @author Santiago Begueria
#' 
#' 
#' @examples
#' # Load data for Tampa, lat=37.6475N, elevation=402.6 m. a.s.l.
#' # Data consists on monthly values since January 1980
#' data(wichita)
#' attach(wichita)
#' names(wichita)
#' 
#' # PET according to Thornthwaite
#' tho <- thornthwaite(TMED,37.6475)
#' # Hargreaves
#' har <- hargreaves(TMIN,TMAX,lat=37.6475)
#' # Penman, based on sun hours, ignore NAs
#' pen <- penman(TMIN,TMAX,AWND,tsun=TSUN,lat=37.6475,z=402.6,na.rm=TRUE)
#' # Penman, based on cloud cover
#' pen2 <- penman(TMIN,TMAX,AWND,CC=ACSH,lat=37.6475,z=402.6,na.rm=TRUE)
#' # Plot them together
#' plot(cbind(tho,har,pen,pen2))
#' 
#' # Now consider the data started in June 1900
#' thornthwaite(ts(TMED,start=c(1900,6),frequency=12),37.6475)
#' 
#' # Comparison with example from Allen et al. (1998), p. 69, fig. 18:
#' # Data from Cabinda, Angola (-5.33S, 12.11E, 20 m a.s.l.)
#' data(cabinda)
#' pen.cab <- penman(cabinda$Tmin,cabinda$Tmax,cabinda$U2,
#' 	Rs=cabinda$Rs,tsun=cabinda$tsun,RH=cabinda$RH,lat=-5.33,z=20)
#' plot(cabinda$ET0,pen.cab)
#' abline(0,1,lt='dashed')
#' summary(lm(pen.cab~cabinda$ET0))$r.squared
#'
#'
#' @importFrom stats cycle frequency ts start
#' 
#' 
#' @export
#' 
hargreaves <-
function(Tmin, Tmax, Ra=NA, lat=NA, Pre=NA, na.rm=FALSE) {

	if (length(Ra)==1 && length(lat)!=ncol(as.matrix(Tmin))){
		stop('Error: lat should be specified for estimating external radiation if Ra is not provided, and should have the same number of elements than Tmin.')
	}
	if (sum(is.na(Tmin),is.na(Tmax))!=0 && na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	if (length(Ra)>1 && !anyNA(Ra) && na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	if (length(Tmin)!=length(Tmax)) {
		stop('Error: Tmin and Tmax must be of the same lenght')
	}
	if (length(Ra)>1 && length(Ra)!=length(Tmin)) {
		stop('Error: Ra must be of the same lenght than Tmin and Tmax')
	}
	if (is.ts(Tmin) && frequency(Tmin)!=12) {
		stop('Error: Data should be a monthly time series (frequency = 12)')
	}
	
	if (!is.ts(Tmin)) {
		Tmin <- ts(as.matrix(Tmin),frequency=12)
	} else {
		Tmin <- ts(as.matrix(Tmin),frequency=frequency(Tmin),start=start(Tmin))
	}
	n <- nrow(Tmin)
	m <- ncol(Tmin)
	c <- cycle(Tmin)
	ET0 <- Tmin*NA

	# mean temperature, ºC
	T <- (Tmin+Tmax)/2
	
	# temperature range, ºC
	Tr <- Tmax-Tmin
	Tr <- pmax(0, Tr)
	
	# external radiation, MJ m-2 d-1
	if (length(Ra)==1) {
		# estimate Ra, following Allen et al. (1994)
		# number of day in the year
		J <- as.integer(30.5*c-14.6)
		# solar declination, rad (1 rad = 57.2957795 deg)
		delta <- 0.409*sin(0.0172*J-1.39)
		# relative distance Earth-Sun, []
		dr <- 1 + 0.033*cos(0.0172*J)
		# sunset hour angle, rad
		latr <- lat/57.2957795
		sset <- -tan(latr)*tan(delta)
		omegas <- sset*0
		omegas[abs(sset)<=1] <- acos(sset[abs(sset)<=1])
		# correction for high latitudes
		omegas[sset<(-1)] <- max(omegas)
		# Ra, MJ m-2 d-1
		Ra <- 37.6*dr*(omegas*sin(latr)*sin(delta)+cos(latr)*cos(delta)*sin(omegas))
		Ra <- ifelse(Ra<0,0,Ra)
	}
	
	# Daily ET0, mm day-1
	if (length(Pre)!=n) {
		# Use original Hargreaves
		ET0 <- 0.0023 * 0.408*Ra * (T+17.8) * Tr^0.5
	} else {
		# Use modified method
		ab <- Tr-0.0123*Pre
		ET0 <- 0.0013 * 0.408*Ra * (T+17.0) * ab^0.76
		ET0[is.nan(ab^0.76)] <- 0
	}
	ET0 <- ifelse(ET0<0,0,ET0)

	# Transform ET0 to mm month-1
	mlen <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
	ET0 <- mlen[c]*ET0
	colnames(ET0) <- rep('ET0_har',m)

	return(ET0)
}