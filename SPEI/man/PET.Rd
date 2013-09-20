\name{Potential evapotranspiration}

\alias{thornthwaite}
\alias{hargreaves}
\alias{penman}

\concept{reference evapotranspiration}
\concept{evaporation}
\concept{penman-monteith}

\title{Computation of potential evapotranspiration.}

\description{Potential evapotranspiration (PET) is the amount of evaporation and transpiration that would occur if a sufficient water source were available. Reference evapotranspiration (ET0) is the amount of evaporation and transpiration from a reference vegetation of grass. They are usually considered equivalent. This set of functions calculate PET or ET0 accordind to the Thornthwaite, Hargreaves or Penman-Monteith equations.}

\usage{
thornthwaite(Tave, lat, na.rm = FALSE)

hargreaves(Tmin, Tmax, Ra = NA, lat = NA, Pre = NA, na.rm = FALSE)

penman(Tmin, Tmax, U2, Ra = NA, lat = NA, Rs = NA, tsun = NA,
	CC = NA, ed = NA, Tdew = NA, RH = NA, P = NA, P0 = NA,
	z = NA, crop='short', na.rm = FALSE)
}

\arguments{
  \item{Tave}{ a numeric vector, matrix or time series of monthly mean temperatures, ºC.}
  \item{lat}{ a numeric vector with the latitude of the site or sites, in degrees.}
  \item{na.rm}{ optional, a logical value indicating whether NA values should be stripped from the computations.}
  \item{Tmax}{ a numeric vector, matrix or time series of monthly mean daily maximum temperatures, ºC.}
  \item{Tmin}{ a numeric vector, matrix or time series of monthly mean daily minimum temperatures, ºC.}
  \item{Ra}{ optional, a numeric vector, matrix or time series of monthly mean daily external radiation, MJ m-2 d-1.}
  \item{Pre}{ optional, a numeric vector, matrix or time series  of monthly total precipitation, mm.}
  \item{U2}{ a numeric vector, matrix or time series of monthly mean daily wind speeds at 2 m height, m s-1.} 
  \item{Rs}{ optional, a numeric vector, matrix or time series of monthly mean dialy incoming solar radiation, MJ m-2 d-1.} 
  \item{tsun}{ optional, a numeric vector, matrix or time series of monthly mean daily bright sunshine hours, h.} 
  \item{CC}{ optional, numeric a vector, matrix or time series of monthly mean cloud cover, \%.}
  \item{ed}{ optional, numeric a vector, matrix or time series of monthly mean actual vapour pressure at 2 m height, kPa.}
  \item{Tdew}{ optional, a numeric vector, matrix or time series of monthly mean daily dewpoint temperature (used for estimating ed), ºC}
  \item{RH}{ optional, a numeric vector, matrix or time series of monthly mean relative humidity (used for estimating ed), \%.}
  \item{P}{ optional, a numeric vector, matrix or time series of monthly mean atmospheric pressure at surface, kPa.}
  \item{P0}{ optional, a numeric vector, matrix or time series of monthly mean atmospheric pressure at sea level (used for estimating P), kPa.}
  \item{z}{ optional, a numeric vector of the elevation of the site or sites, m above sea level.}
  \item{crop}{ optional, character string, type of reference crop. Either one of 'short' (default) or 'tall'.}
}

\details{
\code{thornthwaite} computes the monthly potential evapotranspiration (PE) according to the Thornthwaite (1948) equation. It is the simplest of the three methods, and can be used when only temperature data are available.

\code{hargreaves} computes the monthly reference evapotranspiration (ET0) of a grass crop based on the original Hargreaves equation (1994). However, if precipitation data \code{Pre} is provided a modified form due to Droogers and Allen (2002) will be used; this equation corrects ET0 using the amount of rain of each month as a proxy for insolation. The Hargreaves method requires data on the mean external radiation, \code{Ra}. If such data are not available it can be estimated from the latitude \code{lat} and the month of the year.

\code{penman} calculates the monthly reference evapotranspiration (ET0) of a hypothetical reference crop according to the FAO-56 Penman-Monteith equation described in Allen et al. (1994). This is a simplification of the original Penman-Monteith equation, and has found widespread use. By default the original parameterization of Allen et al. (1994) is used, corresponding to a short reference crop of 0.12 m height. Parameterization for a tall reference crop of 0.5 m height due to Walter et al. (2002) can also be used, by setting the \code{crop} parameter to 'tall'. The method requires data on the incoming solar radiation, \code{Rs}; since this is seldom available, the code will estimate it from data on the bright sunshine duration \code{tsun}, or alternatively from data on the percent cloud cover \code{CC}. Similarly, if data on the saturation water pressure \code{ed} are not available, it is possible to estimate it from the dewpoint temperature \code{Tdew}, from the relative humidity \code{RH} or even from the minimum temperature \code{Tmin} (sorted from least to most uncertain method). Similarly, the atmospheric surface pressure \code{P} required for computing the psychrometric constant can be calculated from the atmospheric pressure at sea level \code{P0} and the elevation \code{z}, or else it will be assumed to be constant (101.3 kPa). The code will produce an error message if a valid combination of input parameters is not provided.

If the main input object (\code{Tave}, \code{Tmin}, \code{Tmax}) is a vector or a matrix, data will be treated as a sequence of monthly values starting in January. If it is a time series then the function \code{\link{cycle}} will be used to determine the position of each observation within the year (month), allowing the data to start in a month different than January.
}

\value{A time series with the values of monthly potential or reference evapotranspiration, in mm. If the input is a matrix or a multivariate time series each column will be treated as independent data (e.g., diferent observatories), and the output will be a multivariate time series.
}

\references{
Thornthwaite, C. W. (1948). An approach toward a rational classification of climate. \emph{Geographical Review} \bold{38}: 55–94. doi:10.2307/2107309.

Hargreaves G.H. 1994. Defining and using reference evapotranspiration. \emph{Journal of Irrigation and Drainage Engineering} \bold{120}: 1132–1139.

Droogers P., Allen R. G., 2002. Estimating reference evapotranspiration under inaccurate data conditions. \emph{Irrigation and Drainage Systems} \bold{16}: 33–45.

Allen R. G., Smith M., Pereira L. S., Perrier A., 1994. An update for the calculation of reference evapotranspiration. \emph{ICID Bulletin of the International Commission on Irrigation and Drainage}, 35–92.

Allen R.G., Pereira L.S.,Raes D., Smith, M. 1998. \emph{JCrop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}. FAO, Rome. ISBN 92-5-104219-5.

Walter I.A. and 14 co-authors, 2002. The ASCE standardized reference evapotranspiration equation. Rep. Task Com. on Standardized Reference Evapotranspiration July 9, 2002, EWRI–Am. Soc. Civil Engr., Reston, VA, 57 pp.
}

\author{Santiago Beguería}

%\note{}

%\seealso{}

\examples{
# Load data for Tampa, lat=37.6475N, elevation=402.6 m. a.s.l.
# Data consists on monthly values since January 1980
data(wichita)
attach(wichita)
names(wichita)

# PET according to Thornthwaite
tho <- thornthwaite(TMED,37.6475)
# Hargreaves
har <- hargreaves(TMIN,TMAX,lat=37.6475)
# Penman, based on sun hours, ignore NAs
pen <- penman(TMIN,TMAX,AWND,tsun=TSUN,lat=37.6475,z=402.6,na.rm=TRUE)
# Penman, based on cloud cover
pen2 <- penman(TMIN,TMAX,AWND,CC=ACSH,lat=37.6475,z=402.6,na.rm=TRUE)
# Plot them together
plot(cbind(tho,har,pen,pen2))

# Now consider the data started in June 1900
thornthwaite(ts(TMED,start=c(1900,6),frequency=12),37.6475)

# Comparison with example from Allen et al. (1998), p. 69, fig. 18:
# Data from Cabinda, Angola (-5.33S, 12.11E, 20 m a.s.l.)
data(cabinda)
pen.cab <- penman(cabinda$Tmin,cabinda$Tmax,cabinda$U2,
	Rs=cabinda$Rs,tsun=cabinda$tsun,RH=cabinda$RH,lat=-5.33,z=20)
plot(cabinda$ET0,pen.cab)
abline(0,1,lt='dashed')
summary(lm(pen.cab~cabinda$ET0))$r.squared
}

%\keyword{}
%\keyword{}
