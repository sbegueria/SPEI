\name{Datasets}

\alias{wichita}
\alias{balance}
\alias{cabinda}

\title{Datasets for illustrating the functions in the SPEI package.}

\description{Data used in the examples of the SPEI package: \code{wichita} dataset: monthly climate in Wichita (Kansas, lat=37.6475, elevation=402.6 m. a.s.l.) since January 1980; \code{balance} dataset: monthly climatic water balance (precipitation minus potential evapotranspiration) at eleven locations around the World, since January 1900; \code{cabinda}: one year of data for computing Penman-Monteith ET0 from Allen et al. (1998).}

\usage{
data(wichita)
data(balance)
data(cabinda)
}

\format{
\code{wichita} dataset:
\describe{
\item{YEAR}{ monthly precipitation totals, in mm.}
\item{MONTH}{ monthly precipitation totals, in mm.}
\item{PRCP}{ monthly precipitation totals, in mm.}
\item{TMAX}{ monthly mean daily maximum temperature, in ºC.}
\item{TMIN}{ monthly mean daily minimum temperature, in ºC.}
\item{TMED}{ monthly mean temperature, in ºC.}
\item{AWND}{ monthly mean wind speed, in km h-1}
\item{ACSH}{ monthly mean sun hours, in h.}
\item{ACSH}{ monthly mean cloud cover, in \%.}
}

\code{balance} dataset: monthly climatic water balance (precipitation minus potential evapotranspiration) at Indore (India), Kimberley (South Africa), Albuquerque (US), Valencia (Spain), Wien (Austria),  Abashiri (Japan), Tampa (US), Sao Paulo (Brasil), Lahore (India), Punta Arenas (Chile) and Helsinki (Finland), in mm.

\code{cabinda} dataset: one year of monthly climatic data at Cabinda (Angola, -5.33S 12.11E 20 m), taken from Allen et al. (1998), page 69, figure 18:
\describe{
\item{mon}{ month of the year}
\item{Tmin}{ monthly mean daily minimum temperature, in ºC.}
\item{Tmax}{ monthly mean daily maximum temperature, in ºC.}
\item{RH}{ monthly mean relative humidity, in \%.}
\item{U2}{ monthly mean wind speed, in km h-1}
\item{tsun}{ monthly mean sunshine hours, in h.}
\item{Rs}{ monthly mean dialy incoming solar radiation, MJ m-2 d-1.}
\item{ET0}{ monthly ET0 from the original publication, in mm.}
}
}

\references{S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
R.G. Allen, L.S. Pereira, D. Raes, M. Smith. 1998. \emph{JCrop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}. FAO, Rome. ISBN 92-5-104219-5.}

\source{The \code{wichita} data were obtained from the Global Historical Climatology Network (GHCN, \url{http://www.ncdc.noaa.gov/oa/climate/ghcn-daily/}. Data for the \code{balance} dataset were extracted from CRU TS V3.1 and from the 20th Century Reanalysis V2 dataset.  Data for the \code{balance} dataset were taken from Allen et al. (1998), page 69, figure 18.}

\author{Data ported to R by S. Beguería.}

%\note{}

%\seealso{}

\examples{
data(wichita)
names(wichita)
summary(wichita)

data(balance)
summary(balance)
}

%\keyword{}
%\keyword{}
