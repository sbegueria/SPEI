#' @name Datasets
#' @aliases wichita balance cabinda cruts4 campinas
#' @title Data sets for illustrating the functions in the SPEI package.
#' @keywords datasets
#' @description 
#' Data used in the examples of the SPEI package: 
#' \code{wichita} dataset: monthly climate in Wichita (Kansas, lat=37.6475, 
#' elevation=402.6 m. a.s.l.) since January 1980; 
#' \code{balance} dataset: monthly climatic water balance (precipitation 
#' minus potential evapotranspiration) at eleven locations around the World, 
#' since January 1900; 
#' \code{cabinda}: one year of data for computing Penman-Monteith ET0 from
#' Allen et al. (1998);
#' \code{cruts4}: 120 years of monthly climatic water balance (precipitation 
#' minus reference evapotranspiration) data at six grid points from CRU TS 4.05.
#' \code{campinas} dataset: weekly precipitation for Campinas (SP, Brazil,
#' lat = -23, lon -47) from 1981 to Nov 2022. Data extreacted fom CHIRPS dataset. 
#' @details See description.
#' @format 
#' \code{wichita} dataset: a data frame with:
#' \describe{
#'   \item{YEAR}{ monthly precipitation totals, in mm.}
#'   \item{MONTH}{ monthly precipitation totals, in mm.}
#'   \item{PRCP}{ monthly precipitation totals, in mm.}
#'   \item{TMAX}{ monthly mean daily maximum temperature, in ºC.}
#'   \item{TMIN}{ monthly mean daily minimum temperature, in ºC.}
#'   \item{TMED}{ monthly mean temperature, in ºC.}
#'   \item{AWND}{ monthly mean wind speed, in km h-1}
#'   \item{ACSH}{ monthly mean sun hours, in h.}
#'   \item{ACSH}{ monthly mean cloud cover, in \%.}
#'}
#' \code{balance} dataset: a data frame with monthly climatic water balance
#'  (precipitation minus potential evapotranspiration) at Indore (India),
#'  Kimberley (South Africa), Albuquerque (US), Valencia (Spain), Wien (Austria),
#'  Abashiri (Japan), Tampa (US), Sao Paulo (Brazil), Lahore (India), Punta
#'  Arenas (Chile) and Helsinki (Finland), in mm.
#' \code{cabinda} dataset: a data frame with one year of monthly climatic data
#' at Cabinda (Angola, -5.33S 12.11E 20 m), with:
#'  
#' \describe{
#'   \item{mon}{ month of the year}
#'   \item{Tmin}{ monthly mean daily minimum temperature, in ºC.}
#'   \item{Tmax}{ monthly mean daily maximum temperature, in ºC.}
#'   \item{RH}{ monthly mean relative humidity, in \%.}
#'   \item{U2}{ monthly mean wind speed, in km h-1}
#'   \item{tsun}{ monthly mean sunshine hours, in h.}
#'   \item{Rs}{ monthly mean diaily incoming solar radiation, MJ m-2 d-1.}
#'   \item{ET0}{ monthly ET0 from the original publication, in mm.}
#'}
#'
#' \code{cruts4} dataset: an array  with 120 years of monthly climatic water
#' balance (precipitation minus reference evapotranspiration) data at six grid
#' points from CRU TS 4.05 data set. The array has dimensions [time=1440, 
#' longitude=2, latitude=3], with time starting in January 1900. Longitudes
#' are (0.25, 0.75), and latitudes (42.25, 42.75, 43.25), corresponding to the
#' Central Pyrenees between Spain and France.
#'
#' \code{campinas} dataset: weekly precipitation for Campinas (SP, Brazil,
#' lat = -23, lon -47) from 1981 to Nov 2022.
#' Data aggregated using \code{lubridate::week} rules, so every year has 53 weeks.
#' Must be converted to a TS object before extractin SPI.
#' Daily precipitation obtained from CHIRPS v2.0 dataset on 2022-12-19.
#' URL: https://www.chc.ucsb.edu/data/chirps
#' see \code{data-raw/campinas_daily2weekly.R} for daily data processing steps
#' 
#' \describe{
#'   \item{year_week}{ year and week, followin ISO 8601 definitions}
#'   \item{date}{ first day of the year-week period, date}
#'   \item{prcp}{ accumulated precipitation for the year-week period, mm}
#'}
#'
#' @references 
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar 
#' drought index sensitive to global warming: The Standardized Precipitation 
#' Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696, 
#' DOI: 10.1175/2009JCLI2909.1.
#' 
#' R.G. Allen, L.S. Pereira, D. Raes, M. Smith. 1998. \emph{JCrop evapotranspiration
#'  - Guidelines for computing crop water requirements - FAO Irrigation and drainage 
#' paper 56}. FAO, Rome. ISBN 92-5-104219-5.
#'
#'
#' @source 
#' The \code{wichita} data were obtained from the Global Historical Climatology Network 
#' (GHCN, \url{http://www.ncdc.noaa.gov/oa/climate/ghcn-daily/}. Data for the \code{balance} 
#' dataset were extracted from CRU TS V3.1 and from the 20th Century Reanalysis V2 data set.
#' Data for the \code{balance} dataset were taken from Allen et al. (1998), page 69, figure 18.
#' The \code{cruts4} data were obtained from the CRU (Climatic Research Unit, University
#' of East Anglia \url{https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/}) TS V4.05 data set.
#' Data for the \code{campinas} dataset were aggregated from daily 
#'    CHIRPS v2.0 precipitation - https://www.chc.ucsb.edu/data/chirps 
#'
#' @author Data ported to R by S. Beguería. \code{campinas} data prepared by Daniel C. Victoria
#' 
#' 
#' @examples 
#' data(wichita)
#' names(wichita)
#' summary(wichita)
#' data(balance)
#' summary(balance)
#' data(cruts4)
#' summary(cruts4)
#'
NULL

#' @rdname wichita
"wichita"

#' @rdname cabinda
"cabinda"

#' @rdname balance
"balance"

#' @rdname cruts4
"cruts4"

#' @rdname campinas
#' @example 
#' data(campinas)
#' campinas_ts <- ts(campinas$prcp, frequency = 53, start = c(1981, 1))
#' spi(campinas_ts, scale = 12)
"campinas"