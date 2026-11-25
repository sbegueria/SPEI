#' @name Datasets
#' @aliases wichita balance cabinda cruts4 tarazona
#' @title Data sets for illustrating the functions in the SPEI package
#' @keywords datasets
#' @usage
#' data(wichita)
#' data(balance)
#' data(cabinda)
#' data(cruts4)
#' data(tarazona)
#' @description
#' Data used in the examples of the SPEI package:
#' \code{wichita} dataset: monthly climate in Wichita (Kansas, lat=37.6475,
#' elevation=402.6 m a.s.l.) from January 1980 to October 2011;
#' \code{balance} dataset: monthly climatic water balance (precipitation
#' minus potential evapotranspiration) at eleven locations around the World,
#' since January 1900 (1296 months);
#' \code{cabinda}: one year of data for computing Penman-Monteith ET0 from
#' Allen et al. (1998);
#' \code{cruts4}: 120 years of monthly climatic water balance (precipitation
#' minus reference evapotranspiration) data at six grid points from
#' CRU TS 4.05;
#' \code{tarazona}: annual basal area increment of 18 cores of Aleppo pine
#' (\emph{Pinus halepensis}) at Tarazona (Spain) from 1959 to 2017, and
#' climate at the same site with 48 periods per year (four per month) from
#' 1961 to 2020.
#' @details See description.
#' @format
#' \code{wichita} dataset: a data frame with:
#' \describe{
#'   \item{YEAR}{ year.}
#'   \item{MONTH}{ month of the year (1 to 12).}
#'   \item{PRCP}{ monthly precipitation totals, in mm.}
#'   \item{TMAX}{ monthly mean daily maximum temperature, in ºC.}
#'   \item{TMIN}{ monthly mean daily minimum temperature, in ºC.}
#'   \item{TMED}{ monthly mean temperature, in ºC.}
#'   \item{AWND}{ monthly mean wind speed, in m s-1.}
#'   \item{TSUN}{ monthly mean daily sunshine duration, in h. Note: five
#'   values between October 2000 and May 2001 (15.0 to 21.1 h) exceed the
#'   maximum possible day length at this latitude and should be treated with
#'   caution.}
#'   \item{ACSH}{ monthly mean cloud cover, in \%.}
#' }
#' \code{balance} dataset: a data frame with monthly climatic water balance
#'  (precipitation minus potential evapotranspiration) at Indore (India),
#'  Kimberley (South Africa), Albuquerque (US), Valencia (Spain),
#'  Wien (Austria), Abashiri (Japan), Tampa (US), Sao Paulo (Brazil),
#'  Lahore (Pakistan), Punta Arenas (Chile) and Helsinki (Finland), in mm.
#' \code{cabinda} dataset: a data frame with one year of monthly climatic data
#' at Cabinda (Angola, -5.33S 12.11E 20 m), with:
#'
#' \describe{
#'   \item{mon}{ month of the year}
#'   \item{Tmin}{ monthly mean daily minimum temperature, in ºC.}
#'   \item{Tmax}{ monthly mean daily maximum temperature, in ºC.}
#'   \item{RH}{ monthly mean relative humidity, in \%.}
#'   \item{U2}{ monthly mean wind speed at 2 m height, in m s-1.}
#'   \item{tsun}{ monthly mean sunshine hours, in h.}
#'   \item{Rs}{ monthly mean daily incoming solar radiation, MJ m-2 d-1.}
#'   \item{ET0}{ monthly ET0 from the original publication, in mm.}
#' }
#'
#' \code{cruts4} dataset: an array  with 120 years of monthly climatic water
#' balance (precipitation minus reference evapotranspiration) data at six grid
#' points from CRU TS 4.05 data set. The array has dimensions [time=1440,
#' longitude=2, latitude=3], with time starting in January 1900. Longitudes
#' are (0.25, 0.75), and latitudes (42.25, 42.75, 43.25), corresponding to the
#' Central Pyrenees between Spain and France.
#'
#' \code{tarazona} dataset: a list with three elements, with data from the
#' unthinned (control) plot of an Aleppo pine (\emph{Pinus halepensis})
#' plantation at Tarazona (Zaragoza, Spain; lat=41.8667, lon=-1.8333,
#' elevation=785 m a.s.l.):
#' \describe{
#'   \item{site}{ a list with the site metadata: \code{name},
#'   \code{lat}, \code{lon}, \code{elevation} (m a.s.l.), \code{species},
#'   \code{plot} (the plot code, TAPHD), \code{treatment} (control,
#'   i.e. unthinned) and \code{thinning.year} (1998, the year in which the
#'   neighbouring plots of the experiment were thinned; the control plot was
#'   not thinned).}
#'   \item{bai}{ a data frame with the annual basal area increment (BAI,
#'   in cm2 per year) of the control plot. The first column, \code{year},
#'   has the years (1959 to 2017), and the other 18 columns the BAI of each
#'   core, \code{NA} outside the years covered by the core. The cores come
#'   from 11 trees. The column names are the plot code, the tree number
#'   and the core letter, e.g. \code{TAPHD06A} and \code{TAPHD06B} are two
#'   cores of tree TAPHD06. BAI has an age and size trend, which must be
#'   removed before relating it to climate (see
#'   \code{vignette("scale_scan")}).}
#'   \item{clim}{ a multivariate time series (\code{ts}) with frequency 48
#'   (four periods per month), from the first period of January 1961 to the
#'   last period of December 2020 (2880 periods). The periods of each month
#'   start on days 1, 9, 16 and 23, so they span days 1-8, 9-15, 16-22, and
#'   23 to the end of the month. The columns are \code{PRCP}, precipitation
#'   in mm per period; \code{ETO}, FAO-56 Penman-Monteith reference
#'   evapotranspiration in mm per period; \code{TMIN} and \code{TMAX}, mean
#'   daily minimum and maximum temperature in the period, in ºC. Values are
#'   rounded to two decimals.}
#' }
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar
#' drought index sensitive to global warming: The Standardized Precipitation
#' Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696,
#' DOI: 10.1175/2009JCLI2909.1.
#'
#' R.G. Allen, L.S. Pereira, D. Raes, M. Smith. 1998. \emph{Crop
#' evapotranspiration - Guidelines for computing crop water requirements - FAO
#' Irrigation and drainage  paper 56}. FAO, Rome. ISBN 92-5-104219-5.
#'
#' À. Manrique-Alba, S. Beguería, A.J. Molina, M. González-Sanchis, M.
#' Tomàs-Burguera, A.D. del Campo, M. Colangelo, J.J. Camarero. 2020.
#' Long-term thinning effects on tree growth, drought response and water use
#' efficiency at two Aleppo pine plantations in Spain. \emph{Science of the
#' Total Environment} \bold{728}: 138536,
#' DOI: 10.1016/j.scitotenv.2020.138536.
#'
#' À. Manrique-Alba, S. Beguería, J.J. Camarero, A.J. Molina, et al. 2025.
#' Adaptive responses to thinning: growth and sensitivity to hydric and
#' thermal stress in four widely planted pine species. \emph{Agricultural
#' and Forest Meteorology} \bold{373}: 110777,
#' DOI: 10.1016/j.agrformet.2025.110777.
#'
#' S.M. Vicente-Serrano, M. Tomas-Burguera, S. Beguería, F. Reig, B.
#' Latorre, M. Peña-Gallardo, M.Y. Luna, A. Morata, J.C. González-Hidalgo.
#' 2017. A high resolution dataset of drought indices for Spain. \emph{Data}
#' \bold{2}(3): 22, DOI: 10.3390/data2030022.
#'
#'
#' @source
#' The \code{wichita} data were obtained from the Global Historical Climatology
#' Network (GHCN, \url{https://www.ncei.noaa.gov/}). Data for the \code{balance}
#' dataset were extracted from CRU TS V3.1 and from the 20th Century Reanalysis
#' V2 data set.
#' Data for the \code{cabinda} dataset were taken from Allen et al. (1998),
#' page 69, figure 18.
#' The \code{cruts4} data were obtained from the CRU (Climatic Research Unit,
#' University of East Anglia \url{https://crudata.uea.ac.uk/}) TS V4.05
#' data set.
#' The basal area increment in the \code{tarazona} dataset comes from
#' Manrique-Alba et al. (2020), and is part of the dataset: Manrique-Alba,
#' À., Molina, A.J., Beguería, S., Camarero, J.J., et al. (2025). Basal Area
#' Increment and Climate Data from Long-Term Silviculture Monitoring Sites in
#' Spain (SilvAdapt.net) [Dataset]. DIGITAL.CSIC,
#' \doi{10.20350/digitalcsic/17165}. The same data were used in
#' Manrique-Alba et al. (2025). The climate data were extracted from the
#' gridded dataset of the Spanish drought monitor
#' (\url{https://monitordesequia.csic.es}, Vicente-Serrano et al., 2017).
#' The \code{tarazona} data are distributed with the package, under its
#' licence (GPL (>= 2)), by the data authors. The script used to build the
#' dataset is \code{data-raw/tarazona.R} in the package source repository.
#'
#' @author Data ported to R by S. Beguería.
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
#' data(tarazona)
#' str(tarazona)
#'
NULL

#' @rdname Datasets
#' @format NULL
#' @usage NULL
"wichita"

#' @rdname Datasets
#' @format NULL
#' @usage NULL
"cabinda"

#' @rdname Datasets
#' @format NULL
#' @usage NULL
"balance"

#' @rdname Datasets
#' @format NULL
#' @usage NULL
"cruts4"

#' @rdname Datasets
#' @format NULL
#' @usage NULL
"tarazona"
