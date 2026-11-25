#' @name Potential-evapotranspiration
#' @title Computation of potential and reference evapotranspiration
#' @aliases thornthwaite penman
#' @usage
#' thornthwaite(Tave, lat, na.rm = FALSE, verbose=TRUE)
#'
#' hargreaves(Tmin, Tmax, Ra = NULL, lat = NULL, Pre = NULL, na.rm = FALSE,
#' verbose=TRUE, dates = NULL)
#'
#' penman(Tmin, Tmax, U2 = NULL, Ra = NULL, lat = NULL, Rs = NULL, tsun = NULL,
#'        CC = NULL, ed = NULL, Tdew = NULL, RH = NULL, P = NULL, P0 = NULL,
#'        CO2 = NULL, z = NULL, crop='short', na.rm = FALSE, method='ICID',
#'        verbose=TRUE, dates = NULL, kRs = NULL, aT = 0)
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
#' @param U2   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean daily wind speeds at 2 m height, m s-1. No height
#' conversion is applied, so wind speeds measured at other heights must be
#' converted to 2 m by the user (e.g., FAO-56 eq. 47). If not provided, a
#' constant value of 2 m s-1 is used. Values below 0.5 m s-1 are set to
#' 0.5 m s-1 (Pereira et al., 2025, p. 105).
#' @param Rs   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean daily incoming solar radiation, MJ m-2 d-1.
#' @param tsun   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean daily bright sunshine hours, h.
#' @param CC   optional, numeric a vector, matrix or time series of monthly
#' mean cloud cover, \%.
#' @param ed   optional, a numeric vector, tsvector, matrix, tsmatrix, or 3-d
#' array of monthly mean actual vapour pressure at 2 m height, kPa.
#' @param Tdew   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean daily dewpoint temperature (used for
#' estimating ed), ºC.
#' @param RH   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean relative humidity (used for estimating ed), \%.
#' @param P   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean atmospheric pressure at the surface, kPa.
#' If not provided, it is estimated from \code{P0} and \code{z}, or from
#' \code{z} alone assuming a sea level pressure of 101.3 kPa (FAO-56 eq. 7).
#' @param P0   optional, a numeric vector, tsvector, matrix, tsmatrix,
#' or 3-d array of monthly mean atmospheric pressure at sea level (used
#' together with \code{z} for estimating \code{P}), kPa.
#' @param CO2   optional, a single numeric value, a numeric vector,
#' or a tsvector of monthly mean CO2 atmospheric concentration, ppm. If not
#' provided, no CO2 correction is applied (equivalent to 300 ppm).
#' @param z   a numeric vector or matrix of the elevation of the
#' site or sites, m above sea level. Required by \code{penman}, where it is
#' used for the clear-sky solar radiation and, unless \code{P} is given, for
#' the atmospheric surface pressure.
#' @param crop   optional, character string, type of reference crop. Either
#' one of 'short' (default) or 'tall'.
#' @param method   optional, character string, Penman-Monteith calculation
#' method. Either one of 'ICID' (default), 'FAO', or 'ASCE'.
#' @param verbose   optional, logical, report the computation options during
#' calculation. Either 'TRUE' (default) or 'FALSE'. The report is issued as
#' a message, so it can also be silenced with \code{suppressMessages()}.
#' @param dates   optional, a \code{Date} vector (or a character vector that
#' can be converted with \code{as.Date()}) with the date of each time step,
#' i.e. one element per row (first dimension) of the input data. Only used
#' by \code{hargreaves} and \code{penman}. If provided, the computation is
#' done at a daily time step: the inputs are daily values (instead of
#' monthly means and totals) and the output is ETo in mm per day. The dates
#' must not contain missing values and must be in strictly increasing order;
#' gaps between dates are allowed. If \code{NULL} (the default), the data
#' are monthly. See Details.
#' @param kRs   optional, a single numeric value, or one value per site (a
#' vector, or a matrix for 3-d array input), with the radiation adjustment
#' coefficient of the temperature-only method (Pereira et al., 2025, eq.
#' 3.52), ºC^-0.5. Only used by \code{penman} when no radiation data
#' (\code{Rs}, \code{tsun} or \code{CC}) are given; otherwise it is ignored
#' with a warning. It must be between 0.1 and 0.35. If \code{NULL} (the
#' default), 0.16 is used. See Details.
#' @param aT   optional, a single numeric value, or one value per site (a
#' vector, or a matrix for 3-d array input), with the difference between the
#' minimum temperature and the dewpoint temperature, ºC (Pereira et al.,
#' 2025, eq. 2.6). Only used by \code{penman} when the actual vapour
#' pressure is estimated from \code{Tmin} (no \code{ed}, \code{Tdew} or
#' \code{RH} given); otherwise a non-zero value is ignored with a warning.
#' It must be between -5 and 10. The default, 0, assumes that the
#' dewpoint temperature equals the minimum temperature. See Details.
#'
#'
#' @details \code{thornthwaite} computes the monthly potential
#' evapotranspiration (PE) according to the Thornthwaite (1948) equation.
#' It is the simplest of the three methods, and can be used when only
#' mean temperature data are available.
#' The annual heat index is computed from the mean temperature of each
#' calendar month, so at least 12 monthly values are required.
#' Thornthwaite's original method uses a separate table for monthly mean
#' temperatures of 26.5 ºC or higher. This implementation applies the general
#' equation at all temperatures, which gives somewhat different values in
#' that range (about 2\% at 27.3 ºC).
#'
#'
#' \code{hargreaves} computes the monthly reference evapotranspiration (ETo)
#' of a grass crop based on the original Hargreaves equation (Hargreaves and
#' Samani, 1985). However, if precipitation data \code{Pre} is provided a
#' modified form due to Droogers and Allen (2002) will be used; this equation
#' corrects ETo using the amount of rain of each month as a proxy for
#' irradiation. The Hargreaves method requires data on the mean external
#' radiation, \code{Ra}. If \code{Ra} is not available it can be estimated
#' from the latitude \code{lat} and the month of the year.
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
#' al., 2002) is used when setting it to `ASCE`.
#' By default the original parameterization corresponding to a short reference
#' crop of 0.12 m height is used, although the parameterization for a tall
#' reference crop of 0.5 m height (Walter et al., 2002) can also be used,
#' by setting the \code{crop} parameter to 'tall'.
#' The method requires data on the incoming solar radiation, \code{Rs};
#' since this is seldom available, the code will estimate it from data on the
#' bright sunshine duration \code{tsun}, or alternatively from data on the
#' percent cloud cover \code{CC}. If none of \code{Rs}, \code{tsun} or
#' \code{CC} is given (or they contain only missing values), \code{Rs} is
#' estimated from the temperature range (see below). Similarly, if data on
#' the actual vapour pressure \code{ed} are not available, it is possible to
#' estimate it from the dew point temperature \code{Tdew}, from the relative
#' humidity \code{RH} or even from the minimum temperature \code{Tmin}
#' (sorted from least to most uncertain method), as the saturation vapour
#' pressure at Tdew = Tmin - \code{aT}. Similarly, the atmospheric surface pressure \code{P}
#' required for computing the psychrometric constant can be calculated from the
#' atmospheric pressure at sea level \code{P0} and the elevation \code{z},
#' or else from the elevation \code{z} assuming a sea level pressure of
#' 101.3 kPa (FAO-56 eq. 7). The elevation \code{z} is always required by
#' \code{penman} (there is no default value), since it is also used to
#' compute the clear-sky solar radiation, Rso = (0.75 + 2e-5 z) Ra. The
#' ratio Rs/Rso used in the net longwave radiation term is limited to 1.
#' Wind speed \code{U2} must refer to 2 m height, as no height conversion is
#' applied. If no wind speed data \code{U2} are available, a constant value
#' of 2 m s-1 is used. Wind speeds below 0.5 m s-1 are set to 0.5 m s-1,
#' with all three methods, following Pereira et al. (2025, p. 105): under
#' calm conditions the buoyancy of warm air still promotes the exchange of
#' vapour at the surface.
#' Custom CO2 atmospheric concentration \code{CO2} can also be provided,
#' following Yang et al. (2019): the term 0.00024 (CO2 - 300) is added to the
#' wind coefficient of the denominator (0.34 for the short crop, 0.38 for the
#' tall crop). This correction was derived for the short reference crop, but
#' it is applied to whichever crop is selected, including 'tall'.
#' With monthly data, the soil heat flux G is computed from the difference
#' in mean temperature between the previous and the following months
#' (FAO-56 eq. 43), using forward or backward differences at the first and
#' last time steps. \code{penman} works with series of any length; with a
#' single time step G is set to 0. With daily data (\code{dates}), G is
#' always 0.
#'
#' \strong{FAO-56 revision 1 and the temperature-only method.} The revision
#' of FAO-56 (Pereira et al., 2025) keeps the FAO Penman-Monteith equation
#' and its components unchanged from Allen et al. (1998). It recommends the
#' Penman-Monteith temperature approach (PMT) over the Hargreaves-Samani
#' equation (\code{hargreaves}) when only maximum and minimum temperatures
#' are available, because the Penman-Monteith equation keeps the physical
#' relations between the weather variables. In PMT the missing variables are
#' estimated as follows, and \code{penman} does so when only \code{Tmin} and
#' \code{Tmax} are given:
#' \itemize{
#'   \item Solar radiation: Rs = kRs sqrt(Tmax - Tmin) Ra (Pereira et al.,
#'   2025, eq. 3.52), limited to Rso. The coefficient \code{kRs} was
#'   originally 0.16 for interior locations and 0.19 for coastal locations
#'   (Hargreaves and Samani, as cited by Pereira et al., 2025). Pereira et
#'   al. (2025, eqs. 3.53 and 3.54) also give regressions of kRs on the
#'   long-term means of Tmax - Tmin, wind speed and relative humidity, for
#'   all climates or by aridity class;
#'   these are not implemented, but their result can be passed as
#'   \code{kRs}. The default is 0.16.
#'   \item Humidity: Tdew = Tmin - aT (Pereira et al., 2025, eqs. 2.6 and
#'   2.7), with aT depending on the aridity of the region (UNEP aridity
#'   index, AI): 4-6 ºC in hyper-arid regions (AI < 0.05), 2-3 ºC in arid
#'   regions (0.05-0.20), 1-2 ºC in semiarid regions (0.20-0.50), 0-1 ºC in
#'   dry sub-humid regions (0.50-0.65) and 0 ºC in moist sub-humid and humid
#'   regions (AI >= 0.65). The value 6 ºC is only for very arid sites. The
#'   revision also allows aT calibrated for each calendar month with
#'   regional data, which can be negative in winter. \code{aT} takes a
#'   single value or one value per site; monthly values can be applied by
#'   passing \code{Tdew = Tmin - aT} instead.
#'   \item Wind speed: the regional mean, or 2 m s-1 if it is not known
#'   (the default of \code{U2}).
#' }
#' The cloud cover route (\code{CC}) and the CO2 correction (\code{CO2})
#' are not FAO procedures. The tall reference crop (\code{crop = 'tall'})
#' follows ASCE-EWRI (Walter et al., 2002; ASCE-EWRI, 2005).
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
#' \strong{Daily data.} By default all three functions work with monthly
#' data. \code{hargreaves} and \code{penman} can also compute daily ETo by
#' passing the date of each time step in the argument \code{dates}
#' (\code{thornthwaite} is a monthly method and has no daily version). In
#' that case all the inputs are daily values (e.g., daily maximum and
#' minimum temperature, daily mean wind speed, daily sunshine hours or
#' daily solar radiation) and the output is ETo in mm per day, following
#' Allen et al. (1998). The day of the year used for the extraterrestrial
#' radiation is that of each date (FAO-56 eq. 21-25), instead of the
#' mid-month day used for monthly data, and the result is not multiplied by
#' the number of days in the month. In \code{penman} the soil heat flux G is
#' set to zero, as recommended for daily time steps (FAO-56 eq. 42), so
#' the ETo of each day only depends on the inputs of that day. The
#' precipitation-modified Hargreaves equation (Droogers and Allen, 2002)
#' was calibrated with monthly precipitation, so \code{Pre} can not be used
#' together with \code{dates}. With daily data the result carries the
#' dates: vector and matrix input give a \code{\link[zoo]{zoo}} object
#' indexed by \code{dates}, and 3-d array input gives an array with the
#' dates as names of the time dimension. If the main input is a time series
#' (\code{ts}) and \code{dates} is given, the time attributes of the series
#' are ignored and the dates are taken from \code{dates}. A \code{zoo}
#' result can be aggregated to other time steps with
#' \code{\link[zoo]{aggregate.zoo}}, e.g. to monthly totals with
#' \code{aggregate(x, zoo::as.yearmon, sum)}.
#'
#'
#' @return A vector, matrix or 3-d array with the values of monthly potential
#' or reference evapotranspiration, in mm. If the main input is a time series
#' (and \code{dates} is not given) the output is a time series with the same
#' time attributes.
#'
#' If \code{dates} is given, the output has one value of daily reference
#' evapotranspiration per date, in mm per day. For vector or matrix input
#' (including time series) it is a \code{\link[zoo]{zoo}} object with
#' \code{index} equal to \code{dates}; a matrix input gives a multi-column
#' \code{zoo} object that keeps the column names of \code{Tmin}. For 3-d
#' array input it is an array of the same dimensions whose first
#' \code{dimnames} are the dates in "YYYY-MM-DD" format, keeping the other
#' \code{dimnames} of \code{Tmin}.
#'
#'
#' @references
#' Thornthwaite, C. W., 1948. An approach toward a rational classification of
#' climate.
#' \emph{Geographical Review} \bold{38}: 55–94. DOI:10.2307/2107309.
#'
#' Hargreaves G.H., Samani Z.A., 1985. Reference crop evapotranspiration from
#' temperature. \emph{Applied Engineering in Agriculture} \bold{1}(2): 96–99.
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
#' Allen R.G., Pereira L.S., Raes D., Smith, M., 1998. \emph{Crop
#' evapotranspiration - Guidelines for computing crop water requirements - FAO
#' Irrigation and drainage paper 56}. FAO, Rome. ISBN 92-5-104219-5.
#'
#' Pereira L.S., Allen R.G., Paredes P., López-Urrea R., Raes D., Smith M.,
#' Kilic A., Salman M., 2025. \emph{Crop evapotranspiration - Guidelines for
#' computing crop water requirements - FAO Irrigation and Drainage Paper 56
#' Rev. 1}. FAO, Rome. DOI:10.4060/cd6621en.
#'
#' ASCE-EWRI, 2005. \emph{The ASCE standardized reference
#' evapotranspiration equation.} Report of the Task Committee on
#' Standardization of Reference Evapotranspiration, R.G. Allen et al.
#' (eds.), American Society of Civil Engineers, Reston, VA.
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
#' # Load data for Wichita (Kansas), lat=37.6475N, elevation=402.6 m. a.s.l.
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
#' # Penman with temperature only (PMT): Rs from the temperature range,
#' # constant wind, ed from Tmin with aT = 1 (e.g., a dry sub-humid region)
#' pen4 <- penman(TMIN, TMAX, lat = 37.6475, z = 402.6, kRs = 0.16, aT = 1,
#'   na.rm = TRUE)
#'
#' # Plot them together
#' plot(ts(cbind(tho, har, pen, pen2, pen3, pen4), fr = 12))
#'
#' # Compare between methods
#' pairs(cbind(tho, har, pen, pen2, pen3, pen4))
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
#' # Daily data
#'
#' # Synthetic daily series for one year (not real observations), for a site
#' # at 40 N and 500 m a.s.l.
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
#' doy <- as.numeric(format(dates, "%j"))
#' tmin_d <- 5 - 8 * cos(2 * pi * (doy - 15) / 366)
#' tmax_d <- tmin_d + 11 + 2 * sin(2 * pi * doy / 7)
#' tsun_d <- 7 - 3 * cos(2 * pi * (doy - 15) / 366)
#' # ETo in mm per day
#' har_d <- hargreaves(tmin_d, tmax_d, lat = 40, dates = dates)
#' pen_d <- penman(tmin_d, tmax_d,
#'   tsun = tsun_d, lat = 40, z = 500,
#'   method = "FAO", dates = dates
#' )
#' # The result is a zoo object indexed by the dates
#' class(pen_d)
#' head(pen_d)
#' plot(cbind(penman = pen_d, hargreaves = har_d),
#'   plot.type = "single",
#'   col = c("black", "red"), ylab = "ETo (mm/day)"
#' )
#' # Monthly and weekly totals of the daily values
#' aggregate(pen_d, zoo::as.yearmon, sum)
#' head(aggregate(pen_d, function(d) as.Date(cut(d, "week")), sum))
#'
#' # FAO-56 Example 18 (Allen et al., 1998): Uccle (Brussels), 6 July,
#' # 50 48'N, 100 m a.s.l.; ETo = 3.9 mm/day
#' penman(12.3, 21.5, 2.078,
#'   tsun = 9.25, ed = 1.409, lat = 50.8, z = 100,
#'   method = "FAO", dates = as.Date("2023-07-06")
#' )
#'
#' @export
#'
hargreaves <- function(Tmin, Tmax, Ra = NULL, lat = NULL, Pre = NULL,
                       na.rm = FALSE, verbose = TRUE, dates = NULL) {
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

  # Daily (`dates` given) or monthly (default) time step
  daily <- !is.null(dates)

  if (!is.null(Pre)) {
    using$Pre <- TRUE
    warn$push("Using precipitation data, following Droogers and Allen (2002).")
    if (daily) {
      check$push(paste(
        "The precipitation-modified Hargreaves equation (`Pre`; Droogers and",
        "Allen, 2002) uses monthly precipitation, so it can not be used with",
        "daily data (`dates`)."
      ))
    }
  }

  if (!is_flag(na.rm)) {
    check$push("Argument `na.rm` must be set to either TRUE or FALSE.")
  } else if (na.rm) {
    warn$push("Missing values (`NA`) will not be considered in the calculation.")
  } else {
    warn$push("Checking for missing values (`NA`): all the data must be complete.")
  }

  if (!is_flag(verbose)) {
    check$push("Argument `verbose` must be set to either TRUE or FALSE.")
  }

  # Check for missing values in inputs
  if (isFALSE(na.rm) && (anyNA(Tmin) || anyNA(Tmax))) {
    check$push("`Tmin` and `Tmax` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  if (isFALSE(na.rm) && (anyNA(Ra))) {
    check$push("`Ra` must not contain NA values if argument `na.rm` is set to FALSE.")
  }

  if (isFALSE(na.rm) && (anyNA(Pre))) {
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

  # Determine output data shape. With daily data a time series input is
  # treated as a plain vector or matrix (the output is a zoo object).
  if (is.ts(Tmin) && !daily) {
    if (is.matrix(Tmin)) {
      out_type <- "tsmatrix"
    } else {
      out_type <- "tsvector"
    }
  } else if (is.vector(Tmin) || (is.ts(Tmin) && !is.matrix(Tmin))) {
    out_type <- "vector"
  } else if (is.matrix(Tmin)) {
    out_type <- "matrix"
  } else { # is.array; default
    out_type <- "array"
  }
  warn$push(paste0("Input type is ", out_type, "."))

  # Save column names for later
  names <- dimnames(Tmin)

  # Determine dates: day of the year of each date (daily data), or month
  # length and mid-month day-within-year (monthly data)
  if (daily) {
    dates <- check_dates(dates, n_times, check)
    if (!is.null(dates)) {
      warn$push(paste0(
        "Daily data (`dates` provided) spanning ", dates[1], " to ",
        dates[n_times], "; ET0 is given in mm per day."
      ))
      if (is.ts(Tmin)) {
        warn$push("The time series attributes of `Tmin` are ignored.")
      }
      msum_array <- array(yday(dates), dim = int_dims)
    }
  } else if (is.ts(Tmin)) {
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
    mlen_array <- array(rep_len(mlen, n_times), dim = int_dims)
    msum_array <- array(rep_len(msum, n_times), dim = int_dims)
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

  # Report the computation options
  if (verbose) {
    message(paste(warn$getMessages(), collapse = " "))
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
    # number of day in the year: mid-month day for monthly data, day of the
    # year of each date for daily data
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
    omegas[sset < (-1)] <- pi
    # Ra, MJ m-2 d-1
    Ra <- 37.6 * dr *
      (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
    Ra <- ifelse(Ra < 0, 0, Ra)
  }

  # 2. Daily ET0, mm day-1
  if (!using$Pre) {
    # Use original Hargreaves equation (Hargreaves and Samani, 1985)
    ET0 <- 0.0023 * 0.408 * Ra * (Tmean + 17.8) * Tr^0.5
  } else {
    # Use modified method (Droogers and Allen, 2002)
    ab <- Tr - 0.0123 * Rs
    ET0 <- 0.0013 * 0.408 * Ra * (Tmean + 17.0) * ab^0.76
    ET0[is.nan(ab^0.76)] <- 0
  }
  ET0 <- ifelse(ET0 < 0, 0, ET0)

  # Transform ET0 to mm month-1 (monthly data); daily data are kept in
  # mm day-1
  if (!daily) {
    ET0 <- ET0 * mlen_array
  }


  ### Format output and return - - - - - - - - - - - - - - - - - - - - - - -

  if (daily) {
    return(daily_output(ET0, out_type, dates, names))
  }
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
