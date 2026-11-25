#' @title Standardized Evapotranspiration Deficit Index (SEDI)
#'
#' @description
#' Given a time series of the evapotranspiration deficit (actual
#' evapotranspiration minus atmospheric evaporative demand, a variable
#' bounded above by zero), gives a time series of the Standardized
#' Evapotranspiration Deficit Index (SEDI).
#'
#' @param data a vector, matrix, 3-d array or time series with time ordered
#' values of the evapotranspiration deficit, i.e. actual evapotranspiration
#' (AET) minus atmospheric evaporative demand (AED, e.g. potential or
#' reference evapotranspiration), in the same units. These values are zero or
#' negative.
#' @param scale,kernel,fit,na.rm,ref.start,ref.end,keep.x,params,verbose as
#' in \code{\link{spei}}.
#' @param distribution optional, either 'auto' (default), for the flexible
#' choice of the distribution described in Details, or 'log-Logistic', the
#' single reference distribution used for the SEDI by Vicente-Serrano et al.
#' (2025), fitted as in the SPEI (no special handling of zeros).
#' @param ... not used.
#'
#' @details
#' The SEDI (Kim and Rhee, 2016) measures drought from the deficit of
#' actual evapotranspiration with respect to the atmospheric evaporative
#' demand. \code{sedi} works as \code{\link{spei}} and \code{\link{spi}}
#' (same time scales, kernels, reference period, parameter fitting and
#' input types), but the data are bounded above by zero, rather than below
#' as the precipitation in the SPI.
#'
#' The package has no function to compute actual evapotranspiration: it must
#' be supplied by the user, for example from a land surface model or a
#' dataset such as GLEAM (\url{https://www.gleam.eu}), as in the example of
#' FlexDroughtIndex (\url{https://github.com/lcsc/FlexDroughtIndex}). The
#' evaporative demand can be computed with \code{\link{penman}} or
#' \code{\link{hargreaves}}.
#'
#' With \code{distribution = 'auto'} (the default), for each series and
#' position in the year (month, week, etc.) six three-parameter
#' distributions (generalized extreme value, generalized logistic,
#' generalized Pareto, three-parameter log-normal, Pearson III and Weibull)
#' are fitted by L-moments to the negative values, and the one whose
#' standardized values in the reference period are closest to normal
#' (Shapiro-Wilk W) is kept, as with \code{distribution = 'auto'} in
#' \code{\link{spei}} (see there for the details). Zeros are handled through
#' their probability (pze) in the reference period: a negative value x gets
#' the probability (1 - pze) F(x), where F is the fitted distribution, and
#' zeros get 1 - pze (the mirror of the SPI, where zeros get pze). Positive
#' values, which should not occur, are treated as zeros, with a warning.
#'
#' The \code{coefficients} element of the result has, for each series and
#' period, a distribution code (\code{dist}; 1 = gev, 2 = glo, 3 = gpa,
#' 4 = ln3, 5 = pe3, 6 = wei), the location, scale and shape parameters
#' (as in package \code{lmom}) and pze; the \code{selected} element has the
#' names of the chosen distributions. The coefficients can be passed back
#' through \code{params}.
#'
#' @return An object of class \code{spei} (see \code{\link{spei}}), with
#' \code{index = "SEDI"}, so that \code{print}, \code{summary} and
#' \code{plot} work as for the SPEI and the SPI.
#'
#' @references
#' D. Kim, J. Rhee. 2016. A drought index based on actual evapotranspiration
#' from the Bouchet hypothesis. \emph{Geophysical Research Letters}
#' \bold{43}(19): 10277-10285, DOI: 10.1002/2016GL070302.
#'
#' S.M. Vicente-Serrano, F. Reig, S. Beguería, A. El-Kenawy, F.
#' Domínguez-Castro, M. Franquesa, L. Gimeno-Sotelo, M. Adell-Michavilla, A.
#' Halifa-Marín, I. Noguera, M. Andres-Martin, C. Azorin-Molina, A.
#' Crespillo, D. Pérez-Pajuelo. 2025. An optimal and flexible approach for
#' drought quantification based on standardised indices.
#' \emph{International Journal of Climatology} \bold{45}(10): e8906,
#' DOI: 10.1002/joc.8906.
#'
#' @seealso \code{\link{spei}}, \code{\link{spi}}.
#'
#' @examples
#' # Synthetic evapotranspiration deficit (AET - AED), bounded above by zero
#' set.seed(1)
#' aed <- 70 + 50 * sin(2 * pi * (1:480 - 4) / 12) + rnorm(480, 0, 5)
#' aet <- aed * rbeta(480, 4, 2)
#' def <- ts(aet - aed, start = c(1981, 1), frequency = 12)
#' sedi3 <- sedi(def, 3)
#' summary(sedi3$fitted)
#' table(sedi3$selected)
#' plot(sedi3)
#'
#' @export
#'
sedi <- function(data, scale, kernel = list(type = "rectangular", shift = 0),
                 distribution = "auto", fit = "ub-pwm", na.rm = FALSE,
                 ref.start = NULL, ref.end = NULL, keep.x = FALSE,
                 params = NULL, verbose = TRUE, ...) {
  if (!is.character(distribution) || length(distribution) != 1 ||
    !distribution %in% c("auto", "log-Logistic")) {
    stop("Argument `distribution` must be `auto` or `log-Logistic` for sedi().")
  }

  # spei() reports the options for the SPEI; reword them for the SEDI
  to_sedi <- function(x) {
    sub(
      "Standardized Precipitation Evapotranspiration Index (SPEI)",
      "Standardized Evapotranspiration Deficit Index (SEDI)", x,
      fixed = TRUE
    )
  }
  sol <- withCallingHandlers(
    spei(
      data, scale, kernel, distribution, fit, na.rm,
      ref.start, ref.end, keep.x, params, verbose,
      .index = "SEDI"
    ),
    message = function(m) {
      message(to_sedi(conditionMessage(m)), appendLF = FALSE)
      invokeRestart("muffleMessage")
    }
  )
  sol$info <- to_sedi(sol$info)
  sol$index <- "SEDI"
  sol$call <- match.call(expand.dots = FALSE)

  return(sol)
}
