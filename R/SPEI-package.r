#' SPEI.
#'
#'
#' @name SPEI
#' @docType package
#'
#'
#' @description
#' A set of functions for computing potential evapotranspiration and several
#' widely used drought indices including the Standardized
#' Precipitation-Evapotranspiration Index (SPEI).
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab SPEI\cr
#'   Type: \tab Package\cr
#'   Version: \tab 1.8\cr
#'   Date: \tab 2023-01-20\cr
#'   License: \tab GPL version 2 or newer\cr
#'   LazyLoad: \tab yes\cr
#' }
#'
#' Functions \code{\link{spei}} and \code{\link{spi}} are the workhorse of the
#' SPEI library. Other functions such as \code{\link{kern}},
#' \code{\link{parglo.maxlik}} is an auxiliary low-level function and
#' will not be used directly by the typical user. Functions for computing
#' potential evapotranspiration are provided, too, for helping computing
#' the SPEI. They are: \code{\link{thornthwaite}},
#' \code{\link{hargreaves}} and \code{\link{penman}}.
#'
#' @author
#' Santiago Beguería and Sergio M. Vicente-Serrano
#' Maintainer: Santiago Beguería
#' Contributors: Fergus Reig
#'
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar
#' drought index sensitive to global warming: The Standardized Precipitation
#' Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696,
#' DOI: 10.1175/2009JCLI2909.1.
#'
#' \url{https://spei.csic.es}
#' @keywords package
#'
#' @importFrom checkmate makeAssertCollection
#' @importFrom stats cycle end frequency start ts optim
#' @importFrom zoo as.yearmon as.Date.yearmon rollapply na.trim
#' @importFrom stats aggregate is.ts pnorm qnorm sd time window
#' @importFrom TLMoments PWM
#' @importFrom lmomco are.lmom.valid are.parglo.valid cdfgam cdfpe3 cdfgam
#' @importFrom lmomco pargam parglo parpe3 pwm.pp pwm2lmom
#' @importFrom lmom pelgam pelglo pelpe3 cdfglo

NULL
