#' SPEI.
#'
#'
#' @name SPEI
#' @docType package
#' 
#'
#' @description  
#' A set of functions for computing potential evapotranspiration and several widely 
#' used drought indices including the Standardized Precipitation-Evapotranspiration 
#' Index (SPEI).
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab SPEI\cr
#'   Type: \tab Package\cr
#'   Version: \tab 1.4\cr
#'   Date: \tab 2011-26-09\cr
#'   License: \tab GPL version 2 or newer\cr
#'   LazyLoad: \tab yes\cr
#' }
#'   
#' Functions \code{\link{spei}} and \code{\link{spi}} are the workhorse of the 
#' SPEI library. Other functions such as \code{\link{kern}}, \code{\link{cdfglo}} 
#' or \code{\link{pglo}} are auxiliary low-level functions and they will not be used 
#' directly by the typical user. Functions for computing potential evapotranspiration 
#' are provided, too, for helping computing the SPEI. They are: \code{\link{thornthwaite}}, 
#' \code{\link{hargreaves}} and \code{\link{penman}}.
#'      
#' @author     
#' Santiago Beguería and Sergio M. Vicente-Serrano
#' Maintainer: Santiago Beguería
#' 
#' 
#' @references 
#' S.M. Vicente-Serrano, S. Begueria, J.I. Lopez-Moreno. 2010. A Multi-scalar drought 
#' index sensitive to global warming: The Standardized Precipitation Evapotranspiration 
#' Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
#' 
#' \url{http://sac.csic.es/spei/}
#' @keywords package
#' 
NULL
