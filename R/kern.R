#' @name Kernel-functions
#' @title Time kernel for computing the SPEI at different time scales.
#' @aliases kern.plot
#' @description Function \code{kern} is used internally by
#' \code{\link{spei}}
#' and \code{\link{spi}}for computing drought indices at different
#' time scales.
#' @param scale numeric, time scale or length of the kernel.
#' @param type character, shape of the kernel function.
#' @param shift numeric, shifting of the kernel peak.
#' @details
#' Drought indices, such as the SPEI or the SPI, are usually
#' computed at different time scales to adapt to the different response
#' times of systems affected by drought. This is accomplished by applying
#' a kernel function to the data prior to computation of the SPEI.
#' Application of a kernel has the effect of smoothing the temporal
#' variability of the resulting SPEI, allowing for the major patterns
#' to emerge from the noise. Other way of considering it is that the
#' kernel allows incorporating information of previous time steps into the
#'  calculation of the current time step, so the resulting values of the
#'  SPEI adapt to the memory of the system under study.
#'
#'
#'  The most common kernel function is rectangular, i.e. all the data
#'  of the previous \emph{n} time steps are given equal weight. This
#'  was the way the Standardized Precipitation Index (SPI) was defined,
#'  and it is also the way the SPEI is computed. This would be the default
#'  option for the \code{kern} function. However, data from the past can be
#'  thought of as having a decreasing influence in the current state of the
#'  system as the temporal lag between them increases. The function
#'  \code{kern} allows weighting the past data as a function of the time
#'  lapse, according to a series of pre-defined shapes. Available options
#'  are 'rectangular' (default), 'triangular', 'circular' and 'gaussian'.
#'
#'  By default the highest weight will be given to the observation of the
#'   current month. However, it is possible to modify this by setting the
#'    \code{shift} parameter to a value higher than zero. This will cause
#'    the highest weight be given to the \emph{n} antecedent observation.
#'
#'
#'  \code{kern.plot} produces plots of the weighting factor against the
#'  time lag for the four different kernel shapes so they can be compared.
#'
#'
#' @return A vector of length equal to \code{scale} with weights used for
#' computing the drought index.
#'
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010.
#' A Multi-scalar drought index sensitive to global warming: The
#' Standardized Precipitation Evapotranspiration Index – SPEI.
#' \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
#'
#'
#' @author Santiago Beguería
#'
#'
#' @examples
#' # A rectangular kernel with a time scale of 12 and no shift
#' kern(12)

#' # A gaussian kernel with a time scale of 12 and no shift
#' kern(12,'gaussian')

#' # Comparison of the four kernels, with and without shift
#' kern.plot(12)
#' kern.plot(12,2)
#'
#'
#' @export
#'
kern <- function(scale, type = "rectangular", shift = 0) {
  if (!(type %in% c("rectangular", "triangular", "circular", "gaussian"))) {
    stop("type must be one of: rectangular, triangular, circular, gaussian")
  }
  #
  s <- scale
  h <- shift
  if (h >= s) {
    stop('Parameter "shift" must be lower than "scale"')
  }
  if (h < 0) {
    stop('Parameter "shift" cannot have a negative value')
  }
  if (s <= 0) {
    stop('Parameter "scale" must be higher than zero')
  }

  if (s == 1) type == "rectangular"

  k <- switch(type,
    rectangular = rep(1, s),
    triangular = s:1,
    circular = (s^2 + (1 - (1:s)^2)),
    gaussian = (1 / 0.4) * 1 / sqrt(2 * pi * 1^2) *
      exp(-(seq(0, -3, -3 / (s - 1)) - 0)^2 / 2 * 1^2)
  )

  if (h) k <- c(k[(h + 1):2], k[1:(s - h)])
  return(k / sum(k))
}
