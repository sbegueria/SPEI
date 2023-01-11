#' @name Generalized-Logistic
#' @title Generalized Logistic maximum likelihood function
#' @description Maximum likelihood fitting function for
#' generalized logistic distribution.
#' @details This function is used internally by \code{spei}
#' and \code{spi} and is supposed to never be needed by the
#' regular user. Initial values for maximum likelihood estimation
#' can be provided by \code{parglo}.
#' @usage
#' parglo.maxlik(x, ini)
#'
#' @param x vector of quantiles for which to evaluate the PDF.
#' @param ini a vector of initial values of the parameters to be fit.
#'
#' @return a list of parameters of a generalized Logistic
#' distribution function
#'
#' @references
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010.
#' A Multi-scalar drought index sensitive to global warming:
#' The Standardized Precipitation Evapotranspiration Index – SPEI.
#' \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
#'
#' @author Santiago Beguería
#'
#' @importFrom stats optim
#' @importFrom lmomco are.parglo.valid
#'
#' @export
#'
parglo.maxlik <- function(x, ini) {
  # generalized logistic log-likelihood function
  glo.loglik <- function(theta, x) {
    if (!are.parglo.valid(list(type = "glo", para = theta), nowarn = TRUE) |
      theta[[3]] == 0) {
      return(1000000)
    }
    gamma <- theta[1]
    alpha <- theta[2]
    kappa <- theta[3]
    y <- 1 - kappa * ((x[!is.na(x)] - gamma) / alpha)
    if (min(y) <= 0) {
      return(1000000)
    }
    y <- -(1 / kappa) * log(y)
    n <- length(x)
    logl <- -n * log(alpha) - (1 - kappa) * sum(y) - 2 * sum(log(1 + exp(-y)))
    return(-logl) # optim() does minimization by default
  }
  # optimize
  o <- suppressWarnings(optim(par = ini, fn = glo.loglik, x = x))
  # o <- optim(par=ini, fn=glo.loglik, x=x,
  # 	lower=c(-Inf,0.00001,-0.5), upper=c(Inf,Inf,0.5))
  return(list(
    type = "glo",
    para = o$par,
    source = "parglo.loglik", value = o$value, count = o$count,
    conv = o$convergence, msg = o$message
  ))
}
