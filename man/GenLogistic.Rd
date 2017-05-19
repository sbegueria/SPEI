\name{Generalized Logistic}

\alias{pglo}
\alias{parglo.maxlik}

\title{Generalized Logistic distribution function.}

\description{Cumulative distribution function of the Generalized Logistic probability distribution function, and maximum likelihood fitting function.}

\usage{
pglo(x, para)

parglo.maxlik(x, ini)
}

\arguments{
  \item{x}{ vector of quantiles for which to evaluate the PDF.}
  \item{para}{ a list of parameters of a generalized Logistic distribution function, as yield by function \code{parglo}.}
  \item{ini}{ a vector of initial values of the parameters to be fit.}
}

\details{This functions are used internally by \code{spei} and \code{spi} and are supposed to never be needed by the regular user. Initial values for maximum likelihood estimation can be provided by \code{parglo}.}

\value{A vector of cumulative probabilities corresponding to the quantiles in \code{x}.}

\references{S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.}

\author{Santiago Beguería}

%\note{}

%\seealso{}

%\examples{}

%\keyword{}
%\keyword{}
