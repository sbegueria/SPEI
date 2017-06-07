\name{Generic methods for spei objects}

\alias{print.spei}
\alias{summary.spei}
\alias{plot.spei}
\alias{print.spi}
\alias{summary.spi}
\alias{plot.spi}


\title{Generic methods for \code{spei} objects.}

\description{Generic methods for extracting information and plotting \code{spei} objects.}

\usage{
\method{print}{spei}(x, ...)

\method{summary}{spei}(object, ...)

\method{plot}{spei}(x, main, ...)
}

\arguments{
  \item{x}{ an object of class \code{spei}.}
  \item{object}{ an object of class \code{spei}.}
  \item{main}{ an (optional) title for the plot.}
  \item{...}{ additional parameters, not used at present.}
}

\details{This functions allow extracting information and plotting \code{spei} objects. \code{print} yields the fitted values, i.e. a time series of SPEI or SPI values. \code{summary} reports the function call, the parameters of the PDF used, and the time series of SPEI or SPI values. \code{plot} produces a plot of the time series of SPEI or SPI values, with blue and red colors for positive and negative values, respectively. If a reference period was used in the function call it is shown by a shaded area. In the unlikely case that NA or Inf values were produced, these are shown by circles.}

\references{S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.}

\author{Santiago Beguería}

%\note{}

%\seealso{}

%\examples{}

%\keyword{}
%\keyword{}
