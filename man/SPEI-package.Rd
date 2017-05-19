\name{SPEI-package}

\docType{package}

\title{Calculation of the Standardised Precipitation-Evapotranspiration Index}

\description{A set of functions for computing potential evapotranspiration and several widely used drought indices including the Standardized Precipitation-Evapotranspiration Index (SPEI).}

\details{
%\tabular{ll}{
%Package: \tab SPEI\cr
%Type: \tab Package\cr
%Version: \tab 1.4\cr
%Date: \tab 2011-26-09\cr
%License: \tab GPL version 2 or newer\cr
%LazyLoad: \tab yes\cr
%}
Functions \code{\link{spei}} and \code{\link{spi}} are the workhorse of the SPEI library. Other functions such as \code{\link{kern}}, \code{\link{cdfglo}} or \code{\link{pglo}} are auxiliary low-level functions and they will not be used directly by the typical user. Functions for computing potential evapotranspiration are provided, too, for helping computing the SPEI. They are: \code{\link{thornthwaite}}, \code{\link{hargreaves}} and \code{\link{penman}}.

Version history:

1.4. Minor fixes to functions \code{\link{penman}} and \code{\link{pwm}}. Documentation of the penman function defined by mistake ed as the saturation vapour pressure, while it should read 'actual vapour pressure'. Function zzz.R added to display basic information about the SPEI package at startup. Function \code{\link{SPEINews}} added to display the NEWS file.

1.3. Minor fixes to \code{\link{spei}} and \code{\link{penman}} functions. New option for user-supplied SPEI parameters in the \code{\link{spei}} function. New dataset \code{\link{cabinda}}.

1.2. A bug causing several functions to fail when a time series not belonging to matrix class was provided was fixed. Function \code{\link{plot.spei}} now distinguises between calls to spei and spi and labels the axis accordingly .

1.1. Functions \code{\link{spei}} and \code{\link{spi}} now yield an object of class "spei". New functions for summarizing and plotting "spei" objects are provided. An option to establish a reference period for the computation of the indices has been implemented in functions \code{\link{spei}} and \code{\link{spi}}.

1.0. First release.
}

\author{
Santiago Beguería and Sergio M. Vicente-Serrano
Maintainer: Santiago Beguería
}

\references{
S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.

http://sac.csic.es/spei/
}

\keyword{package}

%\seealso{#~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~}

%\examples{}
