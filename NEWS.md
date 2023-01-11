# SPEI 1.8.0, November 2022

This is a major release that adds a range of substantial new features and
fixes a number of major and minor bugs.

## Breaking changes

* All functions now accept 3-d arrayed data, enabling the possibility of
  using them on gridded climate data. A new data set (`cruts4`) has been added
  to test this new function.
* Function `<spei>` now accepts non-monthly data (that is, data with frequency
  other than 12).
* A thorough set of test functions have been implemented to help developing
  new functions without compromising the old results (Thanks to Doug
  Freedman who wrote the seminal set of tests).

## New features

* More thorough data and options checks have been implemented, and a single
  error / warning message is produced containing all the errors upon failure.
* Different verbosity levels are now implemented in all functions: now they
  print information about the options being used. A new argument (`verbosity`)
  can be used to override this behavior if not wanted.
* Different versions of the Penman-Monteith ETo calculation have been 
  implemented in function `<penman>`.
* A new option to include CO2 concentration data is now available in function
  `<penman>`.
* A new option for when no wind data are available in function `<penman>`.
* `<plot.spei>` has been completely rewritten based on `ggplot2`, solving
  some bugs and enabling more flexibility.

## Bug fixes

* Several minor bugs in `<thornthwaite>`, `<hargreaves>`, and `<penman>`
  functions have been solved (output difference is less than 0.1%).
* A bug in `<spei>` has been solved that resulted in bad cumulative data when
  using a non-rectangular kernel, leading to incorrect SPEI values.


# SPEI 1.7.2, June 2019 (only on GitHub).

## Breaking changes

* Allowing for data with frequency other than 12 in `<spei>` function.


# SPEI 1.7.1, June 2017.

## Bug fixes

* Corrected an error in `<spei>` function, which was not working when
  distribution was Gamma or PeasonIII and using user provided parameters.
  (Thanks to Emanuele Cordano, emanuele.cordano@gmail.com -- 'ecor')
* Added probability of monthly precipitation = 0 (pze) when using user
  provided parameters. (Thanks to Emanuele Cordano). 


# SPEI Version 1.7, June 2017.

## Bug fixes

* Corrected a bug in the \code{\link{kern}} function which resulted in a
  multiplicative kernel instead of an additive one such the one expected in
  the \code{\link{spei}} and \code{\link{spi}} functions.
* Plotting methods for \code{spei} objects completely rewritten, now using
  \code{ggplot2}. Additionally, a bug causing a bad representation of time
  series starting in months other than January has been corrected.


# SPEI 1.6, September 2013.

## Bug fixes

* Corrected an error in the function \code{\link{thornthwaite}} which resulted
  in wrong potential evapotranspiration estimates when a multivariate time
  series was used as input.
* Corrected an error in the function \code{\link{spi}} which resulted in wrong
  handling of zero precipitation months when using the Gamma or PearsonIII
  distribution.
* Minor fixes to the \code{\link{spi}} and \code{\link{plot.spei}} functions
  to correctly handle \code{spei} objects when they result from a call to
  \code{\link{spi}}.
* Modification to the \code{\link{kern}} function, which now yields kernel
  coefficients averaging one.
* Corrected an error in the functions \code{\link{spi}} and \code{\link{spei}}
  which resulted in ub-pwm method being used irrespective of the value of the
  \code{fit} parameter used, when using the 'Gamma' or 'PearsonIII' distributions.
* Minor changes to the documentation.


# SPEI 1.5, May 2013.

## Bug fixes

* Optimization of function \code{\link{spei}}, now using embed() for
  accumulating the data at the desired time scale.


# SPEI 1.4, May 2013.

## Bug fixes

* Minor fixes to functions \code{\link{penman}} and \code{\link{pwm}}.
* Documentation of the \code{\link{penman}} function defined by mistake ed as
  the saturation vapor pressure, while it should read 'actual vapor pressure'.
* Function \code{\link{zzz.R}} added to display basic information about the
  SPEI package at startup.
* Function \code{\link{SPEINews}} added to display the NEWS file.


# SPEI 1.3, March 2013.

## Bug fixes

* Minor fixes to functions \code{\link{spei}} and \code{\link{penman}}.
* Added new option for user-supplied SPEI parameters in the \code{\link{spei}}
  function. This overrides the fitting of a probability function to the data.
* Added new dataset \code{\link{cabinda}} from Allen et al. (1998).


# SPEI 1.2, October 2012.

## Bug fixes

* Fixed a bug causing several functions to fail when a time series not
  belonging to matrix class was provided.
* Function \code{\link{plot.spei}} now distinguishes between calls to
  \code{\link{spei}} and \code{\link{spi}} and labels the axis accordingly.


# SPEI 1.1, March 2012.

* Functions \code{\link{spei}} and \code{\link{spi}} now yield an object of
  class "spei".
* New functions for summarizing and plotting "spei" objects are provided.
* An option to establish a reference period for the computation of the indices
  has been implemented in functions \code{\link{spei}} and \code{\link{spi}}.


# SPEI 1.0, January 2012.

First release of the SPEI package.
