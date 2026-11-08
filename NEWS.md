# SPEI (development version)

## Breaking changes

* For the `Gamma` and `PearsonIII` distributions, `$coefficients` now has an
  extra row `pze` with the probability of zero precipitation in the reference
  period, and this value is used when the coefficients are passed back through
  `params`. Before, `pze` was re-estimated from the new data, so applying
  calibrated parameters to other data gave inconsistent values. Arrays without
  the `pze` row (from earlier versions) are still accepted, with a warning.
* `<penman>` now requires the elevation `z`. It was optional when `P` or `P0`
  was given, and a fixed elevation of 840 m was then silently used for the
  clear-sky solar radiation.

## Bug fixes

* `<spei>` and `<spi>`: `fit = "max-lik"` and `fit = "pp-pwm"` returned only
  `NA`. `max-lik` now uses the `ub-pwm` estimates as starting values, and
  `pp-pwm` passes the plotting-position constants correctly to
  `lmomco::pwm.pp()` (#67).
* `<spei>` and `<spi>`: `fit = "max-lik"` with `Gamma` or `PearsonIII` now
  warns and uses `ub-pwm`, as documented.
* `<spei>` and `<spi>`: when a month had no data or constant values in one
  series, values of the same month in the first series (or of another month,
  when `ref.start` was set) were set to `NA` (#64).
* `<penman>`, `<hargreaves>` and `<thornthwaite>`: with matrix or array input
  that is not a time series, and a length that is not a multiple of 12, the
  sites after the first got shifted calendar months (day length and days per
  month).
* `<penman>`: with several sites and `P0`, the elevations `z` were recycled
  along time instead of across sites. `z` is now also used for the clear-sky
  radiation when `P` or `P0` is given.
* `<penman>` and `<hargreaves>`: extraterrestrial radiation in polar summer
  (sun above the horizon all day) used the largest sunset hour angle found in
  the data instead of pi, underestimating it above about 66.5 degrees.
* `<spei>` and `<spi>`: 3-d arrays with `dimnames` gave an error.
* `<spei>` and `<spi>`: time series with a non-integer frequency (e.g.
  365.25) returned only `NA`; they now give an error.
* `<spei>` and `<spi>`: with `fit = "max-lik"`, fits where the optimisation
  found no valid parameters were used anyway; the `ub-pwm` estimates are now
  kept, with a warning. `<parglo.maxlik>` now reports `conv = 1` in that
  case.
* `<hargreaves>`: `Pre` given as a matrix or array failed with
  "non-conformable arrays" (#59, thanks to Kai Lochbihler).
* `<penman>`: with `tsun`, months of polar night (day length 0) returned
  `NA`.
* `<penman>`: input with one or two time steps failed with "subscript out of
  bounds" (#75).
* `<thornthwaite>`: with several sites, a site with all monthly means below
  0 C got `NaN` instead of zero.
* `<kern>`: `kern(1, "gaussian")` returned `NaN`.
* `<plot.spei>`: colours were assigned by factor order with inverted labels,
  so a series with only negative (or only positive) values was drawn
  entirely in the wet colour (#72).

## Other changes

* `<spei>` and `<spi>`: the `Gamma` and `PearsonIII` distributions now warn
  when the data contain negative values, which they cannot fit.
* `<thornthwaite>` now requires at least 12 months of data. With fewer, the
  annual heat index was computed from a partial year without notice (#68).

## Documentation

* `wichita` dataset: `TSUN` was missing from the documentation (`ACSH` was
  listed twice), and five `TSUN` values between October 2000 and May 2001
  exceed the possible day length; this is now noted.

* `<spei>` and `<spi>`: the adjustment for the probability of zero
  precipitation (`pze`) was overwritten for the `Gamma` and `PearsonIII`
  distributions, so zero-precipitation periods were returned as `-Inf` and
  non-zero periods were biased toward drier values. This regression was
  introduced in 1.8.0; `log-Logistic` was not affected. Thanks to Mark L.
  Morrissey for reporting it (#87; also #79, #85).

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
