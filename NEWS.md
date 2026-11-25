# SPEI (development version)

## New features

* New function `<scale_scan>` finds the time scale and the position in the
  year (month, week, etc.) at which the SPEI or the SPI best correlates
  with an annual series, such as tree-ring widths, crop yields or reservoir
  levels (#7). It computes the index at a set of time scales (or takes
  already computed index values), correlates it with the annual series at
  every position in the year and time scale, optionally also in previous
  years (`lag`), adjusts the p-values for multiple testing, and reports the
  strongest significant positive and negative windows. It works with any
  time series frequency. Several annual series (e.g. several trees) are
  averaged per year. A `plot` method draws a heatmap of the correlations,
  and `<scale_extract>` extracts the annual series of the index for a given
  window. A new vignette, `vignette("scale_scan")`, shows how to use them.

* New function `<drought_events>` finds the drought (or wet) events of an
  index series using run theory (#41): runs of consecutive time steps with
  the index at or below a threshold (dry events, default -1) or at or above
  it (wet events, default 1). It returns a data frame with one row per
  event and its start, end, duration, severity (the sum of the index
  values, negative for droughts), intensity (severity / duration), peak
  (the most extreme value) and time of the peak. It takes `spei` objects,
  time series (univariate or multivariate), vectors and matrices; time
  series give the times as labels such as "2011-05". Events shorter than
  `min.duration` can be dropped, and events separated by a few time steps
  can be merged (`pool`). A missing value breaks an event.

* New dataset `tarazona`: annual basal area increment of 18 cores of Aleppo
  pine (*Pinus halepensis*) in the unthinned plot of a plantation at
  Tarazona (Spain), 1959-2017, and the climate of the site (precipitation,
  reference evapotranspiration, minimum and maximum temperature) with 48
  periods per year (quarter-months), 1961-2020, from the Spanish drought
  monitor. It is used in `vignette("scale_scan")`. See `?Datasets`.

* New vignette, `vignette("weekly")`, on computing the indices with weekly
  and other non-monthly time series: building quarter-month and 52-week
  series from daily data, evapotranspiration at those resolutions, the
  reference period, and the comparison with the monthly index, using the
  `tarazona` dataset.

* `<penman>` and `<hargreaves>` can compute daily reference
  evapotranspiration (#6). The new argument `dates` takes the date of each
  time step (a `Date` vector, or character dates that `as.Date()` can read,
  sorted and without repeats; gaps are allowed). With `dates` the inputs are
  daily values and the result is in mm per day: the day of the year of each
  date is used for the extraterrestrial radiation, the result is not
  multiplied by the length of the month, and in `<penman>` the soil heat
  flux is zero (FAO-56 eq. 42). The daily result carries the dates: vector
  and matrix input (including time series, whose time attributes are
  ignored) give a `zoo` object indexed by `dates`, keeping the column names
  of a matrix, which can be aggregated to months or weeks with
  `aggregate()`; a 3-d array input gives an array whose first `dimnames`
  are the dates. The precipitation-modified Hargreaves equation (`Pre`) is
  monthly only and gives an error with `dates`. Without `dates` (the default) the
  results are the same as before.

* `<penman>` implements the Penman-Monteith temperature approach (PMT) of
  the revised FAO-56 (Pereira et al., 2025, FAO Irrigation and Drainage
  Paper 56 Rev.1, <https://doi.org/10.4060/cd6621en>), recommended over the
  Hargreaves-Samani equation when only maximum and minimum temperatures are
  available (#93). When none of `Rs`, `tsun` or `CC` is given (or they
  contain only `NA`), the incoming solar radiation is estimated from the
  temperature range, Rs = kRs sqrt(Tmax - Tmin) Ra (eq. 3.52), limited to
  the clear-sky radiation Rso; this used to be an error. The new argument
  `kRs` (default 0.16 for interior locations; 0.19 for coastal locations)
  takes one value or one value per site, and is ignored with a warning if
  radiation data are given. The new argument `aT` (default 0) gives the
  difference between `Tmin` and the dewpoint temperature, Tdew = Tmin - aT
  (eqs. 2.6-2.7), when the actual vapour pressure is estimated from `Tmin`;
  the revision suggests 0 in humid regions and up to 4-6 ºC in hyper-arid
  regions. With the default `aT = 0` the results are the same as before.
  `tsun` without `lat` now gives a specific error. The documentation
  describes what is new in the revision: the FAO Penman-Monteith equation
  is unchanged from the 1998 edition, and the cloud cover (`CC`) route and
  the CO2 correction are not FAO procedures.

* Time information for data that are not time series, in particular 3-d
  arrays of gridded data (#94). `<spei>` and `<spi>` have two new
  arguments, `start` and `frequency`, with the meaning of `ts()`, for
  vectors, matrices and 3-d arrays; they give an error if `data` is a time
  series. With them, `ref.start` and `ref.end` are calendar dates, the data
  can start in any period of the year, and non-monthly gridded data (e.g.
  weekly, `frequency = 48` or `52`) can be processed as arrays. A vector or
  matrix with `start`/`frequency` is processed as `ts(data, start,
  frequency)` and gives a time series. A 3-d array gives an array whose time
  `dimnames` label each period: "YYYY-MM" for monthly data, "YYYY" for
  annual data and "YYYY-Pnn" for other frequencies (e.g. "1961-P01" to
  "1961-P48"); the other `dimnames` are kept, and the `plot` method uses
  the labels. If neither argument is given, a 3-d array whose time
  `dimnames` are consecutive months ("YYYY-MM" or "YYYY-MM-DD") is taken to
  be a monthly series starting at the first of them (reported in the
  verbose message). The monthly PET functions, `<thornthwaite>`,
  `<hargreaves>` and `<penman>`, have a new argument `start` (the first
  month), used for the month length and the mid-month day of non-ts input;
  a 3-d array gives an array with "YYYY-MM" time `dimnames`, and a vector or
  matrix gives a monthly time series. `start` gives an error with time
  series input or with `dates`. Without the new arguments (or time
  `dimnames`) the results are the same as before: the data are taken to
  start in January. `vignette("gridded")` now uses `start` instead of
  reshaping the array into a time series matrix.

* `<spei>` and `<spi>` warn when `ref.start` and/or `ref.end` fall outside
  the time span of `data`. Such a reference period used to be silently
  limited to the data, which could hide a mistake in the time information
  (e.g. an array without `start`, whose years are counted from year 1, or
  an array whose time `dimnames` are now used, with the reference period
  given in years counted from year 1). The results are the same as before:
  the reference period is still limited to the data. The example of a
  reference period in `?spei`, which started before the data, now starts
  with them (same result).

## Changes in results

* `<penman>`: wind speeds `U2` below 0.5 m s-1 are set to 0.5 m s-1, as
  recommended by the revised FAO-56 (Pereira et al., 2025, p. 105) to
  account for the exchange of vapour by buoyancy under calm conditions.
  This applies to all three methods (`ICID`, `FAO` and `ASCE`) and changes
  the result only where `U2 < 0.5`; a message reports how many values were
  changed. The `wichita` and `cabinda` examples and the regression
  baselines are not affected (their lowest monthly wind speeds are 3.49 and
  0.80 m s-1).

# SPEI 1.9.0

## Changes in results with respect to 1.8.1

Most calls give the same results as in 1.8.1. The ones that change are
listed below, with the size of the change found by running the same calls
with both versions (script in `data-raw/compare-1.8.1.R`). Index differences
are in index units (z-values); PET differences are relative to the 1.8.1
value.

Calls that give the same results as 1.8.1:

* `<spei>` and `<spi>` with the `log-Logistic` distribution (the default in
  `<spei>`) and `fit = "ub-pwm"` (the default), at any scale, with any kernel
  and with or without a reference period.
* `<spi>` and `<spei>` with `Gamma` or `PearsonIII` in calendar months whose
  reference period has no zeros. In the `wichita` precipitation series
  (382 months, 4 of them zero) this covers all values at scales 3 and 12.
* `<thornthwaite>` and `<hargreaves>` with a vector, a time series, or a
  matrix whose number of rows is a multiple of 12.
* `<penman>` with `Rs` or `CC`, methods `ICID` and `FAO`, when the given or
  estimated solar radiation does not exceed the clear-sky radiation, and
  when `z` is given without `P` or `P0`.

Calls that give different results:

| Function and options | Cause | Values changed | Size of the change |
| --- | --- | --- | --- |
| `<spi>`/`<spei>`, `Gamma` or `PearsonIII`, calendar months with zeros in the reference period | #87 | All values of those calendar months | `wichita`, SPI-1 Gamma: 91 of 382 values, mean +0.09, max +0.72. The 4 zero months were `-Inf` and are now finite. A dry-season series with up to 60% zero months: SPI-1 Gamma 365 of 480 values, mean +0.32, max +3.2 (115 `-Inf` now finite); PearsonIII 432 of 480, mean +0.51; SPI-3 Gamma 186 of 480, mean +0.14. Changes are always upwards: 1.8.1 values were biased toward drought. |
| `<spi>`/`<spei>`, `fit = "pp-pwm"` or `"max-lik"` | #67 | All | 1.8.1 returned only `NA`. |
| `<spi>`/`<spei>`, `Gamma` or `PearsonIII` with `params` | pze stored in coefficients | All | Results now equal those of the call that produced `params`. In 1.8.1 the difference was masked by #87. |
| `<spi>`/`<spei>`, a series or month that cannot be fitted, in multi-series input or with `ref.start` | #64 | Values wrongly set to `NA` | Those values are now computed. |
| `<thornthwaite>`, `<hargreaves>`, `<penman>`, matrix or array input that is not a time series, with a number of rows that is not a multiple of 12 | Calendar of sites after the first was shifted | All sites except the first | `wichita` (382 months) as two identical columns: `<thornthwaite>` second column mean 15%, max 31%; `<hargreaves>` second column mean 33%, max 74%. |
| `<penman>` with `P` or `P0` | Clear-sky radiation used a fixed 840 m instead of `z` | Most months | `wichita` (z = 402.6 m) with `P`: mean -0.3%, max -0.7%. The change grows with the difference between `z` and 840 m. |
| `<penman>`, solar radiation above clear-sky radiation | Rs/Rso limited to 1 (FAO-56 eq. 39) | Only those months | `wichita` with `tsun`: 9 of 382 months (Oct 2000 to Jun 2001, where `TSUN` exceeds the day length), mean +8%, max +29%. |
| `<penman>`, `method = "ASCE"` with `RH` | Vapour pressure from mean of e0(Tmax) and e0(Tmin) | All | `cabinda`: mean -0.8%, max -1.1%. |
| `<penman>`, `<hargreaves>`, latitudes above about 66.5 degrees in polar summer | Sunset hour angle set to pi | Polar-day months | 70 degrees N: June +1.8 to 2.0%, July +0.9 to 1.1%. 75 degrees N (`<penman>`): May +2.9%, June +4.9%, July +3.9%. |
| `<penman>` with `tsun` in polar night; `<thornthwaite>` for an all-cold site among others; `<penman>` with 1 or 2 time steps | Fixes P5, P6, P8 | `NA`, `NaN` or an error before | Values are now returned. |

Other changes in behaviour that can break existing scripts (see below):
`<penman>` requires `z`, `<thornthwaite>` requires at least 12 months,
`scale` must be a positive integer, and the coefficients of `Gamma` and
`PearsonIII` have an extra row `pze`.

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

* `<spei>` and `<spi>`: the adjustment for the probability of zero
  precipitation (`pze`) was overwritten for the `Gamma` and `PearsonIII`
  distributions, so zero-precipitation periods were returned as `-Inf` and
  non-zero periods were biased toward drier values. This regression was
  introduced in 1.8.0; `log-Logistic` was not affected. Thanks to Mark L.
  Morrissey for reporting it (#87; also #79, #85).
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
* `<penman>`: the ratio Rs/Rso in the net longwave radiation term is now
  limited to 1, as in FAO-56 eq. 39.
* `<penman>`: with `method = "ASCE"` and `RH`, the actual vapour pressure was
  computed from the saturation pressure at mean temperature instead of the
  mean of the saturation pressures at maximum and minimum temperature
  (ASCE-EWRI 2005, eq. 41).
* `<kern.plot>` restores the graphics parameters it changes.
* `<plot.spei>`: colours were assigned by factor order with inverted labels,
  so a series with only negative (or only positive) values was drawn
  entirely in the wet colour (#72).
* `<plot.spei>`: the y-axis label was taken from the deparsed call, so it
  failed when `spi`/`spei` were called under another name and could give the
  wrong label with `do.call()`. The index name is now stored in the object
  (`$index`).
* `<plot.spei>`: the shading of the reference period started one month late
  and assumed monthly data.
* `<plot.spei>`: every plot gave a "Removed n rows containing missing values"
  warning.
* `<summary.spei>`: the fitted values were printed once per series.

## Other changes

* `<spei>` and `<spi>`: the `Gamma` and `PearsonIII` distributions now warn
  when the data contain negative values, which they cannot fit.
* `<thornthwaite>` now requires at least 12 months of data. With fewer, the
  annual heat index was computed from a partial year without notice (#68).
* `<spei>`, `<spi>`, `<penman>`, `<hargreaves>` and `<thornthwaite>` report
  the computation options with `message()` instead of `print()`, so they can
  be silenced with `suppressMessages()` as well as with `verbose = FALSE`
  (#61). `<spi>` now names the SPI, not the SPEI, in this report and in
  `$info`.
* `<spei>` and `<spi>` warn when periods of the year could not be fitted
  (fewer than 4 non-zero values, or constant values, in the reference
  period) and were set to `NA` (#76, #77).
* `<spei>` and `<spi>`: `scale` must be a positive integer no larger than the
  length of the series. Before, non-integer values were accepted, 0 gave a
  misleading error, and a scale longer than the series returned only `NA`.
* Clearer errors for malformed arguments: an unnamed `kernel` list, `na.rm`
  or `verbose` set to `NA`, `params` that is not a 3-d array, and an unknown
  `method` in `<penman>`. A `kernel` shift given as an integer (`0L`) is now
  accepted.
* Tests no longer depend on R's English error messages.
* `<plot.spei>` has a `main` argument for a title, and the y-axis breaks
  cover the full range of values instead of stopping at -2 and 2 (#38). The
  documentation explains that the methods are called as `plot(x)`,
  `summary(x)` and `print(x)` (#78).
* The dependency on the retired `reshape` package has been removed.
* Unused imports and the unused internal function `pglo()` were removed.

## Documentation

* Documentation of the PET functions revised: `P` is not a constant but is
  computed from `z` when missing; `z` is required by `<penman>`; `U2` must be
  wind speed at 2 m (no height conversion is done); the CO2 correction is
  applied to both reference crops; minimum input lengths; and the
  Thornthwaite method does not use the original table for temperatures of
  26.5 ºC or more. The Hargreaves equation is now cited as Hargreaves and
  Samani (1985) (#86).
* Dataset documentation corrected: `YEAR` and `MONTH` in `wichita`, wind
  speed units (m s-1) in `wichita` and `cabinda`, the source of `cabinda`,
  and the country of Lahore in `balance`.
* Licence field changed from `GPL-2` to `GPL (>= 2)`, matching the "version 2
  or later" terms stated in the README and package documentation.
* README: licence badge, repository links and a reference fixed; the version
  history now points to NEWS.md.
* `wichita` dataset: `TSUN` was missing from the documentation (`ACSH` was
  listed twice), and five `TSUN` values between October 2000 and May 2001
  exceed the possible day length; this is now noted.

# SPEI 1.8.1, July 2023

* Maintenance release with fixes for changes in R-devel, required by CRAN.


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
