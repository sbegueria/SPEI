SPEI
====

An R package incorporating a set of functions for computing potential evapotranspiration and several widely used drought indices including the Standardized Precipitation-Evapotranspiration Index (SPEI).


Details
--------

Functions spei and spi are the workhorse of the SPEI library. Other functions such as kern, cdfglo or pglo are auxiliary low-level functions and they will not be used directly by the typical user. Functions for computing potential evapotranspiration are provided, too, for helping computing the SPEI. They are: thornthwaite, hargreaves and penman.

References
----------

S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. Journal of Climate 23: 1696, DOI: 10.1175/2009JCLI2909.1.

http://sac.csic.es/spei/

Version history
---------------

_Version 1.6.2, (actually forked)  March 2016:  Corrected an error in \code{\link{spei}} function. It was not working when distribution was Gamma or PeasonIII  and params was not NULL. This was fixed. (Emanuele Cordano, emanuele.cordano@gmail.com) 

 

_Version 1.6, September 2013 (current)

_Version 1.6, September 2013.
1. Corrected an error in the function \code{\link{thornthwaite}} which resulted in wrong potential evapotranspiration estimates when a multivariate time series was used as input.
2. Corrected an error in the function \code{\link{spi}} which resulted in wrong handling of zero precipitation months when using the Gamma or PearsonIII distribution.
3. Minor fixes to the \code{\link{spi}} and \code{\link{plot.spei}} functions to correctly handle \code{spei} objects when they result from a call to \code{\link{spi}}.
4. Modification to the \code{\link{kern}} function, which now yields kernel coefficients averaging one.
5. Corrected an error in the functions \code{\link{spi}} and \code{\link{spei}} which resulted in ub-pwm method being used irrespective of the value of the \code{fit} parameter used, when using the 'Gamma' or 'PearsonIII' distributions.
6. Minor changes to the documentation.

_Version 1.5, May 2013.

1. Optimization of function \code{\link{spei}}, now using embed() for accumulating the data at the desired time scale.

_Version 1.4, May 2013.

1. Minor fixes to functions \code{\link{penman}} and \code{\link{pwm}}.
2. Documentation of the penman function defined by mistake ed as the saturation vapour pressure, while it should read 'actual vapour pressure'.
3. Function zzz.R added to display basic information about the SPEI package at startup.
4. Function \code{\link{SPEINews}} added to display the NEWS file.

_Version 1.3, March 2013.

1. Minor fixes to functions \code{\link{spei}} and \code{\link{penman}}.
2. Added new option for user-supplied SPEI parameters in the \code{\link{spei}} function. This overrides the fitting of a probability function to the data.
3. Added new dataset \code{\link{cabinda}} from Allen et al. (1998).

_Version 1.2, October 2012.

1. Fixed a bug causing several functions to fail when a time series not belonging to matrix class was provided.
2. Function \code{\link{plot.spei}} now distinguises between calls to spei and spi and labels the axis accordingly.

_Version 1.1, March 2012.

1. Functions \code{\link{spei}} and \code{\link{spi}} now yield an object of class "spei".
2. New functions for summarizing and plotting "spei" objects are provided.
3. An option to establish a reference period for the computation of the indices has been implemented in functions \code{\link{spei}} and \code{\link{spi}}.

_Version 1.0, January 2012.

First release of the SPEI package.

To do (work in progress)
------------------------

1. Complete documentation for pwmLC.Rd.
2. Review method plot.spei() that produces wrong results in some cases.
3. Parallel processing - branch parallel.
4. Analysis function - branch corel_anal.

Any problems?
-------------
Feel free to [write an issue](https://github.com/sbegueria/SPEI/issues) if you have any questions or problems.

Copyright and license
---------------------

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
