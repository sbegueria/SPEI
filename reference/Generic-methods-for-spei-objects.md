# Generic methods for `spei` objects.

Generic methods for extracting information and plotting `spei` objects.

See print.spei

See print.spei

## Usage

``` r
# S3 method for class 'spei'
print(x, ...)
# S3 method for class 'spei'
summary(object, ...)
# S3 method for class 'spei'
plot(x, main = NULL, ...)

# S3 method for class 'spei'
summary(object, ...)

# S3 method for class 'spei'
plot(x, main = NULL, ...)
```

## Arguments

- x:

  an object of class `spei`.

- ...:

  additional parameters, not used at present.

- object:

  an object of class `spei`.

- main:

  optional, a character string with the plot title.

## Details

These functions allow extracting information and plotting `spei`
objects. `print` yields the fitted values, i.e. a time series of SPEI or
SPI values. `summary` reports the function call, the parameters of the
PDF used, and the time series of SPEI or SPI values. `plot` produces a
plot of the time series of SPEI or SPI values, with blue and red colors
for positive and negative values, respectively. If a reference period
was used in the function call it is shown by a shaded area. In the event
that NA or Inf values were produced, these are shown by circles. The
plot is a `ggplot2` object, so it can be further modified by adding
`ggplot2` layers or themes.

These are S3 methods: call them as `print(x)`, `summary(x)` and
`plot(x)`. The method functions themselves (e.g. `plot.spei`) are not
exported.

See print.spei

See print.spei

## References

S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A
Multi-scalar drought index sensitive to global warming: The Standardized
Precipitation Evapotranspiration Index – SPEI. *Journal of Climate*
**23**: 1696, DOI: 10.1175/2009JCLI2909.1.

## Author

Santiago Beguería

## Examples

``` r
# See examples of use in the help page of the spei() function.
```
