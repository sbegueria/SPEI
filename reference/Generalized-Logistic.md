# Generalized Logistic maximum likelihood function

Maximum likelihood fitting function for generalized logistic
distribution.

## Usage

``` r
parglo.maxlik(x, ini)
```

## Arguments

- x:

  vector of quantiles for which to evaluate the PDF.

- ini:

  a vector of initial values of the parameters to be fit.

## Value

a list of parameters of a generalized Logistic distribution function

## Details

This function is used internally by `spei` and `spi` and is supposed to
never be needed by the regular user. Initial values for maximum
likelihood estimation can be provided by `parglo`.

## References

S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A
Multi-scalar drought index sensitive to global warming: The Standardized
Precipitation Evapotranspiration Index – SPEI. *Journal of Climate*
**23**: 1696, DOI: 10.1175/2009JCLI2909.1.

## Author

Santiago Beguería
