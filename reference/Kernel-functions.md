# Time kernel for computing the SPEI at different time scales.

Function `kern` is used internally by
[`spei`](https://sbegueria.github.io/SPEI/reference/Drought-indices.md)
and
[`spi`](https://sbegueria.github.io/SPEI/reference/Drought-indices.md)for
computing drought indices at different time scales.

See kern

## Usage

``` r
kern(scale, type = "rectangular", shift = 0)

kern.plot(scale = 12, shift = 0)
```

## Arguments

- scale:

  numeric, time scale or length of the kernel.

- type:

  character, shape of the kernel function.

- shift:

  numeric, shifting of the kernel peak.

## Value

A vector of length equal to `scale` with weights used for computing the
drought index.

## Details

Drought indices, such as the SPEI or the SPI, are usually computed at
different time scales to adapt to the different response times of
systems affected by drought. This is accomplished by applying a kernel
function to the data prior to computation of the SPEI. Application of a
kernel has the effect of smoothing the temporal variability of the
resulting SPEI, allowing for the major patterns to emerge from the
noise. Other way of considering it is that the kernel allows
incorporating information of previous time steps into the calculation of
the current time step, so the resulting values of the SPEI adapt to the
memory of the system under study.

The most common kernel function is rectangular, i.e. all the data of the
previous *n* time steps are given equal weight. This was the way the
Standardized Precipitation Index (SPI) was defined, and it is also the
way the SPEI is computed. This would be the default option for the
`kern` function. However, data from the past can be thought of as having
a decreasing influence in the current state of the system as the
temporal lag between them increases. The function `kern` allows
weighting the past data as a function of the time lapse, according to a
series of pre-defined shapes. Available options are 'rectangular'
(default), 'triangular', 'circular' and 'gaussian'.

By default the highest weight will be given to the observation of the
current month. However, it is possible to modify this by setting the
`shift` parameter to a value higher than zero. This will cause the
highest weight be given to the *n* antecedent observation.

`kern.plot` produces plots of the weighting factor against the time lag
for the four different kernel shapes so they can be compared.

See kern

## References

S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A
Multi-scalar drought index sensitive to global warming: The Standardized
Precipitation Evapotranspiration Index – SPEI. *Journal of Climate*
**23**: 1696, DOI: 10.1175/2009JCLI2909.1.

## Author

Santiago Beguería

## Examples

``` r
# A rectangular kernel with a time scale of 12 and no shift
kern(12)
#>  [1] 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333
#>  [7] 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333
# A gaussian kernel with a time scale of 12 and no shift
kern(12,'gaussian')
#>  [1] 0.196545478 0.189370187 0.169378103 0.140637140 0.108402649 0.077566977
#>  [7] 0.051524162 0.031771822 0.018187391 0.009664865 0.004767803 0.002183423
# Comparison of the four kernels, with and without shift
kern.plot(12)

kern.plot(12,2)


```
