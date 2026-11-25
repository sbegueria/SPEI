# Drought and wet events of an index series.

`index_events` finds the drought (or wet) events of a series of a
drought index, such as the SPEI or the SPI, and returns their start,
end, duration, severity, intensity and peak, using the run theory of
Yevjevich (1967).

## Usage

``` r
index_events(x, type = "dry", threshold = NULL, min.duration = 1, pool = 0)
```

## Arguments

- x:

  the index values: an object of class `spei` (as returned by
  [`spei`](https://sbegueria.github.io/SPEI/reference/Drought-indices.md)
  or
  [`spi`](https://sbegueria.github.io/SPEI/reference/Drought-indices.md)),
  whose `fitted` values are used; a univariate or multivariate time
  series (`ts`); a numeric vector; or a numeric matrix with one column
  per series. Three-dimensional arrays (gridded data) are not supported
  (see 'Details').

- type:

  the type of events, either `'dry'` (default) or `'wet'`.

- threshold:

  the threshold that defines an event. A dry event is a run of
  consecutive time steps with the index at or below the threshold, and a
  wet event a run with the index at or above it. Defaults to -1 for dry
  events and to 1 for wet events.

- min.duration:

  a positive integer, the minimum duration (number of time steps) of an
  event. Shorter events are discarded. Defaults to 1 (all events are
  kept).

- pool:

  a non-negative integer. Events separated by `pool` time steps or fewer
  are merged into one event (pooling). Defaults to 0 (no pooling).

## Value

A data frame with one row per event, in chronological order (and by
series, if there are several), with the columns:

- `site`: only if `x` has several series (columns), the series of the
  event: its column name, or its column number if the columns have no
  names.

- `start`, `end`: the first and the last time step of the event.

- `duration`: the number of time steps of the event.

- `severity`: the sum of the index values over the event.

- `intensity`: the severity divided by the duration.

- `peak`: the most extreme index value of the event.

- `peak.time`: the time step of the peak (the first one, if it is
  reached more than once).

If `x` is a time series with a whole-number frequency, the time steps
(`start`, `end` and `peak.time`) are labels: `'YYYY-MM'` for monthly
series (e.g. `'2011-03'`), `'YYYY'` for annual series, and `'YYYY-Pnn'`
for other frequencies, with the period `nn` within the year (e.g.
`'1961-P05'` for the fifth period of 1961). For time series with other
frequencies they are the decimal times given by
[`time`](https://rdrr.io/r/stats/time.html). If `x` carries no time
information (a vector or a matrix), they are the positions of the time
steps in the series. If there are no events, the data frame has no rows.

## Details

Following the run theory (Yevjevich, 1967), an event is a run of
consecutive time steps in which the index is beyond a threshold: at or
below `threshold` for dry events, at or above it for wet events. The run
starts at the first time step beyond the threshold and ends at the last
one.

A missing value (`NA`) breaks an event: the time steps before and after
it belong to different events. Missing values at the start of the series
(the first `scale - 1` time steps of the SPEI or the SPI at time scale
`scale`) are simply skipped.

With `pool` greater than 0, two consecutive events separated by `pool`
or fewer time steps are merged into one event, which spans from the
start of the first one to the end of the second one. The time steps
between them count in the duration of the merged event, and their values
in its severity. Events separated by a missing value are never merged.
`min.duration` is applied after pooling.

The severity of an event is the sum of the index values over its time
steps, so it is negative for dry events (with a negative threshold) and
positive for wet events. Its intensity is the severity divided by the
duration (the mean value of the index during the event), and its peak is
the most extreme value of the index: the lowest one for dry events and
the highest one for wet events. Note that other definitions of severity
are used in the literature, such as the cumulative deficit below the
threshold; it can be computed from the output as
`threshold * duration - severity` for dry events.

The function works on series. To get the events of each cell of gridded
data (a 3-d array), apply it to the series of each cell, e.g.
`index_events(x[, i, j])`.

## References

Yevjevich, V. 1967. An objective approach to definitions and
investigations of continental hydrologic droughts. Hydrology Paper 23,
Colorado State University, Fort Collins.

## See also

[`spei`](https://sbegueria.github.io/SPEI/reference/Drought-indices.md),
[`spi`](https://sbegueria.github.io/SPEI/reference/Drought-indices.md).

## Examples

``` r
# SPEI-12 at Wichita
data(wichita)
wichita$PET <- thornthwaite(wichita$TMED, 37.6475, verbose = FALSE)
wichita$BAL <- wichita$PRCP - wichita$PET
wichita <- ts(wichita[, -c(1, 2)], end = c(2011, 10), frequency = 12)
spei12 <- spei(wichita[, "BAL"], 12, verbose = FALSE)

# Drought events (SPEI-12 at or below -1)
index_events(spei12)
#>      start     end duration   severity intensity      peak peak.time
#> 1  1980-12 1981-09       10 -16.317124 -1.631712 -1.916626   1981-04
#> 2  1985-04 1985-05        2  -2.397704 -1.198852 -1.232911   1985-05
#> 3  1988-08 1989-06       11 -17.732251 -1.612023 -1.884387   1989-04
#> 4  1990-08 1992-01       18 -27.378900 -1.521050 -1.865432   1991-03
#> 5  1994-05 1994-11        7  -8.428643 -1.204092 -1.356032   1994-06
#> 6  1996-06 1996-08        3  -3.132460 -1.044153 -1.060782   1996-06
#> 7  2001-07 2001-07        1  -1.164280 -1.164280 -1.164280   2001-07
#> 8  2001-11 2002-06        8 -10.523032 -1.315379 -1.536284   2002-03
#> 9  2010-11 2010-11        1  -1.023504 -1.023504 -1.023504   2010-11
#> 10 2011-05 2011-10        6  -9.628277 -1.604713 -1.808277   2011-09

# Only those lasting six months or more, merging events separated by one
# or two months
index_events(spei12, min.duration = 6, pool = 2)
#>     start     end duration   severity intensity      peak peak.time
#> 1 1980-12 1981-09       10 -16.317124 -1.631712 -1.916626   1981-04
#> 2 1988-08 1989-06       11 -17.732251 -1.612023 -1.884387   1989-04
#> 3 1990-08 1992-01       18 -27.378900 -1.521050 -1.865432   1991-03
#> 4 1994-05 1994-11        7  -8.428643 -1.204092 -1.356032   1994-06
#> 5 2001-11 2002-06        8 -10.523032 -1.315379 -1.536284   2002-03
#> 6 2011-05 2011-10        6  -9.628277 -1.604713 -1.808277   2011-09

# Wet events (SPEI-12 at or above 1)
index_events(spei12, type = "wet")
#>      start     end duration  severity intensity     peak peak.time
#> 1  1987-08 1987-08        1  1.009934  1.009934 1.009934   1987-08
#> 2  1993-02 1993-02        1  1.066617  1.066617 1.066617   1993-02
#> 3  1993-05 1993-10        6  7.859985  1.309998 1.817992   1993-05
#> 4  1995-06 1995-06        1  1.108589  1.108589 1.108589   1995-06
#> 5  1995-08 1995-09        2  2.344999  1.172500 1.179370   1995-08
#> 6  1998-01 1998-01        1  1.038569  1.038569 1.038569   1998-01
#> 7  1998-03 1998-04        2  2.217452  1.108726 1.172832   1998-03
#> 8  1999-06 2000-07       14 21.163668  1.511691 2.053076   2000-03
#> 9  2004-07 2004-08        2  2.593585  1.296793 1.451115   2004-07
#> 10 2008-05 2009-08       16 32.505565  2.031598 2.905221   2009-04
#> 11 2009-10 2009-10        1  1.061464  1.061464 1.061464   2009-10
#> 12 2010-02 2010-02        1  1.011018  1.011018 1.011018   2010-02

# All the runs of negative values, as in the run theory with threshold 0
head(index_events(spei12, threshold = 0))
#>     start     end duration     severity   intensity        peak peak.time
#> 1 1980-12 1982-05       18 -20.15861494 -1.11992305 -1.91662561   1981-04
#> 2 1982-10 1983-03        6  -2.63316542 -0.43886090 -0.72854831   1982-11
#> 3 1983-05 1983-06        2  -0.26491019 -0.13245509 -0.18625649   1983-06
#> 4 1984-06 1985-08       15 -10.27038420 -0.68469228 -1.23291130   1985-05
#> 5 1986-03 1986-04        2  -0.08774775 -0.04387387 -0.07104789   1986-04
#> 6 1986-07 1986-07        1  -0.04553927 -0.04553927 -0.04553927   1986-07

# Several series: SPEI-6 and SPEI-12
spei6 <- spei(wichita[, "BAL"], 6, verbose = FALSE)
x <- cbind(spei6 = spei6$fitted, spei12 = spei12$fitted)
index_events(x, min.duration = 3)
#>      site   start     end duration   severity intensity      peak peak.time
#> 1   spei6 1980-07 1981-02        8 -12.840544 -1.605068 -1.888597   1980-11
#> 2   spei6 1984-09 1984-11        3  -4.249882 -1.416627 -1.495166   1984-10
#> 3   spei6 1986-04 1986-06        3  -3.485468 -1.161823 -1.452925   1986-05
#> 4   spei6 1988-07 1989-05       11 -15.441664 -1.403788 -1.753958   1988-11
#> 5   spei6 1990-06 1991-07       14 -20.109950 -1.436425 -1.732847   1991-06
#> 6   spei6 1994-03 1994-06        4  -5.998997 -1.499749 -1.755436   1994-05
#> 7   spei6 1996-02 1996-06        5  -6.327669 -1.265534 -1.463262   1996-03
#> 8   spei6 2001-08 2002-01        6  -7.525423 -1.254237 -1.485757   2001-12
#> 9   spei6 2006-02 2006-04        3  -4.592306 -1.530769 -1.686173   2006-02
#> 10  spei6 2011-01 2011-10       10 -13.405884 -1.340588 -1.589587   2011-08
#> 11 spei12 1980-12 1981-09       10 -16.317124 -1.631712 -1.916626   1981-04
#> 12 spei12 1988-08 1989-06       11 -17.732251 -1.612023 -1.884387   1989-04
#> 13 spei12 1990-08 1992-01       18 -27.378900 -1.521050 -1.865432   1991-03
#> 14 spei12 1994-05 1994-11        7  -8.428643 -1.204092 -1.356032   1994-06
#> 15 spei12 1996-06 1996-08        3  -3.132460 -1.044153 -1.060782   1996-06
#> 16 spei12 2001-11 2002-06        8 -10.523032 -1.315379 -1.536284   2002-03
#> 17 spei12 2011-05 2011-10        6  -9.628277 -1.604713 -1.808277   2011-09
```
