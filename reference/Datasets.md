# Data sets for illustrating the functions in the SPEI package

Data used in the examples of the SPEI package: `wichita` dataset:
monthly climate in Wichita (Kansas, lat=37.6475, elevation=402.6 m
a.s.l.) from January 1980 to October 2011; `balance` dataset: monthly
climatic water balance (precipitation minus potential
evapotranspiration) at eleven locations around the World, since January
1900 (1296 months); `cabinda`: one year of data for computing
Penman-Monteith ET0 from Allen et al. (1998); `cruts4`: 120 years of
monthly climatic water balance (precipitation minus reference
evapotranspiration) data at six grid points from CRU TS 4.05;
`tarazona`: annual basal area increment of 18 cores of Aleppo pine
(*Pinus halepensis*) at Tarazona (Spain) from 1959 to 2017, and climate
at the same site with 48 periods per year (four per month) from 1961 to
2020.

## Usage

``` r
data(wichita)
data(balance)
data(cabinda)
data(cruts4)
data(tarazona)
```

## Format

`wichita` dataset: a data frame with:

- YEAR:

  year.

- MONTH:

  month of the year (1 to 12).

- PRCP:

  monthly precipitation totals, in mm.

- TMAX:

  monthly mean daily maximum temperature, in ºC.

- TMIN:

  monthly mean daily minimum temperature, in ºC.

- TMED:

  monthly mean temperature, in ºC.

- AWND:

  monthly mean wind speed, in m s-1.

- TSUN:

  monthly mean daily sunshine duration, in h. Note: five values between
  October 2000 and May 2001 (15.0 to 21.1 h) exceed the maximum possible
  day length at this latitude and should be treated with caution.

- ACSH:

  monthly mean cloud cover, in %.

`balance` dataset: a data frame with monthly climatic water balance
(precipitation minus potential evapotranspiration) at Indore (India),
Kimberley (South Africa), Albuquerque (US), Valencia (Spain), Wien
(Austria), Abashiri (Japan), Tampa (US), Sao Paulo (Brazil), Lahore
(Pakistan), Punta Arenas (Chile) and Helsinki (Finland), in mm.
`cabinda` dataset: a data frame with one year of monthly climatic data
at Cabinda (Angola, -5.33S 12.11E 20 m), with:

- mon:

  month of the year

- Tmin:

  monthly mean daily minimum temperature, in ºC.

- Tmax:

  monthly mean daily maximum temperature, in ºC.

- RH:

  monthly mean relative humidity, in %.

- U2:

  monthly mean wind speed at 2 m height, in m s-1.

- tsun:

  monthly mean sunshine hours, in h.

- Rs:

  monthly mean daily incoming solar radiation, MJ m-2 d-1.

- ET0:

  monthly ET0 from the original publication, in mm.

`cruts4` dataset: an array with 120 years of monthly climatic water
balance (precipitation minus reference evapotranspiration) data at six
grid points from CRU TS 4.05 data set. The array has dimensions
\[time=1440, longitude=2, latitude=3\], with time starting in January
1900. Longitudes are (0.25, 0.75), and latitudes (42.25, 42.75, 43.25),
corresponding to the Central Pyrenees between Spain and France.

`tarazona` dataset: a list with three elements, with data from the
unthinned (control) plot of an Aleppo pine (*Pinus halepensis*)
plantation at Tarazona (Zaragoza, Spain; lat=41.8667, lon=-1.8333,
elevation=785 m a.s.l.):

- site:

  a list with the site metadata: `name`, `lat`, `lon`, `elevation` (m
  a.s.l.), `species`, `plot` (the plot code, TAPHD), `treatment`
  (control, i.e. unthinned) and `thinning.year` (1998, the year in which
  the neighbouring plots of the experiment were thinned; the control
  plot was not thinned).

- bai:

  a data frame with the annual basal area increment (BAI, in cm2 per
  year) of the control plot. The first column, `year`, has the years
  (1959 to 2017), and the other 18 columns the BAI of each core, `NA`
  outside the years covered by the core. The cores come from 11 trees.
  The column names are the plot code, the tree number and the core
  letter, e.g. `TAPHD06A` and `TAPHD06B` are two cores of tree TAPHD06.
  BAI has an age and size trend, which must be removed before relating
  it to climate (see
  [`vignette("scale_scan")`](https://sbegueria.github.io/SPEI/articles/scale_scan.md)).

- clim:

  a multivariate time series (`ts`) with frequency 48 (four periods per
  month), from the first period of January 1961 to the last period of
  December 2020 (2880 periods). The periods of each month start on days
  1, 9, 16 and 23, so they span days 1-8, 9-15, 16-22, and 23 to the end
  of the month. The columns are `PRCP`, precipitation in mm per period;
  `ETO`, FAO-56 Penman-Monteith reference evapotranspiration in mm per
  period; `TMIN` and `TMAX`, mean daily minimum and maximum temperature
  in the period, in ºC. Values are rounded to two decimals.

## Source

The `wichita` data were obtained from the Global Historical Climatology
Network (GHCN, <https://www.ncei.noaa.gov/>). Data for the `balance`
dataset were extracted from CRU TS V3.1 and from the 20th Century
Reanalysis V2 data set. Data for the `cabinda` dataset were taken from
Allen et al. (1998), page 69, figure 18. The `cruts4` data were obtained
from the CRU (Climatic Research Unit, University of East Anglia
<https://crudata.uea.ac.uk/>) TS V4.05 data set. The basal area
increment in the `tarazona` dataset comes from Manrique-Alba et al.
(2020), and is part of the dataset: Manrique-Alba, À., Molina, A.J.,
Beguería, S., Camarero, J.J., et al. (2025). Basal Area Increment and
Climate Data from Long-Term Silviculture Monitoring Sites in Spain
(SilvAdapt.net) \[Dataset\]. DIGITAL.CSIC,
[doi:10.20350/digitalcsic/17165](https://doi.org/10.20350/digitalcsic/17165)
. The same data were used in Manrique-Alba et al. (2025). The climate
data were extracted from the gridded dataset of the Spanish drought
monitor (<https://monitordesequia.csic.es>, Vicente-Serrano et al.,
2017). The `tarazona` data are distributed with the package, under its
licence (GPL (\>= 2)), by the data authors. The script used to build the
dataset is `data-raw/tarazona.R` in the package source repository.

## Details

See description.

## References

S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A
Multi-scalar drought index sensitive to global warming: The Standardized
Precipitation Evapotranspiration Index – SPEI. *Journal of Climate*
**23**: 1696, DOI: 10.1175/2009JCLI2909.1.

R.G. Allen, L.S. Pereira, D. Raes, M. Smith. 1998. *Crop
evapotranspiration - Guidelines for computing crop water requirements -
FAO Irrigation and drainage paper 56*. FAO, Rome. ISBN 92-5-104219-5.

À. Manrique-Alba, S. Beguería, A.J. Molina, M. González-Sanchis, M.
Tomàs-Burguera, A.D. del Campo, M. Colangelo, J.J. Camarero. 2020.
Long-term thinning effects on tree growth, drought response and water
use efficiency at two Aleppo pine plantations in Spain. *Science of the
Total Environment* **728**: 138536, DOI:
10.1016/j.scitotenv.2020.138536.

À. Manrique-Alba, S. Beguería, J.J. Camarero, A.J. Molina, et al. 2025.
Adaptive responses to thinning: growth and sensitivity to hydric and
thermal stress in four widely planted pine species. *Agricultural and
Forest Meteorology* **373**: 110777, DOI:
10.1016/j.agrformet.2025.110777.

S.M. Vicente-Serrano, M. Tomas-Burguera, S. Beguería, F. Reig, B.
Latorre, M. Peña-Gallardo, M.Y. Luna, A. Morata, J.C. González-Hidalgo.
2017. A high resolution dataset of drought indices for Spain. *Data*
**2**(3): 22, DOI: 10.3390/data2030022.

## Author

Data ported to R by S. Beguería.

## Examples

``` r
data(wichita)
names(wichita)
#> [1] "YEAR"  "MONTH" "PRCP"  "TMAX"  "TMIN"  "TMED"  "AWND"  "TSUN"  "ACSH" 
summary(wichita)
#>       YEAR          MONTH             PRCP             TMAX      
#>  Min.   :1980   Min.   : 1.000   Min.   :  0.00   Min.   :-4.45  
#>  1st Qu.:1987   1st Qu.: 3.250   1st Qu.: 24.00   1st Qu.:11.09  
#>  Median :1995   Median : 6.000   Median : 51.10   Median :20.70  
#>  Mean   :1995   Mean   : 6.474   Mean   : 67.74   Mean   :20.10  
#>  3rd Qu.:2003   3rd Qu.: 9.000   3rd Qu.: 97.03   3rd Qu.:29.12  
#>  Max.   :2011   Max.   :12.000   Max.   :333.80   Max.   :40.48  
#>                                                                  
#>       TMIN               TMED            AWND             TSUN       
#>  Min.   :-12.9600   Min.   :-8.71   Min.   : 3.490   Min.   : 3.210  
#>  1st Qu.: -0.9325   1st Qu.: 5.09   1st Qu.: 4.650   1st Qu.: 6.475  
#>  Median :  7.7950   Median :14.52   Median : 5.170   Median : 8.340  
#>  Mean   :  7.8089   Mean   :13.96   Mean   : 5.218   Mean   : 8.404  
#>  3rd Qu.: 16.7600   3rd Qu.:23.16   3rd Qu.: 5.660   3rd Qu.:10.200  
#>  Max.   : 24.5400   Max.   :32.46   Max.   :18.220   Max.   :21.140  
#>                                     NAs    :51       NAs    :36      
#>       ACSH      
#>  Min.   :23.23  
#>  1st Qu.:45.70  
#>  Median :55.84  
#>  Mean   :54.54  
#>  3rd Qu.:62.59  
#>  Max.   :80.97  
#>  NAs    :228    
data(balance)
summary(balance)
#>      indore          kimberley        albuquerque         valencia      
#>  Min.   :-355.70   Min.   :-188.10   Min.   :-138.11   Min.   :-141.99  
#>  1st Qu.:-125.12   1st Qu.: -83.36   1st Qu.: -64.22   1st Qu.: -58.73  
#>  Median : -56.05   Median : -47.83   Median : -27.38   Median : -24.91  
#>  Mean   : -50.57   Mean   : -53.70   Mean   : -34.53   Mean   : -21.30  
#>  3rd Qu.: -11.07   3rd Qu.: -24.63   3rd Qu.:  -2.56   3rd Qu.:   4.68  
#>  Max.   : 667.03   Max.   : 197.58   Max.   : 105.45   Max.   : 300.52  
#>      viena           abashiri          tampa            sao_paulo      
#>  Min.   :-89.14   Min.   :-91.56   Min.   :-161.450   Min.   :-109.69  
#>  1st Qu.:-10.73   1st Qu.: 11.03   1st Qu.: -46.070   1st Qu.: -26.36  
#>  Median : 16.45   Median : 33.02   Median :  -6.580   Median :  15.26  
#>  Mean   : 17.30   Mean   : 36.00   Mean   :   3.824   Mean   :  33.28  
#>  3rd Qu.: 41.37   3rd Qu.: 58.70   3rd Qu.:  42.915   3rd Qu.:  79.30  
#>  Max.   :178.08   Max.   :276.00   Max.   : 358.970   Max.   : 426.63  
#>      lahore         punta_arenas         helsinki      
#>  Min.   :-456.54   Min.   :-102.560   Min.   :-76.410  
#>  1st Qu.:-179.15   1st Qu.: -51.638   1st Qu.:  2.717  
#>  Median : -77.59   Median : -26.730   Median : 26.310  
#>  Mean   :-100.40   Mean   : -24.553   Mean   : 26.812  
#>  3rd Qu.: -14.47   3rd Qu.:  -2.257   3rd Qu.: 50.865  
#>  Max.   : 352.27   Max.   : 145.460   Max.   :194.700  
data(cruts4)
summary(cruts4)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -265.600  -33.200    4.900    7.755   49.225  188.300 
data(tarazona)
str(tarazona)
#> List of 3
#>  $ site:List of 8
#>   ..$ name         : chr "Tarazona"
#>   ..$ lat          : num 41.9
#>   ..$ lon          : num -1.83
#>   ..$ elevation    : num 785
#>   ..$ species      : chr "Pinus halepensis"
#>   ..$ plot         : chr "TAPHD"
#>   ..$ treatment    : chr "control (unthinned)"
#>   ..$ thinning.year: num 1998
#>  $ bai :'data.frame':    59 obs. of  19 variables:
#>   ..$ year    : int [1:59] 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 ...
#>   ..$ TAPHD06A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD06B: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD10A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD10B: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD15A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD15B: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD25A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD31A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD31B: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD35A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD45A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD53A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD53B: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD55A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD55B: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD59A: num [1:59] 0.207 0.505 1.239 2.329 3.906 ...
#>   ..$ TAPHD79A: num [1:59] NA NA NA NA NA ...
#>   ..$ TAPHD79B: num [1:59] NA NA NA NA NA ...
#>  $ clim: Time-Series [1:2880, 1:4] from 1961 to 2021: 15.1 4.16 16.51 9.75 1.35 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:4] "PRCP" "ETO" "TMIN" "TMAX"
```
