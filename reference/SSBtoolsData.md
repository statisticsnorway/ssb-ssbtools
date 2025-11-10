# Function that returns a dataset

Function that returns a dataset

## Usage

``` r
SSBtoolsData(dataset)
```

## Arguments

- dataset:

  Name of data set within the SSBtools package

## Value

data frame

## Details

**FIFA2018ABCD:** A hierarchy table based on countries within groups A-D
in the football championship, 2018 FIFA World Cup.

**sprt_emp:** Employment in sport in thousand persons. Data from
Eurostat database.

**sprt_emp_geoHier:** Country hierarchy for the employment in sport
data.

**sprt_emp_ageHier:** Age hierarchy for the employment in sport data.

**sprt_emp_withEU:** The data set sprt_emp extended with a EU variable.

**sp_emp_withEU:** As `sprt_emp_withEU`, but coded differently.

**example1** Example data similar to `sp_emp_withEU`.

**magnitude1:** Example data for magnitude tabulation. Same countries as
above.

**my_km2:** Fictitious grid data.

**mun_accidents:** Fictitious traffic accident by municipality data.

**sosialFiktiv, z1, z1w, z2, z2w, z3, z3w, z3wb:** See
[`sosialFiktiv`](https://statisticsnorway.github.io/ssb-ssbtools/reference/sosialFiktiv.md).

**d4, d1, d1w, d2, d2w, d3, d3w, d3wb:** English translation of the
datasets above.

**d2s, d2ws:** `d2` and `d2w` modified to smaller/easier data.

**power10to1, power10to2, \\\ldots\\:** `power10to`\\i\\ is hierarchical
data with \\10^i\\ rows and \\2\*i\\ columns. Tip: Try
`FindDimLists(SSBtoolsData("power10to3"))`

**code_pairs:** Example dataset with two code columns illustrating
paired categorical codes.

**barcelona2025:** Example data in [poster at expert meeting in
Barcelona
2025](https://langsrud.com/stat/A0_poster_Barcelona_2025.html).

**paris2025_freq:** Example frequency data for INSEE Statistical
Methodology Days (JMS 2025)

**paris2025_micro:** Example microdata for INSEE Statistical Methodology
Days (JMS 2025)

## Author

Øyvind Langsrud and Daniel Lupp

## Examples

``` r
SSBtoolsData("FIFA2018ABCD")
#>        mapsFrom      mapsTo sign level
#> 1     Australia     Oceania    1     1
#> 2          Iran        Asia    1     1
#> 3  Saudi Arabia        Asia    1     1
#> 4         Egypt      Africa    1     1
#> 5       Morocco      Africa    1     1
#> 6       Nigeria      Africa    1     1
#> 7     Argentina     America    1     1
#> 8          Peru     America    1     1
#> 9       Uruguay     America    1     1
#> 10      Croatia      Europe    1     1
#> 11      Denmark      Europe    1     1
#> 12       France      Europe    1     1
#> 13      Iceland      Europe    1     1
#> 14     Portugal      Europe    1     1
#> 15       Russia      Europe    1     1
#> 16        Spain      Europe    1     1
#> 17      Iceland       nonEU    1     1
#> 18       Russia       nonEU    1     1
#> 19       Russia nonSchengen    1     1
#> 20      Croatia nonSchengen    1     1
#> 21       Europe          EU    1     2
#> 22        nonEU          EU   -1     2
#> 23       Europe    Schengen    1     2
#> 24  nonSchengen    Schengen   -1     2
SSBtoolsData("sprt_emp")
#>       age      geo year ths_per
#> 1  Y15-29    Spain 2014    66.9
#> 2  Y15-29  Iceland 2014     1.8
#> 3  Y15-29 Portugal 2014    11.6
#> 4  Y30-64    Spain 2014   120.3
#> 5  Y30-64  Iceland 2014     1.5
#> 6  Y30-64 Portugal 2014    20.2
#> 7  Y15-29    Spain 2015    63.4
#> 8  Y15-29  Iceland 2015     1.9
#> 9  Y15-29 Portugal 2015    14.2
#> 10 Y30-64    Spain 2015   119.6
#> 11 Y30-64  Iceland 2015     1.6
#> 12 Y30-64 Portugal 2015    24.3
#> 13 Y15-29    Spain 2016    69.1
#> 14 Y15-29  Iceland 2016     1.9
#> 15 Y15-29 Portugal 2016    12.7
#> 16 Y30-64    Spain 2016   122.1
#> 17 Y30-64  Iceland 2016     1.9
#> 18 Y30-64 Portugal 2016    25.8
SSBtoolsData("sprt_emp_geoHier")
#>   mapsFrom mapsTo sign level
#> 1  Iceland Europe    1     1
#> 2 Portugal Europe    1     1
#> 3    Spain Europe    1     1
#> 4  Iceland  nonEU    1     1
#> 5   Europe     EU    1     2
#> 6    nonEU     EU   -1     2
SSBtoolsData("sprt_emp_ageHier")
#>   mapsFrom mapsTo sign level
#> 1   Y15-29 Y15-64    1     1
#> 2   Y30-64 Y15-64    1     1
SSBtoolsData("sprt_emp_withEU")
#>       age      geo year ths_per    eu
#> 1  Y15-29    Spain 2014    66.9    EU
#> 2  Y15-29  Iceland 2014     1.8 nonEU
#> 3  Y15-29 Portugal 2014    11.6    EU
#> 4  Y30-64    Spain 2014   120.3    EU
#> 5  Y30-64  Iceland 2014     1.5 nonEU
#> 6  Y30-64 Portugal 2014    20.2    EU
#> 7  Y15-29    Spain 2015    63.4    EU
#> 8  Y15-29  Iceland 2015     1.9 nonEU
#> 9  Y15-29 Portugal 2015    14.2    EU
#> 10 Y30-64    Spain 2015   119.6    EU
#> 11 Y30-64  Iceland 2015     1.6 nonEU
#> 12 Y30-64 Portugal 2015    24.3    EU
#> 13 Y15-29    Spain 2016    69.1    EU
#> 14 Y15-29  Iceland 2016     1.9 nonEU
#> 15 Y15-29 Portugal 2016    12.7    EU
#> 16 Y30-64    Spain 2016   122.1    EU
#> 17 Y30-64  Iceland 2016     1.9 nonEU
#> 18 Y30-64 Portugal 2016    25.8    EU
SSBtoolsData("d1w")
#>   region other wages assistance pensions
#> 1      A    11    11         55       36
#> 2      B     7     1         29       18
#> 3      C     5     8         35       25
#> 4      D    13     2         17       13
#> 5      E     9    14         63       52
#> 6      F    12     9         24       22
#> 7      G     6     4         22        8
#> 8      H     9     3         38       15
SSBtoolsData("barcelona2025")
#>    country  city   age    sex income
#> 1  Denmark  <NA>   old   male    760
#> 2  Denmark  <NA> young female    480
#> 3  Denmark  <NA> young   male   1910
#> 4  Denmark  <NA> young   male   1810
#> 5  Denmark  <NA> young   male    570
#> 6  Finland  <NA>   old female    520
#> 7  Finland  <NA>   old   male   1470
#> 8  Finland  <NA> young female    710
#> 9  Finland  <NA> young female    440
#> 10 Finland  <NA> young   male   1020
#> 11 Finland  <NA> young   male   1550
#> 12  France Paris   old female    940
#> 13  France Paris   old female   4340
#> 14  France Paris   old   male   4730
#> 15  France Paris young female   5630
#> 16  France Paris young female    590
#> 17  France Paris young   male    600
#> 18  France  <NA>   old female    940
#> 19  France  <NA>   old female   1150
#> 20  France  <NA> young female   1090
SSBtoolsData("paris2025_freq")
#>    nation continent   age freq
#> 1 Denmark    Europe   old    1
#> 2 Denmark    Europe young    1
#> 3  France    Europe   old    6
#> 4  France    Europe young    2
#> 5     USA   America   old    3
#> 6     USA   America young    4
```
