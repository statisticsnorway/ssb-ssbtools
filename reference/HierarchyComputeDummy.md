# HierarchyComputeDummy

From hierarchies to a sparse model matrix with possible cross table by
wrapping HierarchyCompute

## Usage

``` r
HierarchyComputeDummy(
  data,
  hierarchies,
  inputInOutput = TRUE,
  crossTable = FALSE,
  ...
)
```

## Arguments

- data:

  data

- hierarchies:

  hierarchies

- crossTable:

  Cross table in output when TRUE

- ...:

  Further parameters sent to
  [`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md)

## Value

A sparse model matrix or a list of model matrix and cross table

## Details

This function is a special wrapper of
[`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md)
and the input argument hierarchies is specified the same way. That is,
variables can also be coded by `"rowFactor"` ( but not colFactor).

## Author

Øyvind Langsrud

## Examples

``` r
# Data and hierarchies used in the examples
x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
geoHier <- SSBtoolsData("sprt_emp_geoHier")
ageHier <- SSBtoolsData("sprt_emp_ageHier")
HierarchyComputeDummy(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), 
                      inputInOutput = FALSE, crossTable = TRUE)
#> $modelMatrix
#> 18 x 9 sparse Matrix of class "dgCMatrix"
#>       Y15-64:Europe:2014 Y15-64:nonEU:2014 Y15-64:EU:2014 Y15-64:Europe:2015
#>  [1,]                  1                 .              1                  .
#>  [2,]                  1                 1              0                  .
#>  [3,]                  1                 .              1                  .
#>  [4,]                  1                 .              1                  .
#>  [5,]                  1                 1              0                  .
#>  [6,]                  1                 .              1                  .
#>  [7,]                  .                 .              .                  1
#>  [8,]                  .                 .              .                  1
#>  [9,]                  .                 .              .                  1
#> [10,]                  .                 .              .                  1
#> [11,]                  .                 .              .                  1
#> [12,]                  .                 .              .                  1
#> [13,]                  .                 .              .                  .
#> [14,]                  .                 .              .                  .
#> [15,]                  .                 .              .                  .
#> [16,]                  .                 .              .                  .
#> [17,]                  .                 .              .                  .
#> [18,]                  .                 .              .                  .
#>       Y15-64:nonEU:2015 Y15-64:EU:2015 Y15-64:Europe:2016 Y15-64:nonEU:2016
#>  [1,]                 .              .                  .                 .
#>  [2,]                 .              .                  .                 .
#>  [3,]                 .              .                  .                 .
#>  [4,]                 .              .                  .                 .
#>  [5,]                 .              .                  .                 .
#>  [6,]                 .              .                  .                 .
#>  [7,]                 .              1                  .                 .
#>  [8,]                 1              0                  .                 .
#>  [9,]                 .              1                  .                 .
#> [10,]                 .              1                  .                 .
#> [11,]                 1              0                  .                 .
#> [12,]                 .              1                  .                 .
#> [13,]                 .              .                  1                 .
#> [14,]                 .              .                  1                 1
#> [15,]                 .              .                  1                 .
#> [16,]                 .              .                  1                 .
#> [17,]                 .              .                  1                 1
#> [18,]                 .              .                  1                 .
#>       Y15-64:EU:2016
#>  [1,]              .
#>  [2,]              .
#>  [3,]              .
#>  [4,]              .
#>  [5,]              .
#>  [6,]              .
#>  [7,]              .
#>  [8,]              .
#>  [9,]              .
#> [10,]              .
#> [11,]              .
#> [12,]              .
#> [13,]              1
#> [14,]              0
#> [15,]              1
#> [16,]              1
#> [17,]              0
#> [18,]              1
#> 
#> $crossTable
#>      age    geo year
#> 1 Y15-64 Europe 2014
#> 2 Y15-64  nonEU 2014
#> 3 Y15-64     EU 2014
#> 4 Y15-64 Europe 2015
#> 5 Y15-64  nonEU 2015
#> 6 Y15-64     EU 2015
#> 7 Y15-64 Europe 2016
#> 8 Y15-64  nonEU 2016
#> 9 Y15-64     EU 2016
#> 
```
