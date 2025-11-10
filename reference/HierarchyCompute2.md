# Extended Hierarchical Computations

Extended variant of
[`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md)
with several column variables (not just `"colFactor"`). Parameter colVar
splits the hierarchy variables in two groups and this variable overrides
the difference between `"rowFactor"` and `"colFactor"`.

## Usage

``` r
HierarchyCompute2(
  data,
  hierarchies,
  valueVar,
  colVar,
  rowSelect = NULL,
  colSelect = NULL,
  select = NULL,
  output = "data.frame",
  ...
)
```

## Arguments

- data:

  The input data frame

- hierarchies:

  A named list with hierarchies

- valueVar:

  Name of the variable(s) to be aggregated

- colVar:

  Name of the column variable(s)

- rowSelect:

  Data frame specifying variable combinations for output

- colSelect:

  Data frame specifying variable combinations for output

- select:

  Data frame specifying variable combinations for output

- output:

  One of "data.frame" (default), "outputMatrix", "matrixComponents".

- ...:

  Further parameters sent to
  [`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md)

## Value

As specified by the parameter `output`

## Details

Within this function, `HierarchyCompute` is called two times. By
specifying output as `"matrixComponents"`, output from the two runs are
retuned as a list with elements `hcRow` and `hcCol`. The matrix
multiplication in HierarchyCompute is extended to `outputMatrix` `=`
`hcRow$dataDummyHierarchy` `%*%` `hcRow$valueMatrix` `%*%`
`t(hcCol$dataDummyHierarchy)`. This is modified in cases with more than
a single `valueVar`.

## Note

There is no need to call `HierarchyCompute2` directly. The main function
[`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md)
can be used instead.

## See also

[`Hierarchies2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchies2ModelMatrix.md),
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).

## Author

Øyvind Langsrud

## Examples

``` r
x <- SSBtoolsData("sprt_emp")
geoHier <- SSBtoolsData("sprt_emp_geoHier")
ageHier <- SSBtoolsData("sprt_emp_ageHier")

HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
                 colVar = c("age", "year"))
#>      age year    geo ths_per
#> 1 Y15-64 2014 Europe   222.3
#> 2 Y15-64 2014  nonEU     3.3
#> 3 Y15-64 2014     EU   219.0
#> 4 Y15-64 2015 Europe   225.0
#> 5 Y15-64 2015  nonEU     3.5
#> 6 Y15-64 2015     EU   221.5
#> 7 Y15-64 2016 Europe   233.5
#> 8 Y15-64 2016  nonEU     3.8
#> 9 Y15-64 2016     EU   229.7
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per",
                 colVar = c("age", "geo"))
#>      age    geo year ths_per
#> 1 Y15-64 Europe 2014   222.3
#> 2 Y15-64 Europe 2015   225.0
#> 3 Y15-64 Europe 2016   233.5
#> 4 Y15-64  nonEU 2014     3.3
#> 5 Y15-64  nonEU 2015     3.5
#> 6 Y15-64  nonEU 2016     3.8
#> 7 Y15-64     EU 2014   219.0
#> 8 Y15-64     EU 2015   221.5
#> 9 Y15-64     EU 2016   229.7
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
                 colVar = c("age", "year"), output = "matrixComponents")
#> $hcRow
#> $hcRow$dataDummyHierarchy
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>             
#> Europe 1 1 1
#> nonEU  1 . .
#> EU     0 1 1
#> 
#> $hcRow$valueMatrix
#> 3 x 6 sparse Matrix of class "dgCMatrix"
#>         1    2    3     4     5     6
#> [1,]  1.8  1.9  1.9   1.5   1.6   1.9
#> [2,] 11.6 14.2 12.7  20.2  24.3  25.8
#> [3,] 66.9 63.4 69.1 120.3 119.6 122.1
#> 
#> $hcRow$fromCrossCode
#>        geo
#> 1  Iceland
#> 2 Portugal
#> 3    Spain
#> 
#> $hcRow$toCrossCode
#>      geo
#> 1 Europe
#> 2  nonEU
#> 3     EU
#> 
#> 
#> $hcCol
#> $hcCol$dataDummyHierarchy
#> 3 x 6 sparse Matrix of class "dgCMatrix"
#>                        
#> Y15-64:2014 1 . . 1 . .
#> Y15-64:2015 . 1 . . 1 .
#> Y15-64:2016 . . 1 . . 1
#> 
#> $hcCol$codeFrame
#>      age year
#> 1 Y15-64 2014
#> 2 Y15-64 2015
#> 3 Y15-64 2016
#> 
#> 
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
                 colVar = c("age", "geo"), output = "matrixComponents")
#> $hcRow
#> $hcRow$dataDummyHierarchy
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>           
#> 2014 1 . .
#> 2015 . 1 .
#> 2016 . . 1
#> 
#> $hcRow$valueMatrix
#> 3 x 6 sparse Matrix of class "dgCMatrix"
#>        1    2    3   4    5     6
#> [1,] 1.8 11.6 66.9 1.5 20.2 120.3
#> [2,] 1.9 14.2 63.4 1.6 24.3 119.6
#> [3,] 1.9 12.7 69.1 1.9 25.8 122.1
#> 
#> $hcRow$fromCrossCode
#>   year
#> 1 2014
#> 2 2015
#> 3 2016
#> 
#> $hcRow$toCrossCode
#>   year
#> 1 2014
#> 2 2015
#> 3 2016
#> 
#> 
#> $hcCol
#> $hcCol$dataDummyHierarchy
#> 3 x 6 sparse Matrix of class "dgCMatrix"
#>                          
#> Y15-64:Europe 1 1 1 1 1 1
#> Y15-64:nonEU  1 . . 1 . .
#> Y15-64:EU     0 1 1 0 1 1
#> 
#> $hcCol$codeFrame
#>      age    geo
#> 1 Y15-64 Europe
#> 2 Y15-64  nonEU
#> 3 Y15-64     EU
#> 
#> 
```
