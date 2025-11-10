# Model matrix representing crossed hierarchies

Make a model matrix, x, that corresponds to data and represents all
hierarchies crossed. This means that aggregates corresponding to
numerical variables can be computed as `t(x) %*% y`, where `y` is a
matrix with one column for each numerical variable.

## Usage

``` r
Hierarchies2ModelMatrix(
  data,
  hierarchies,
  inputInOutput = TRUE,
  crossTable = FALSE,
  total = "Total",
  hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level =
    "level"),
  unionComplement = FALSE,
  reOrder = TRUE,
  select = NULL,
  removeEmpty = FALSE,
  selectionByMultiplicationLimit = 10^7,
  makeColnames = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  Matrix or data frame with data containing codes of relevant variables

- hierarchies:

  List of hierarchies, which can be converted by
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).
  Thus, the variables can also be coded by `"rowFactor"` or `""`, which
  correspond to using the categories in the data.

- inputInOutput:

  Logical vector (possibly recycled) for each element of hierarchies.
  TRUE means that codes from input are included in output. Values
  corresponding to `"rowFactor"` or `""` are ignored. Also see note.

- crossTable:

  Cross table in output when TRUE

- total:

  See
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)

- hierarchyVarNames:

  Variable names in the hierarchy tables as in
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md)

- unionComplement:

  Logical vector (possibly recycled) for each element of hierarchies.
  When TRUE, sign means union and complement instead of addition or
  subtraction. Values corresponding to `"rowFactor"` and `"colFactor"`
  are ignored.

- reOrder:

  When TRUE (default) output codes are ordered in a way similar to a
  usual model matrix ordering.

- select:

  Data frame specifying variable combinations for output or a named list
  specifying code selections for each variable (see details).

- removeEmpty:

  When TRUE and when `select` is not a data frame, empty columns (only
  zeros) are not included in output.

- selectionByMultiplicationLimit:

  With non-NULL `select` and when the number of elements in the model
  matrix exceeds this limit, the computation is performed by a slower
  but more memory efficient algorithm.

- makeColnames:

  Colnames included when TRUE (default).

- verbose:

  Whether to print information during calculations. FALSE is default.

- ...:

  Extra unused parameters

## Value

A sparse model matrix or a list of two elements (model matrix and cross
table)

## Details

This function makes use of
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)
and
[`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md)
via
[`HierarchyComputeDummy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyComputeDummy.md).
Since the dummy matrix is transposed in comparison to
`HierarchyCompute`, the parameter `rowSelect` is renamed to `select` and
`makeRownames` is renamed to `makeColnames`.

The select parameter as a list can be partially specified in the sense
that not all hierarchy names have to be included. The parameter
`inputInOutput` will only apply to hierarchies that are not in the
`select` list (see note).

## Note

The `select` as a list is run via a special coding of the
`inputInOutput` parameter. This parameter is converted into a list
(`as.list`) and `select` elements are inserted into this list. This is
also an additional option for users of the function.

## See also

[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md),
[`HierarchiesAndFormula2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchiesAndFormula2ModelMatrix.md)

## Author

Øyvind Langsrud

## Examples

``` r
# Create some input
z <- SSBtoolsData("sprt_emp_withEU")
ageHier <- SSBtoolsData("sprt_emp_ageHier")
geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]


# First example has list output
Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), inputInOutput = FALSE, 
                        crossTable = TRUE)
#> $modelMatrix
#> 18 x 3 sparse Matrix of class "dgCMatrix"
#>       Y15-64:Europe Y15-64:EU Y15-64:nonEU
#>  [1,]             1         1            .
#>  [2,]             1         .            1
#>  [3,]             1         1            .
#>  [4,]             1         1            .
#>  [5,]             1         .            1
#>  [6,]             1         1            .
#>  [7,]             1         1            .
#>  [8,]             1         .            1
#>  [9,]             1         1            .
#> [10,]             1         1            .
#> [11,]             1         .            1
#> [12,]             1         1            .
#> [13,]             1         1            .
#> [14,]             1         .            1
#> [15,]             1         1            .
#> [16,]             1         1            .
#> [17,]             1         .            1
#> [18,]             1         1            .
#> 
#> $crossTable
#>      age    geo
#> 1 Y15-64 Europe
#> 2 Y15-64     EU
#> 3 Y15-64  nonEU
#> 


m1 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), inputInOutput = FALSE)
m2 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList))
m3 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""),
                              inputInOutput = FALSE)
m4 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = "allYears"), 
                              inputInOutput = c(FALSE, FALSE, TRUE))

# Illustrate the effect of unionComplement, geoHier2 as in the examples of HierarchyCompute
geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
                  SSBtoolsData("sprt_emp_geoHier")[, -4])
m5 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoHier2, year = "allYears"), 
                              inputInOutput = FALSE)  # Spain is counted twice
m6 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoHier2, year = "allYears"), 
                              inputInOutput = FALSE, unionComplement = TRUE)


# Compute aggregates
ths_per <- as.matrix(z[, "ths_per", drop = FALSE])  # matrix with the values to be aggregated
Matrix::t(m1) %*% ths_per  # Matrix::crossprod(m1, ths_per) is equivalent and faster
#> 3 x 1 Matrix of class "dgeMatrix"
#>               ths_per
#> Y15-64:Europe   680.8
#> Y15-64:EU       670.2
#> Y15-64:nonEU     10.6
Matrix::t(m2) %*% ths_per
#> 18 x 1 Matrix of class "dgeMatrix"
#>                 ths_per
#> Y15-64:Europe     680.8
#> Y15-64:EU         670.2
#> Y15-64:nonEU       10.6
#> Y15-64:Iceland     10.6
#> Y15-64:Portugal   108.8
#> Y15-64:Spain      561.4
#> Y15-29:Europe     243.5
#> Y15-29:EU         237.9
#> Y15-29:nonEU        5.6
#> Y15-29:Iceland      5.6
#> Y15-29:Portugal    38.5
#> Y15-29:Spain      199.4
#> Y30-64:Europe     437.3
#> Y30-64:EU         432.3
#> Y30-64:nonEU        5.0
#> Y30-64:Iceland      5.0
#> Y30-64:Portugal    70.3
#> Y30-64:Spain      362.0
Matrix::t(m3) %*% ths_per
#> 9 x 1 Matrix of class "dgeMatrix"
#>                    ths_per
#> Y15-64:Europe:2014   222.3
#> Y15-64:Europe:2015   225.0
#> Y15-64:Europe:2016   233.5
#> Y15-64:EU:2014       219.0
#> Y15-64:EU:2015       221.5
#> Y15-64:EU:2016       229.7
#> Y15-64:nonEU:2014      3.3
#> Y15-64:nonEU:2015      3.5
#> Y15-64:nonEU:2016      3.8
Matrix::t(m4) %*% ths_per
#> 12 x 1 Matrix of class "dgeMatrix"
#>                        ths_per
#> Y15-64:Europe:allYears   680.8
#> Y15-64:Europe:2014       222.3
#> Y15-64:Europe:2015       225.0
#> Y15-64:Europe:2016       233.5
#> Y15-64:EU:allYears       670.2
#> Y15-64:EU:2014           219.0
#> Y15-64:EU:2015           221.5
#> Y15-64:EU:2016           229.7
#> Y15-64:nonEU:allYears     10.6
#> Y15-64:nonEU:2014          3.3
#> Y15-64:nonEU:2015          3.5
#> Y15-64:nonEU:2016          3.8
Matrix::t(m5) %*% ths_per
#> 4 x 1 Matrix of class "dgeMatrix"
#>                            ths_per
#> Y15-64:EUandSpain:allYears  1231.6
#> Y15-64:EU:allYears           670.2
#> Y15-64:Europe:allYears       680.8
#> Y15-64:nonEU:allYears         10.6
Matrix::t(m6) %*% ths_per
#> 4 x 1 Matrix of class "dgeMatrix"
#>                            ths_per
#> Y15-64:EUandSpain:allYears   670.2
#> Y15-64:EU:allYears           670.2
#> Y15-64:Europe:allYears       680.8
#> Y15-64:nonEU:allYears         10.6


# Example using the select parameter as a data frame
select <- data.frame(age = c("Y15-64", "Y15-29", "Y30-64"), geo = c("EU", "nonEU", "Spain"))
m2a <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), select = select)

# Same result by slower alternative
m2B <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), crossTable = TRUE)
m2b <- m2B$modelMatrix[, Match(select, m2B$crossTable), drop = FALSE]
Matrix::t(m2b) %*% ths_per
#> 3 x 1 Matrix of class "dgeMatrix"
#>              ths_per
#> Y15-64:EU      670.2
#> Y15-29:nonEU     5.6
#> Y30-64:Spain   362.0

# Examples using the select parameter as a list
Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), 
       inputInOutput = FALSE, 
       select = list(geo = c("nonEU", "Portugal")))
#> 18 x 2 sparse Matrix of class "dgCMatrix"
#>       Y15-64:nonEU Y15-64:Portugal
#>  [1,]            .               .
#>  [2,]            1               .
#>  [3,]            .               1
#>  [4,]            .               .
#>  [5,]            1               .
#>  [6,]            .               1
#>  [7,]            .               .
#>  [8,]            1               .
#>  [9,]            .               1
#> [10,]            .               .
#> [11,]            1               .
#> [12,]            .               1
#> [13,]            .               .
#> [14,]            1               .
#> [15,]            .               1
#> [16,]            .               .
#> [17,]            1               .
#> [18,]            .               1
Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), 
       select = list(geo = c("nonEU", "Portugal"), age = c("Y15-64", "Y15-29")))
#> 18 x 4 sparse Matrix of class "dgCMatrix"
#>       Y15-64:nonEU Y15-64:Portugal Y15-29:nonEU Y15-29:Portugal
#>  [1,]            .               .            .               .
#>  [2,]            1               .            1               .
#>  [3,]            .               1            .               1
#>  [4,]            .               .            .               .
#>  [5,]            1               .            .               .
#>  [6,]            .               1            .               .
#>  [7,]            .               .            .               .
#>  [8,]            1               .            1               .
#>  [9,]            .               1            .               1
#> [10,]            .               .            .               .
#> [11,]            1               .            .               .
#> [12,]            .               1            .               .
#> [13,]            .               .            .               .
#> [14,]            1               .            1               .
#> [15,]            .               1            .               1
#> [16,]            .               .            .               .
#> [17,]            1               .            .               .
#> [18,]            .               1            .               .
```
