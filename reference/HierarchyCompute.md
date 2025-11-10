# Hierarchical Computations

This function computes aggregates by crossing several hierarchical
specifications and factorial variables.

## Usage

``` r
HierarchyCompute(
  data,
  hierarchies,
  valueVar,
  colVar = NULL,
  rowSelect = NULL,
  colSelect = NULL,
  select = NULL,
  inputInOutput = FALSE,
  output = "data.frame",
  autoLevel = TRUE,
  unionComplement = FALSE,
  constantsInOutput = NULL,
  hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level =
    "level"),
  selectionByMultiplicationLimit = 10^7,
  colNotInDataWarning = TRUE,
  useMatrixToDataFrame = TRUE,
  handleDuplicated = "sum",
  asInput = FALSE,
  verbose = FALSE,
  reOrder = FALSE,
  reduceData = TRUE,
  makeRownames = NULL
)
```

## Arguments

- data:

  The input data frame

- hierarchies:

  A named (names in `data`) list with hierarchies. Variables can also be
  coded by `"rowFactor"` and `"colFactor"`.

- valueVar:

  Name of the variable(s) to be aggregated.

- colVar:

  When non-NULL, the function
  [`HierarchyCompute2`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute2.md)
  is called. See its documentation for more information.

- rowSelect:

  Data frame specifying variable combinations for output. The colFactor
  variable is not included. In addition `rowSelect="removeEmpty"`
  removes combinations corresponding to empty rows (only zeros) of
  `dataDummyHierarchy`.

- colSelect:

  Vector specifying categories of the colFactor variable for output.

- select:

  Data frame specifying variable combinations for output. The colFactor
  variable is included.

- inputInOutput:

  Logical vector (possibly recycled) for each element of hierarchies.
  TRUE means that codes from input are included in output. Values
  corresponding to `"rowFactor"` and `"colFactor"` are ignored.

- output:

  One of "data.frame" (default), "dummyHierarchies", "outputMatrix",
  "dataDummyHierarchy", "valueMatrix", "fromCrossCode", "toCrossCode",
  "crossCode" (as toCrossCode), "outputMatrixWithCrossCode",
  "matrixComponents", "dataDummyHierarchyWithCodeFrame",
  "dataDummyHierarchyQuick". The latter two do not require `valueVar`
  (`reduceData` set to `FALSE`).

- autoLevel:

  Logical vector (possibly recycled) for each element of hierarchies.
  When TRUE, level is computed by automatic method as in
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md).
  Values corresponding to `"rowFactor"` and `"colFactor"` are ignored.

- unionComplement:

  Logical vector (possibly recycled) for each element of hierarchies.
  When TRUE, sign means union and complement instead of addition or
  subtraction as in
  [`DummyHierarchy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DummyHierarchy.md).
  Values corresponding to `"rowFactor"` and `"colFactor"` are ignored.

- constantsInOutput:

  A single row data frame to be combine by the other output.

- hierarchyVarNames:

  Variable names in the hierarchy tables as in
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md).

- selectionByMultiplicationLimit:

  With non-NULL `rowSelect` and when the number of elements in
  `dataDummyHierarchy` exceeds this limit, the computation is performed
  by a slower but more memory efficient algorithm.

- colNotInDataWarning:

  When TRUE, warning produced when elements of `colSelect` are not in
  data.

- useMatrixToDataFrame:

  When TRUE (default) special functionality for saving time and memory
  is used.

- handleDuplicated:

  Handling of duplicated code rows in data. One of: "sum" (default),
  "sumByAggregate", "sumWithWarning", "stop" (error), "single" or
  "singleWithWarning". With no colFactor sum and
  sumByAggregate/sumWithWarning are different (original values or
  aggregates in "valueMatrix"). When single, only one of the values is
  used (by matrix subsetting).

- asInput:

  When TRUE (FALSE is default) output matrices match input data. Thus
  `valueMatrix` `=` `Matrix(data[, valueVar],ncol=1)`. Only possible
  when no colFactor.

- verbose:

  Whether to print information during calculations. FALSE is default.

- reOrder:

  When TRUE (FALSE is default) output codes are ordered differently,
  more similar to a usual model matrix ordering.

- reduceData:

  When TRUE (default) unnecessary (for the aggregated result) rows of
  `valueMatrix` are allowed to be removed.

- makeRownames:

  When TRUE `dataDummyHierarchy` contains rownames. By default, this is
  decided based on the parameter `output`.

## Value

As specified by the parameter `output`

## Details

A key element of this function is the matrix multiplication:
`outputMatrix` `=` `dataDummyHierarchy` `%*%` `valueMatrix`. The matrix,
`valueMatrix` is a re-organized version of the valueVar vector from
input. In particular, if a variable is selected as `colFactor`, there is
one column for each level of that variable. The matrix,
`dataDummyHierarchy` is constructed by crossing dummy coding of
hierarchies
([`DummyHierarchy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DummyHierarchy.md))
and factorial variables in a way that matches `valueMatrix`. The code
combinations corresponding to rows and columns of `dataDummyHierarchy`
can be obtained as `toCrossCode` and `fromCrossCode`. In the default
data frame output, the `outputMatrix` is stacked to one column and
combined with the code combinations of all variables.

## See also

[`Hierarchies2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchies2ModelMatrix.md),
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).

## Author

Øyvind Langsrud

## Examples

``` r
# Data and hierarchies used in the examples
x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
geoHier <- SSBtoolsData("sprt_emp_geoHier")
ageHier <- SSBtoolsData("sprt_emp_ageHier")

# Two hierarchies and year as rowFactor
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per")
#>      age    geo year ths_per
#> 1 Y15-64 Europe 2014   222.3
#> 2 Y15-64  nonEU 2014     3.3
#> 3 Y15-64     EU 2014   219.0
#> 4 Y15-64 Europe 2015   225.0
#> 5 Y15-64  nonEU 2015     3.5
#> 6 Y15-64     EU 2015   221.5
#> 7 Y15-64 Europe 2016   233.5
#> 8 Y15-64  nonEU 2016     3.8
#> 9 Y15-64     EU 2016   229.7

# Same result with year as colFactor (but columns ordered differently)
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per")
#>   year    age    geo ths_per
#> 1 2014 Y15-64 Europe   222.3
#> 2 2014 Y15-64  nonEU     3.3
#> 3 2014 Y15-64     EU   219.0
#> 4 2015 Y15-64 Europe   225.0
#> 5 2015 Y15-64  nonEU     3.5
#> 6 2015 Y15-64     EU   221.5
#> 7 2016 Y15-64 Europe   233.5
#> 8 2016 Y15-64  nonEU     3.8
#> 9 2016 Y15-64     EU   229.7

# Internally the computations are different as seen when output='matrixComponents'
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
                 output = "matrixComponents")
#> $dataDummyHierarchy
#> 9 x 18 sparse Matrix of class "dgCMatrix"
#>                                                       
#> Y15-64:Europe:2014 1 1 1 1 1 1 . . . . . . . . . . . .
#> Y15-64:nonEU:2014  . 1 . . 1 . . . . . . . . . . . . .
#> Y15-64:EU:2014     1 0 1 1 0 1 . . . . . . . . . . . .
#> Y15-64:Europe:2015 . . . . . . 1 1 1 1 1 1 . . . . . .
#> Y15-64:nonEU:2015  . . . . . . . 1 . . 1 . . . . . . .
#> Y15-64:EU:2015     . . . . . . 1 0 1 1 0 1 . . . . . .
#> Y15-64:Europe:2016 . . . . . . . . . . . . 1 1 1 1 1 1
#> Y15-64:nonEU:2016  . . . . . . . . . . . . . 1 . . 1 .
#> Y15-64:EU:2016     . . . . . . . . . . . . 1 0 1 1 0 1
#> 
#> $valueMatrix
#> 18 x 1 Matrix of class "dgeMatrix"
#>       ths_per
#>  [1,]    66.9
#>  [2,]     1.8
#>  [3,]    11.6
#>  [4,]   120.3
#>  [5,]     1.5
#>  [6,]    20.2
#>  [7,]    63.4
#>  [8,]     1.9
#>  [9,]    14.2
#> [10,]   119.6
#> [11,]     1.6
#> [12,]    24.3
#> [13,]    69.1
#> [14,]     1.9
#> [15,]    12.7
#> [16,]   122.1
#> [17,]     1.9
#> [18,]    25.8
#> 
#> $fromCrossCode
#>       age      geo year
#> 1  Y15-29    Spain 2014
#> 2  Y15-29  Iceland 2014
#> 3  Y15-29 Portugal 2014
#> 4  Y30-64    Spain 2014
#> 5  Y30-64  Iceland 2014
#> 6  Y30-64 Portugal 2014
#> 7  Y15-29    Spain 2015
#> 8  Y15-29  Iceland 2015
#> 9  Y15-29 Portugal 2015
#> 10 Y30-64    Spain 2015
#> 11 Y30-64  Iceland 2015
#> 12 Y30-64 Portugal 2015
#> 13 Y15-29    Spain 2016
#> 14 Y15-29  Iceland 2016
#> 15 Y15-29 Portugal 2016
#> 16 Y30-64    Spain 2016
#> 17 Y30-64  Iceland 2016
#> 18 Y30-64 Portugal 2016
#> 
#> $toCrossCode
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
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
                 output = "matrixComponents")
#> $dataDummyHierarchy
#> 3 x 6 sparse Matrix of class "dgCMatrix"
#>                          
#> Y15-64:Europe 1 1 1 1 1 1
#> Y15-64:nonEU  1 . . 1 . .
#> Y15-64:EU     0 1 1 0 1 1
#> 
#> $valueMatrix
#> 6 x 3 sparse Matrix of class "dgCMatrix"
#>       2014  2015  2016
#> [1,]   1.8   1.9   1.9
#> [2,]  11.6  14.2  12.7
#> [3,]  66.9  63.4  69.1
#> [4,]   1.5   1.6   1.9
#> [5,]  20.2  24.3  25.8
#> [6,] 120.3 119.6 122.1
#> 
#> $fromCrossCode
#>      age      geo
#> 1 Y15-29  Iceland
#> 2 Y15-29 Portugal
#> 3 Y15-29    Spain
#> 4 Y30-64  Iceland
#> 5 Y30-64 Portugal
#> 6 Y30-64    Spain
#> 
#> $toCrossCode
#>      age    geo
#> 1 Y15-64 Europe
#> 2 Y15-64  nonEU
#> 3 Y15-64     EU
#> 


# Include input age groups by setting inputInOutput = TRUE for this variable
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
                 inputInOutput = c(TRUE, FALSE))
#>    year    age    geo ths_per
#> 1  2014 Y15-29 Europe    80.3
#> 2  2014 Y30-64 Europe   142.0
#> 3  2014 Y15-64 Europe   222.3
#> 4  2014 Y15-29  nonEU     1.8
#> 5  2014 Y30-64  nonEU     1.5
#> 6  2014 Y15-64  nonEU     3.3
#> 7  2014 Y15-29     EU    78.5
#> 8  2014 Y30-64     EU   140.5
#> 9  2014 Y15-64     EU   219.0
#> 10 2015 Y15-29 Europe    79.5
#> 11 2015 Y30-64 Europe   145.5
#> 12 2015 Y15-64 Europe   225.0
#> 13 2015 Y15-29  nonEU     1.9
#> 14 2015 Y30-64  nonEU     1.6
#> 15 2015 Y15-64  nonEU     3.5
#> 16 2015 Y15-29     EU    77.6
#> 17 2015 Y30-64     EU   143.9
#> 18 2015 Y15-64     EU   221.5
#> 19 2016 Y15-29 Europe    83.7
#> 20 2016 Y30-64 Europe   149.8
#> 21 2016 Y15-64 Europe   233.5
#> 22 2016 Y15-29  nonEU     1.9
#> 23 2016 Y30-64  nonEU     1.9
#> 24 2016 Y15-64  nonEU     3.8
#> 25 2016 Y15-29     EU    81.8
#> 26 2016 Y30-64     EU   147.9
#> 27 2016 Y15-64     EU   229.7

# Only input age groups by switching to rowFactor
HierarchyCompute(x, list(age = "rowFactor", geo = geoHier, year = "colFactor"), "ths_per")
#>    year    age    geo ths_per
#> 1  2014 Y15-29 Europe    80.3
#> 2  2014 Y30-64 Europe   142.0
#> 3  2014 Y15-29  nonEU     1.8
#> 4  2014 Y30-64  nonEU     1.5
#> 5  2014 Y15-29     EU    78.5
#> 6  2014 Y30-64     EU   140.5
#> 7  2015 Y15-29 Europe    79.5
#> 8  2015 Y30-64 Europe   145.5
#> 9  2015 Y15-29  nonEU     1.9
#> 10 2015 Y30-64  nonEU     1.6
#> 11 2015 Y15-29     EU    77.6
#> 12 2015 Y30-64     EU   143.9
#> 13 2016 Y15-29 Europe    83.7
#> 14 2016 Y30-64 Europe   149.8
#> 15 2016 Y15-29  nonEU     1.9
#> 16 2016 Y30-64  nonEU     1.9
#> 17 2016 Y15-29     EU    81.8
#> 18 2016 Y30-64     EU   147.9

# Select some years (colFactor) including a year not in input data (zeros produced)
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
                 colSelect = c("2014", "2016", "2018"))
#> Warning: Items in colSelect not in data[,'year'] set to zero: 2018
#>   year    age    geo ths_per
#> 1 2014 Y15-64 Europe   222.3
#> 2 2014 Y15-64  nonEU     3.3
#> 3 2014 Y15-64     EU   219.0
#> 4 2016 Y15-64 Europe   233.5
#> 5 2016 Y15-64  nonEU     3.8
#> 6 2016 Y15-64     EU   229.7
#> 7 2018 Y15-64 Europe     0.0
#> 8 2018 Y15-64  nonEU     0.0
#> 9 2018 Y15-64     EU     0.0

# Select combinations of geo and age including a code not in data or hierarchy (zeros produced)
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
                 rowSelect = data.frame(geo = "EU", age = c("Y0-100", "Y15-64", "Y15-29")))
#>   year geo    age ths_per
#> 1 2014  EU Y0-100     0.0
#> 2 2014  EU Y15-64   219.0
#> 3 2014  EU Y15-29    78.5
#> 4 2015  EU Y0-100     0.0
#> 5 2015  EU Y15-64   221.5
#> 6 2015  EU Y15-29    77.6
#> 7 2016  EU Y0-100     0.0
#> 8 2016  EU Y15-64   229.7
#> 9 2016  EU Y15-29    81.8
                 
# Select combinations of geo, age and year 
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
     select = data.frame(geo = c("EU", "Spain"), age = c("Y15-64", "Y15-29"), year = 2015))
#>   year    age   geo ths_per
#> 1 2015 Y15-64    EU   221.5
#> 2 2015 Y15-29 Spain    63.4

# Extend the hierarchy table to illustrate the effect of unionComplement 
# Omit level since this is handled by autoLevel
geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
                  geoHier[, -4])

# Spain is counted twice
HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per")
#>    year    age        geo ths_per
#> 1  2014 Y15-64     Europe   222.3
#> 2  2014 Y15-64      nonEU     3.3
#> 3  2014 Y15-64         EU   219.0
#> 4  2014 Y15-64 EUandSpain   406.2
#> 5  2015 Y15-64     Europe   225.0
#> 6  2015 Y15-64      nonEU     3.5
#> 7  2015 Y15-64         EU   221.5
#> 8  2015 Y15-64 EUandSpain   404.5
#> 9  2016 Y15-64     Europe   233.5
#> 10 2016 Y15-64      nonEU     3.8
#> 11 2016 Y15-64         EU   229.7
#> 12 2016 Y15-64 EUandSpain   420.9

# Can be seen in the dataDummyHierarchy matrix
HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", 
                 output = "matrixComponents")
#> $dataDummyHierarchy
#> 4 x 6 sparse Matrix of class "dgCMatrix"
#>                              
#> Y15-64:Europe     1 1 1 1 1 1
#> Y15-64:nonEU      1 . . 1 . .
#> Y15-64:EU         0 1 1 0 1 1
#> Y15-64:EUandSpain 0 1 2 0 1 2
#> 
#> $valueMatrix
#> 6 x 3 sparse Matrix of class "dgCMatrix"
#>       2014  2015  2016
#> [1,]   1.8   1.9   1.9
#> [2,]  11.6  14.2  12.7
#> [3,]  66.9  63.4  69.1
#> [4,]   1.5   1.6   1.9
#> [5,]  20.2  24.3  25.8
#> [6,] 120.3 119.6 122.1
#> 
#> $fromCrossCode
#>      age      geo
#> 1 Y15-29  Iceland
#> 2 Y15-29 Portugal
#> 3 Y15-29    Spain
#> 4 Y30-64  Iceland
#> 5 Y30-64 Portugal
#> 6 Y30-64    Spain
#> 
#> $toCrossCode
#>      age        geo
#> 1 Y15-64     Europe
#> 2 Y15-64      nonEU
#> 3 Y15-64         EU
#> 4 Y15-64 EUandSpain
#> 

# With unionComplement=TRUE Spain is not counted twice
HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", 
                 unionComplement = TRUE)
#>    year    age        geo ths_per
#> 1  2014 Y15-64     Europe   222.3
#> 2  2014 Y15-64      nonEU     3.3
#> 3  2014 Y15-64         EU   219.0
#> 4  2014 Y15-64 EUandSpain   219.0
#> 5  2015 Y15-64     Europe   225.0
#> 6  2015 Y15-64      nonEU     3.5
#> 7  2015 Y15-64         EU   221.5
#> 8  2015 Y15-64 EUandSpain   221.5
#> 9  2016 Y15-64     Europe   233.5
#> 10 2016 Y15-64      nonEU     3.8
#> 11 2016 Y15-64         EU   229.7
#> 12 2016 Y15-64 EUandSpain   229.7

# With constantsInOutput
HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per",
                 constantsInOutput = data.frame(c1 = "AB", c2 = "CD"))
#>   c1 c2 year    age    geo ths_per
#> 1 AB CD 2014 Y15-64 Europe   222.3
#> 2 AB CD 2014 Y15-64  nonEU     3.3
#> 3 AB CD 2014 Y15-64     EU   219.0
#> 4 AB CD 2015 Y15-64 Europe   225.0
#> 5 AB CD 2015 Y15-64  nonEU     3.5
#> 6 AB CD 2015 Y15-64     EU   221.5
#> 7 AB CD 2016 Y15-64 Europe   233.5
#> 8 AB CD 2016 Y15-64  nonEU     3.8
#> 9 AB CD 2016 Y15-64     EU   229.7
                 
# More that one valueVar
x$y <- 10*x$ths_per
HierarchyCompute(x, list(age = ageHier, geo = geoHier), c("y", "ths_per"))
#>      age    geo    y ths_per
#> 1 Y15-64 Europe 6808   680.8
#> 2 Y15-64  nonEU  106    10.6
#> 3 Y15-64     EU 6702   670.2
```
