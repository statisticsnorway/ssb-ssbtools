# Model matrix representing crossed hierarchies according to a formula

How to cross the hierarchies are defined by a formula. The formula is
automatically simplified when totals are involved.

## Usage

``` r
HierarchiesAndFormula2ModelMatrix(
  data,
  hierarchies,
  formula,
  inputInOutput = TRUE,
  makeColNames = TRUE,
  crossTable = FALSE,
  total = "Total",
  simplify = TRUE,
  hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level =
    "level"),
  unionComplement = FALSE,
  removeEmpty = FALSE,
  reOrder = TRUE,
  sep = "-",
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

- formula:

  A model formula

- inputInOutput:

  Logical vector (possibly recycled) for each element of hierarchies.
  TRUE means that codes from input are included in output. Values
  corresponding to `"rowFactor"` or `""` are ignored.

- makeColNames:

  Colnames included when TRUE (default).

- crossTable:

  Cross table in output when TRUE

- total:

  Vector of total codes (possibly recycled) passed to
  [AutoHierarchies](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md),
  with optional named vector/list support.

- simplify:

  When TRUE (default) the model can be simplified when total codes are
  found in the hierarchies (see examples).

- hierarchyVarNames:

  Variable names in the hierarchy tables as in
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md)

- unionComplement:

  Logical vector (possibly recycled) for each element of hierarchies.
  When TRUE, sign means union and complement instead of addition or
  subtraction. Values corresponding to `"rowFactor"` and `"colFactor"`
  are ignored.

- removeEmpty:

  When TRUE, empty columns (only zeros) are not included in output.

- reOrder:

  When TRUE (default) output codes are ordered in a way similar to a
  usual model matrix ordering.

- sep:

  String to separate when creating column names

- ...:

  Extra unused parameters

## Value

A sparse model matrix or a list of two elements (model matrix and cross
table)

## See also

[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md),
[`Hierarchies2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchies2ModelMatrix.md),
[`Formula2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md).

## Author

Øyvind Langsrud

## Examples

``` r
# Create some input
z <- SSBtoolsData("sprt_emp_withEU")
ageHier <- SSBtoolsData("sprt_emp_ageHier")
geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]

# Shorter function name
H <- HierarchiesAndFormula2ModelMatrix

# Small dataset example. Two dimensions.
s <- z[z$geo == "Spain", ]
geoYear <- list(geo = geoDimList, year = "")
m <- H(s, geoYear, ~geo * year, inputInOutput = c(FALSE, TRUE))
print(m, col.names = TRUE)
#> <S4 Type Object>
#> attr(,"i")
#>  [1] 0 1 2 3 4 5 0 1 2 3 4 5 0 1 2 3 4 5 0 1 2 3 4 5
#> attr(,"p")
#>  [1]  0  6 12 12 14 16 18 20 22 24 24 24 24
#> attr(,"Dim")
#> [1]  6 12
#> attr(,"Dimnames")
#> attr(,"Dimnames")[[1]]
#> NULL
#> 
#> attr(,"Dimnames")[[2]]
#>  [1] "Europe-Total" "EU-Total"     "nonEU-Total"  "Europe-2014"  "Europe-2015" 
#>  [6] "Europe-2016"  "EU-2014"      "EU-2015"      "EU-2016"      "nonEU-2014"  
#> [11] "nonEU-2015"   "nonEU-2016"  
#> 
#> attr(,"x")
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> attr(,"factors")
#> list()
#> attr(,"class")
#> [1] "dgCMatrix"
#> attr(,"class")attr(,"package")
#> [1] "Matrix"
#> attr(,"total")
#>      geo     year 
#> "Europe"  "Total" 
#> attr(,"startCol")
#>      geo geo:year 
#>        1        4 
attr(m, "total")     # Total code 'Europe' is found
#>      geo     year 
#> "Europe"  "Total" 
attr(m, "startCol")  # Two model terms needed
#>      geo geo:year 
#>        1        4 

# Another model and with crossTable in output
H(s, geoYear, ~geo + year, crossTable = TRUE)
#> $modelMatrix
#> 6 x 9 sparse Matrix of class "dgCMatrix"
#>      Europe-Total EU-Total nonEU-Total Iceland-Total Portugal-Total Spain-Total
#> [1,]            1        1           .             .              .           1
#> [2,]            1        1           .             .              .           1
#> [3,]            1        1           .             .              .           1
#> [4,]            1        1           .             .              .           1
#> [5,]            1        1           .             .              .           1
#> [6,]            1        1           .             .              .           1
#>      Europe-2014 Europe-2015 Europe-2016
#> [1,]           1           .           .
#> [2,]           1           .           .
#> [3,]           .           1           .
#> [4,]           .           1           .
#> [5,]           .           .           1
#> [6,]           .           .           1
#> 
#> $crossTable
#>        geo  year
#> 1   Europe Total
#> 2       EU Total
#> 3    nonEU Total
#> 4  Iceland Total
#> 5 Portugal Total
#> 6    Spain Total
#> 7   Europe  2014
#> 8   Europe  2015
#> 9   Europe  2016
#> 

# Without empty columns  
H(s, geoYear, ~geo + year, crossTable = TRUE, removeEmpty = TRUE)
#> $modelMatrix
#> 6 x 6 sparse Matrix of class "dgCMatrix"
#>      Europe-Total EU-Total Spain-Total Europe-2014 Europe-2015 Europe-2016
#> [1,]            1        1           1           1           .           .
#> [2,]            1        1           1           1           .           .
#> [3,]            1        1           1           .           1           .
#> [4,]            1        1           1           .           1           .
#> [5,]            1        1           1           .           .           1
#> [6,]            1        1           1           .           .           1
#> 
#> $crossTable
#>      geo  year
#> 1 Europe Total
#> 2     EU Total
#> 3  Spain Total
#> 4 Europe  2014
#> 5 Europe  2015
#> 6 Europe  2016
#> 

# Three dimensions
ageGeoYear <- list(age = ageHier, geo = geoDimList, year = "allYears")
m <- H(z, ageGeoYear, ~age * geo + geo * year)
head(colnames(m))
#> [1] "Y15-64-Europe-allYears"   "Y15-64-EU-allYears"      
#> [3] "Y15-64-nonEU-allYears"    "Y15-64-Iceland-allYears" 
#> [5] "Y15-64-Portugal-allYears" "Y15-64-Spain-allYears"   
attr(m, "total")
#>        age        geo       year 
#>   "Y15-64"   "Europe" "allYears" 
attr(m, "startCol")
#>  age:geo geo:year 
#>        1       19 

# With simplify = FALSE
m <- H(z, ageGeoYear, ~age * geo + geo * year, simplify = FALSE)
head(colnames(m))
#> [1] "Total-Total-Total"  "Y15-64-Total-Total" "Y15-29-Total-Total"
#> [4] "Y30-64-Total-Total" "Total-Europe-Total" "Total-EU-Total"    
attr(m, "total")
#>     age     geo    year 
#> "Total" "Total" "Total" 
attr(m, "startCol")
#> (Intercept)         age         geo        year     age:geo    geo:year 
#>           1           2           5          11          15          33 

# Compute aggregates
m <- H(z, ageGeoYear, ~geo * age, inputInOutput = c(TRUE, FALSE, TRUE))
Matrix::t(m) %*% z$ths_per
#> 9 x 1 Matrix of class "dgeMatrix"
#>                [,1]
#> Europe-Y15-64 680.8
#> Europe-Y15-29 243.5
#> Europe-Y30-64 437.3
#> EU-Y15-64     670.2
#> EU-Y15-29     237.9
#> EU-Y30-64     432.3
#> nonEU-Y15-64   10.6
#> nonEU-Y15-29    5.6
#> nonEU-Y30-64    5.0

# Without hierarchies. Only factors.
ageGeoYearFactor <- list(age = "", geo = "", year = "")
Matrix::t(H(z, ageGeoYearFactor, ~geo * age + year:geo))
#> 21 x 18 sparse Matrix of class "dgCMatrix"
#>                                                          
#> Total-Total-Total     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> Iceland-Total-Total   . 1 . . 1 . . 1 . . 1 . . 1 . . 1 .
#> Portugal-Total-Total  . . 1 . . 1 . . 1 . . 1 . . 1 . . 1
#> Spain-Total-Total     1 . . 1 . . 1 . . 1 . . 1 . . 1 . .
#> Total-Y15-29-Total    1 1 1 . . . 1 1 1 . . . 1 1 1 . . .
#> Total-Y30-64-Total    . . . 1 1 1 . . . 1 1 1 . . . 1 1 1
#> Iceland-Y15-29-Total  . 1 . . . . . 1 . . . . . 1 . . . .
#> Iceland-Y30-64-Total  . . . . 1 . . . . . 1 . . . . . 1 .
#> Portugal-Y15-29-Total . . 1 . . . . . 1 . . . . . 1 . . .
#> Portugal-Y30-64-Total . . . . . 1 . . . . . 1 . . . . . 1
#> Spain-Y15-29-Total    1 . . . . . 1 . . . . . 1 . . . . .
#> Spain-Y30-64-Total    . . . 1 . . . . . 1 . . . . . 1 . .
#> Iceland-Total-2014    . 1 . . 1 . . . . . . . . . . . . .
#> Iceland-Total-2015    . . . . . . . 1 . . 1 . . . . . . .
#> Iceland-Total-2016    . . . . . . . . . . . . . 1 . . 1 .
#> Portugal-Total-2014   . . 1 . . 1 . . . . . . . . . . . .
#> Portugal-Total-2015   . . . . . . . . 1 . . 1 . . . . . .
#> Portugal-Total-2016   . . . . . . . . . . . . . . 1 . . 1
#> Spain-Total-2014      1 . . 1 . . . . . . . . . . . . . .
#> Spain-Total-2015      . . . . . . 1 . . 1 . . . . . . . .
#> Spain-Total-2016      . . . . . . . . . . . . 1 . . 1 . .
```
