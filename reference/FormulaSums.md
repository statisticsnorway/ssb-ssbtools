# Sums (aggregates) and/or sparse model matrix with possible cross table

By default this function return sums if the formula contains a response
part and a model matrix otherwise

## Usage

``` r
FormulaSums(
  data,
  formula,
  makeNames = TRUE,
  crossTable = FALSE,
  total = "Total",
  printInc = FALSE,
  dropResponse = FALSE,
  makeModelMatrix = NULL,
  sep = "-",
  sepCross = ":",
  avoidHierarchical = FALSE,
  includeEmpty = FALSE,
  NAomit = TRUE,
  rowGroupsPackage = "base",
  viaSparseMatrix = TRUE,
  ...
)

Formula2ModelMatrix(data, formula, dropResponse = TRUE, ...)
```

## Arguments

- data:

  data frame

- formula:

  A model formula

- makeNames:

  Column/row names made when TRUE

- crossTable:

  Cross table in output when TRUE

- total:

  Total code(s) which can be provided in several forms:

  - A single string (e.g. `"TOTAL"`) to be applied to all variables.

  - A **named vector** or **named list** giving one total code per
    variable, e.g. `c(var1 = "ALL_A", var2 = "ALL_B")` or
    `list(var1 = "ALL_A", var2 = "ALL_B")`.

- printInc:

  Printing "..." to console when TRUE

- dropResponse:

  When TRUE response part of formula ignored.

- makeModelMatrix:

  Make model matrix when TRUE. NULL means automatic.

- sep:

  String to separate when creating column names

- sepCross:

  String to separate when creating column names involving crossing

- avoidHierarchical:

  Whether to avoid treating of hierarchical variables. Instead of
  logical, variables can be specified.

- includeEmpty:

  When `TRUE`, empty columns of the model matrix (only zeros) are
  included. This is not implemented when a response term is included in
  the formula and `dropResponse = FALSE` (error will be produced).

- NAomit:

  When `TRUE`, NAs in the grouping variables are omitted in output and
  not included as a separate category. Technically, this parameter is
  utilized through the function
  [`RowGroups`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md).

- rowGroupsPackage:

  Parameter `pkg` to the function
  [`RowGroups`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md).
  Default is `"base"`. Setting this parameter to `"data.table"` can
  improve speed.

- viaSparseMatrix:

  When TRUE, the model matrix is constructed by a single call to
  [`sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html).
  Setting it to FALSE reverts to the previous behavior. This parameter
  is included for testing purposes and will likely be removed in future
  versions.

- ...:

  Further arguments to be passed to `FormulaSums`

## Value

A matrix of sums, a sparse model matrix or a list of two or three
elements (model matrix and cross table and sums when relevant).

## Details

In the original version of the function the model matrix was constructed
by calling
[`fac2sparse`](https://rdrr.io/pkg/Matrix/man/sparse.model.matrix.html)
repeatedly. Now this is replaced by a single call to
[`sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html). The
sums are computed by calling
[`aggregate`](https://rdrr.io/r/stats/aggregate.html) repeatedly.
Hierarchical variables handled when constructing cross table. Column
names constructed from the cross table. The returned model matrix
includes the attribute `startCol` (see last example line).

## See also

[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)

## Author

Øyvind Langsrud

## Examples

``` r
x <- SSBtoolsData("sprt_emp_withEU")

FormulaSums(x, ths_per ~ year*geo + year*eu)
#>                ths_per
#> Total-Total      680.8
#> 2014-Total       222.3
#> 2015-Total       225.0
#> 2016-Total       233.5
#> Total-Iceland     10.6
#> Total-Portugal   108.8
#> Total-Spain      561.4
#> Total-EU         670.2
#> Total-nonEU       10.6
#> 2014-Iceland       3.3
#> 2014-Portugal     31.8
#> 2014-Spain       187.2
#> 2015-Iceland       3.5
#> 2015-Portugal     38.5
#> 2015-Spain       183.0
#> 2016-Iceland       3.8
#> 2016-Portugal     38.5
#> 2016-Spain       191.2
#> 2014-EU          219.0
#> 2014-nonEU         3.3
#> 2015-EU          221.5
#> 2015-nonEU         3.5
#> 2016-EU          229.7
#> 2016-nonEU         3.8
FormulaSums(x, ~ year*age*eu)
#> 18 x 36 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 36 column names ‘Total-Total-Total’, ‘2014-Total-Total’, ‘2015-Total-Total’ ... ]]
#>                                                                              
#>  [1,] 1 1 . . 1 . 1 . 1 . . . . . 1 . . . . . 1 . . . 1 . . . . . . . . . . .
#>  [2,] 1 1 . . 1 . . 1 1 . . . . . . 1 . . . . . 1 . . . 1 . . . . . . . . . .
#>  [3,] 1 1 . . 1 . 1 . 1 . . . . . 1 . . . . . 1 . . . 1 . . . . . . . . . . .
#>  [4,] 1 1 . . . 1 1 . . 1 . . . . 1 . . . . . . . 1 . . . 1 . . . . . . . . .
#>  [5,] 1 1 . . . 1 . 1 . 1 . . . . . 1 . . . . . . . 1 . . . 1 . . . . . . . .
#>  [6,] 1 1 . . . 1 1 . . 1 . . . . 1 . . . . . . . 1 . . . 1 . . . . . . . . .
#>  [7,] 1 . 1 . 1 . 1 . . . 1 . . . . . 1 . . . 1 . . . . . . . 1 . . . . . . .
#>  [8,] 1 . 1 . 1 . . 1 . . 1 . . . . . . 1 . . . 1 . . . . . . . 1 . . . . . .
#>  [9,] 1 . 1 . 1 . 1 . . . 1 . . . . . 1 . . . 1 . . . . . . . 1 . . . . . . .
#> [10,] 1 . 1 . . 1 1 . . . . 1 . . . . 1 . . . . . 1 . . . . . . . 1 . . . . .
#> [11,] 1 . 1 . . 1 . 1 . . . 1 . . . . . 1 . . . . . 1 . . . . . . . 1 . . . .
#> [12,] 1 . 1 . . 1 1 . . . . 1 . . . . 1 . . . . . 1 . . . . . . . 1 . . . . .
#> [13,] 1 . . 1 1 . 1 . . . . . 1 . . . . . 1 . 1 . . . . . . . . . . . 1 . . .
#> [14,] 1 . . 1 1 . . 1 . . . . 1 . . . . . . 1 . 1 . . . . . . . . . . . 1 . .
#> [15,] 1 . . 1 1 . 1 . . . . . 1 . . . . . 1 . 1 . . . . . . . . . . . 1 . . .
#> [16,] 1 . . 1 . 1 1 . . . . . . 1 . . . . 1 . . . 1 . . . . . . . . . . . 1 .
#> [17,] 1 . . 1 . 1 . 1 . . . . . 1 . . . . . 1 . . . 1 . . . . . . . . . . . 1
#> [18,] 1 . . 1 . 1 1 . . . . . . 1 . . . . 1 . . . 1 . . . . . . . . . . . 1 .
FormulaSums(x, ths_per ~ year*age*geo + year*age*eu, crossTable = TRUE, makeModelMatrix = TRUE)
#> $modelMatrix
#> 18 x 72 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 72 column names ‘Total-Total-Total’, ‘2014-Total-Total’, ‘2015-Total-Total’ ... ]]
#>                                                                                
#>  [1,] 1 1 . . 1 . . . 1 1 . 1 . . . . . . . 1 . . . . . . . . 1 . . . 1 . . . .
#>  [2,] 1 1 . . 1 . 1 . . . 1 1 . . . . . 1 . . . . . . . . 1 . . . . . . 1 . . .
#>  [3,] 1 1 . . 1 . . 1 . 1 . 1 . . . . . . 1 . . . . . . . . 1 . . . . 1 . . . .
#>  [4,] 1 1 . . . 1 . . 1 1 . . 1 . . . . . . 1 . . . . . . . . . . . 1 1 . . . .
#>  [5,] 1 1 . . . 1 1 . . . 1 . 1 . . . . 1 . . . . . . . . . . . 1 . . . 1 . . .
#>  [6,] 1 1 . . . 1 . 1 . 1 . . 1 . . . . . 1 . . . . . . . . . . . 1 . 1 . . . .
#>  [7,] 1 . 1 . 1 . . . 1 1 . . . 1 . . . . . . . . 1 . . . . . 1 . . . . . 1 . .
#>  [8,] 1 . 1 . 1 . 1 . . . 1 . . 1 . . . . . . 1 . . . . . 1 . . . . . . . . 1 .
#>  [9,] 1 . 1 . 1 . . 1 . 1 . . . 1 . . . . . . . 1 . . . . . 1 . . . . . . 1 . .
#> [10,] 1 . 1 . . 1 . . 1 1 . . . . 1 . . . . . . . 1 . . . . . . . . 1 . . 1 . .
#> [11,] 1 . 1 . . 1 1 . . . 1 . . . 1 . . . . . 1 . . . . . . . . 1 . . . . . 1 .
#> [12,] 1 . 1 . . 1 . 1 . 1 . . . . 1 . . . . . . 1 . . . . . . . . 1 . . . 1 . .
#> [13,] 1 . . 1 1 . . . 1 1 . . . . . 1 . . . . . . . . . 1 . . 1 . . . . . . . 1
#> [14,] 1 . . 1 1 . 1 . . . 1 . . . . 1 . . . . . . . 1 . . 1 . . . . . . . . . .
#> [15,] 1 . . 1 1 . . 1 . 1 . . . . . 1 . . . . . . . . 1 . . 1 . . . . . . . . 1
#> [16,] 1 . . 1 . 1 . . 1 1 . . . . . . 1 . . . . . . . . 1 . . . . . 1 . . . . 1
#> [17,] 1 . . 1 . 1 1 . . . 1 . . . . . 1 . . . . . . 1 . . . . . 1 . . . . . . .
#> [18,] 1 . . 1 . 1 . 1 . 1 . . . . . . 1 . . . . . . . 1 . . . . . 1 . . . . . 1
#>                                                                            
#>  [1,] . 1 . . . . . 1 . . . . . . . . . . . . . . . 1 . . . . . . . . . . .
#>  [2,] . . 1 . . 1 . . . . . . . . . . . . . . . . . . 1 . . . . . . . . . .
#>  [3,] . 1 . . . . 1 . . . . . . . . . . . . . . . . 1 . . . . . . . . . . .
#>  [4,] . . . 1 . . . . . . 1 . . . . . . . . . . . . . . 1 . . . . . . . . .
#>  [5,] . . . . 1 . . . 1 . . . . . . . . . . . . . . . . . 1 . . . . . . . .
#>  [6,] . . . 1 . . . . . 1 . . . . . . . . . . . . . . . 1 . . . . . . . . .
#>  [7,] . 1 . . . . . . . . . . . 1 . . . . . . . . . . . . . 1 . . . . . . .
#>  [8,] . . 1 . . . . . . . . 1 . . . . . . . . . . . . . . . . 1 . . . . . .
#>  [9,] . 1 . . . . . . . . . . 1 . . . . . . . . . . . . . . 1 . . . . . . .
#> [10,] . . . 1 . . . . . . . . . . . . 1 . . . . . . . . . . . . 1 . . . . .
#> [11,] . . . . 1 . . . . . . . . . 1 . . . . . . . . . . . . . . . 1 . . . .
#> [12,] . . . 1 . . . . . . . . . . . 1 . . . . . . . . . . . . . 1 . . . . .
#> [13,] . 1 . . . . . . . . . . . . . . . . . 1 . . . . . . . . . . . 1 . . .
#> [14,] 1 . 1 . . . . . . . . . . . . . . 1 . . . . . . . . . . . . . . 1 . .
#> [15,] . 1 . . . . . . . . . . . . . . . . 1 . . . . . . . . . . . . 1 . . .
#> [16,] . . . 1 . . . . . . . . . . . . . . . . . . 1 . . . . . . . . . . 1 .
#> [17,] 1 . . . 1 . . . . . . . . . . . . . . . 1 . . . . . . . . . . . . . 1
#> [18,] . . . 1 . . . . . . . . . . . . . . . . . 1 . . . . . . . . . . . 1 .
#> 
#> $crossTable
#>       year    age      geo       
#>  [1,] "Total" "Total"  "Total"   
#>  [2,] "2014"  "Total"  "Total"   
#>  [3,] "2015"  "Total"  "Total"   
#>  [4,] "2016"  "Total"  "Total"   
#>  [5,] "Total" "Y15-29" "Total"   
#>  [6,] "Total" "Y30-64" "Total"   
#>  [7,] "Total" "Total"  "Iceland" 
#>  [8,] "Total" "Total"  "Portugal"
#>  [9,] "Total" "Total"  "Spain"   
#> [10,] "Total" "Total"  "EU"      
#> [11,] "Total" "Total"  "nonEU"   
#> [12,] "2014"  "Y15-29" "Total"   
#> [13,] "2014"  "Y30-64" "Total"   
#> [14,] "2015"  "Y15-29" "Total"   
#> [15,] "2015"  "Y30-64" "Total"   
#> [16,] "2016"  "Y15-29" "Total"   
#> [17,] "2016"  "Y30-64" "Total"   
#> [18,] "2014"  "Total"  "Iceland" 
#> [19,] "2014"  "Total"  "Portugal"
#> [20,] "2014"  "Total"  "Spain"   
#> [21,] "2015"  "Total"  "Iceland" 
#> [22,] "2015"  "Total"  "Portugal"
#> [23,] "2015"  "Total"  "Spain"   
#> [24,] "2016"  "Total"  "Iceland" 
#> [25,] "2016"  "Total"  "Portugal"
#> [26,] "2016"  "Total"  "Spain"   
#> [27,] "Total" "Y15-29" "Iceland" 
#> [28,] "Total" "Y15-29" "Portugal"
#> [29,] "Total" "Y15-29" "Spain"   
#> [30,] "Total" "Y30-64" "Iceland" 
#> [31,] "Total" "Y30-64" "Portugal"
#> [32,] "Total" "Y30-64" "Spain"   
#> [33,] "2014"  "Total"  "EU"      
#> [34,] "2014"  "Total"  "nonEU"   
#> [35,] "2015"  "Total"  "EU"      
#> [36,] "2015"  "Total"  "nonEU"   
#> [37,] "2016"  "Total"  "EU"      
#> [38,] "2016"  "Total"  "nonEU"   
#> [39,] "Total" "Y15-29" "EU"      
#> [40,] "Total" "Y15-29" "nonEU"   
#> [41,] "Total" "Y30-64" "EU"      
#> [42,] "Total" "Y30-64" "nonEU"   
#> [43,] "2014"  "Y15-29" "Iceland" 
#> [44,] "2014"  "Y15-29" "Portugal"
#> [45,] "2014"  "Y15-29" "Spain"   
#> [46,] "2014"  "Y30-64" "Iceland" 
#> [47,] "2014"  "Y30-64" "Portugal"
#> [48,] "2014"  "Y30-64" "Spain"   
#> [49,] "2015"  "Y15-29" "Iceland" 
#> [50,] "2015"  "Y15-29" "Portugal"
#> [51,] "2015"  "Y15-29" "Spain"   
#> [52,] "2015"  "Y30-64" "Iceland" 
#> [53,] "2015"  "Y30-64" "Portugal"
#> [54,] "2015"  "Y30-64" "Spain"   
#> [55,] "2016"  "Y15-29" "Iceland" 
#> [56,] "2016"  "Y15-29" "Portugal"
#> [57,] "2016"  "Y15-29" "Spain"   
#> [58,] "2016"  "Y30-64" "Iceland" 
#> [59,] "2016"  "Y30-64" "Portugal"
#> [60,] "2016"  "Y30-64" "Spain"   
#> [61,] "2014"  "Y15-29" "EU"      
#> [62,] "2014"  "Y15-29" "nonEU"   
#> [63,] "2014"  "Y30-64" "EU"      
#> [64,] "2014"  "Y30-64" "nonEU"   
#> [65,] "2015"  "Y15-29" "EU"      
#> [66,] "2015"  "Y15-29" "nonEU"   
#> [67,] "2015"  "Y30-64" "EU"      
#> [68,] "2015"  "Y30-64" "nonEU"   
#> [69,] "2016"  "Y15-29" "EU"      
#> [70,] "2016"  "Y15-29" "nonEU"   
#> [71,] "2016"  "Y30-64" "EU"      
#> [72,] "2016"  "Y30-64" "nonEU"   
#> 
#> $allSums
#>                       ths_per
#> Total-Total-Total       680.8
#> 2014-Total-Total        222.3
#> 2015-Total-Total        225.0
#> 2016-Total-Total        233.5
#> Total-Y15-29-Total      243.5
#> Total-Y30-64-Total      437.3
#> Total-Total-Iceland      10.6
#> Total-Total-Portugal    108.8
#> Total-Total-Spain       561.4
#> Total-Total-EU          670.2
#> Total-Total-nonEU        10.6
#> 2014-Y15-29-Total        80.3
#> 2014-Y30-64-Total       142.0
#> 2015-Y15-29-Total        79.5
#> 2015-Y30-64-Total       145.5
#> 2016-Y15-29-Total        83.7
#> 2016-Y30-64-Total       149.8
#> 2014-Total-Iceland        3.3
#> 2014-Total-Portugal      31.8
#> 2014-Total-Spain        187.2
#> 2015-Total-Iceland        3.5
#> 2015-Total-Portugal      38.5
#> 2015-Total-Spain        183.0
#> 2016-Total-Iceland        3.8
#> 2016-Total-Portugal      38.5
#> 2016-Total-Spain        191.2
#> Total-Y15-29-Iceland      5.6
#> Total-Y15-29-Portugal    38.5
#> Total-Y15-29-Spain      199.4
#> Total-Y30-64-Iceland      5.0
#> Total-Y30-64-Portugal    70.3
#> Total-Y30-64-Spain      362.0
#> 2014-Total-EU           219.0
#> 2014-Total-nonEU          3.3
#> 2015-Total-EU           221.5
#> 2015-Total-nonEU          3.5
#> 2016-Total-EU           229.7
#> 2016-Total-nonEU          3.8
#> Total-Y15-29-EU         237.9
#> Total-Y15-29-nonEU        5.6
#> Total-Y30-64-EU         432.3
#> Total-Y30-64-nonEU        5.0
#> 2014-Y15-29-Iceland       1.8
#> 2014-Y15-29-Portugal     11.6
#> 2014-Y15-29-Spain        66.9
#> 2014-Y30-64-Iceland       1.5
#> 2014-Y30-64-Portugal     20.2
#> 2014-Y30-64-Spain       120.3
#> 2015-Y15-29-Iceland       1.9
#> 2015-Y15-29-Portugal     14.2
#> 2015-Y15-29-Spain        63.4
#> 2015-Y30-64-Iceland       1.6
#> 2015-Y30-64-Portugal     24.3
#> 2015-Y30-64-Spain       119.6
#> 2016-Y15-29-Iceland       1.9
#> 2016-Y15-29-Portugal     12.7
#> 2016-Y15-29-Spain        69.1
#> 2016-Y30-64-Iceland       1.9
#> 2016-Y30-64-Portugal     25.8
#> 2016-Y30-64-Spain       122.1
#> 2014-Y15-29-EU           78.5
#> 2014-Y15-29-nonEU         1.8
#> 2014-Y30-64-EU          140.5
#> 2014-Y30-64-nonEU         1.5
#> 2015-Y15-29-EU           77.6
#> 2015-Y15-29-nonEU         1.9
#> 2015-Y30-64-EU          143.9
#> 2015-Y30-64-nonEU         1.6
#> 2016-Y15-29-EU           81.8
#> 2016-Y15-29-nonEU         1.9
#> 2016-Y30-64-EU          147.9
#> 2016-Y30-64-nonEU         1.9
#> 
FormulaSums(x, ths_per ~ year:age:geo -1)
#>                      ths_per
#> 2014-Y15-29-Iceland      1.8
#> 2014-Y15-29-Portugal    11.6
#> 2014-Y15-29-Spain       66.9
#> 2014-Y30-64-Iceland      1.5
#> 2014-Y30-64-Portugal    20.2
#> 2014-Y30-64-Spain      120.3
#> 2015-Y15-29-Iceland      1.9
#> 2015-Y15-29-Portugal    14.2
#> 2015-Y15-29-Spain       63.4
#> 2015-Y30-64-Iceland      1.6
#> 2015-Y30-64-Portugal    24.3
#> 2015-Y30-64-Spain      119.6
#> 2016-Y15-29-Iceland      1.9
#> 2016-Y15-29-Portugal    12.7
#> 2016-Y15-29-Spain       69.1
#> 2016-Y30-64-Iceland      1.9
#> 2016-Y30-64-Portugal    25.8
#> 2016-Y30-64-Spain      122.1
m <- Formula2ModelMatrix(x, ~ year*geo + year*eu)
print(m[1:3, ], col.names = TRUE)
#> <S4 Type Object>
#> attr(,"i")
#>  [1] 0 1 2 0 1 2 1 2 0 0 2 1 1 2 0 0 2 1
#> attr(,"p")
#>  [1]  0  3  6  6  6  7  8  9 11 12 13 14 15 15 15 15 15 15 15 17 18 18 18 18 18
#> attr(,"Dim")
#> [1]  3 24
#> attr(,"Dimnames")
#> attr(,"Dimnames")[[1]]
#> NULL
#> 
#> attr(,"Dimnames")[[2]]
#>  [1] "Total-Total"    "2014-Total"     "2015-Total"     "2016-Total"    
#>  [5] "Total-Iceland"  "Total-Portugal" "Total-Spain"    "Total-EU"      
#>  [9] "Total-nonEU"    "2014-Iceland"   "2014-Portugal"  "2014-Spain"    
#> [13] "2015-Iceland"   "2015-Portugal"  "2015-Spain"     "2016-Iceland"  
#> [17] "2016-Portugal"  "2016-Spain"     "2014-EU"        "2014-nonEU"    
#> [21] "2015-EU"        "2015-nonEU"     "2016-EU"        "2016-nonEU"    
#> 
#> attr(,"x")
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> attr(,"factors")
#> list()
#> attr(,"class")
#> [1] "dgCMatrix"
#> attr(,"class")attr(,"package")
#> [1] "Matrix"
attr(m, "startCol")
#> (Intercept)        year         geo          eu    year:geo     year:eu 
#>           1           2           5           8          10          19 
```
