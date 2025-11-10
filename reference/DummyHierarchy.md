# Converting hierarchy specifications to a (signed) dummy matrix

A matrix for mapping input codes (columns) to output codes (rows) are
created. The elements of the matrix specify how columns contribute to
rows.

## Usage

``` r
DummyHierarchy(
  mapsFrom,
  mapsTo,
  sign,
  level,
  mapsInput = NULL,
  inputInOutput = FALSE,
  keepCodes = mapsFrom[integer(0)],
  unionComplement = FALSE,
  reOrder = FALSE
)

DummyHierarchies(
  hierarchies,
  data = NULL,
  inputInOutput = FALSE,
  unionComplement = FALSE,
  reOrder = FALSE
)
```

## Arguments

- mapsFrom:

  Character vector from hierarchy table

- mapsTo:

  Character vector from hierarchy table

- sign:

  Numeric vector of either 1 or -1 from hierarchy table

- level:

  Numeric vector from hierarchy table

- mapsInput:

  All codes in mapsFrom not in mapsTo (created automatically when NULL)
  and possibly other codes in input data.

- inputInOutput:

  When FALSE all output rows represent codes in mapsTo

- keepCodes:

  To prevent some codes to be removed when inputInOutput = FALSE

- unionComplement:

  When TRUE, sign means union and complement instead of addition or
  subtraction (see note)

- reOrder:

  When TRUE (FALSE is default) output codes are ordered differently,
  more similar to a usual model matrix ordering.

- hierarchies:

  List of hierarchies

- data:

  data

## Value

A sparse matrix with row and column and names

## Details

`DummyHierarchies` is a user-friendly wrapper for the original function
`DummyHierarchy`. Then, the logical input parameters are vectors
(possibly recycled). `mapsInput` and `keepCodes` can be supplied as
attributes. `mapsInput` will be generated when `data` is non-NULL.

## Note

With unionComplement = FALSE (default), the sign of each mapping
specifies the contribution as addition or subtraction. Thus, values
above one and negative values in output can occur. With unionComplement
= TRUE, positive is treated as union and negative as complement. Then 0
and 1 are the only possible elements in the output matrix.

## Author

Øyvind Langsrud

## Examples

``` r
# A hierarchy table
h <- SSBtoolsData("FIFA2018ABCD")

DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level)
#> 9 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names ‘Argentina’, ‘Australia’, ‘Croatia’ ... ]]
#>                                            
#> Africa      . . . . 1 . . . 1 1 . . . . . .
#> America     1 . . . . . . . . . 1 . . . . 1
#> Asia        . . . . . . . 1 . . . . . 1 . .
#> Europe      . . 1 1 . 1 1 . . . . 1 1 . 1 .
#> Oceania     . 1 . . . . . . . . . . . . . .
#> nonEU       . . . . . . 1 . . . . . 1 . . .
#> nonSchengen . . 1 . . . . . . . . . 1 . . .
#> EU          . . 1 1 . 1 0 . . . . 1 0 . 1 .
#> Schengen    . . 0 1 . 1 1 . . . . 1 0 . 1 .
DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level, inputInOutput = TRUE)
#> 25 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names ‘Argentina’, ‘Australia’, ‘Croatia’ ... ]]
#>                                             
#> Argentina    1 . . . . . . . . . . . . . . .
#> Australia    . 1 . . . . . . . . . . . . . .
#> Croatia      . . 1 . . . . . . . . . . . . .
#> Denmark      . . . 1 . . . . . . . . . . . .
#> Egypt        . . . . 1 . . . . . . . . . . .
#> France       . . . . . 1 . . . . . . . . . .
#> Iceland      . . . . . . 1 . . . . . . . . .
#> Iran         . . . . . . . 1 . . . . . . . .
#> Morocco      . . . . . . . . 1 . . . . . . .
#> Nigeria      . . . . . . . . . 1 . . . . . .
#> Peru         . . . . . . . . . . 1 . . . . .
#> Portugal     . . . . . . . . . . . 1 . . . .
#> Russia       . . . . . . . . . . . . 1 . . .
#> Saudi Arabia . . . . . . . . . . . . . 1 . .
#> Spain        . . . . . . . . . . . . . . 1 .
#> Uruguay      . . . . . . . . . . . . . . . 1
#> Africa       . . . . 1 . . . 1 1 . . . . . .
#> America      1 . . . . . . . . . 1 . . . . 1
#> Asia         . . . . . . . 1 . . . . . 1 . .
#> Europe       . . 1 1 . 1 1 . . . . 1 1 . 1 .
#> Oceania      . 1 . . . . . . . . . . . . . .
#> nonEU        . . . . . . 1 . . . . . 1 . . .
#> nonSchengen  . . 1 . . . . . . . . . 1 . . .
#> EU           . . 1 1 . 1 0 . . . . 1 0 . 1 .
#> Schengen     . . 0 1 . 1 1 . . . . 1 0 . 1 .
DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level, keepCodes = c("Portugal", "Spain"))
#> 11 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names ‘Argentina’, ‘Australia’, ‘Croatia’ ... ]]
#>                                            
#> Portugal    . . . . . . . . . . . 1 . . . .
#> Spain       . . . . . . . . . . . . . . 1 .
#> Africa      . . . . 1 . . . 1 1 . . . . . .
#> America     1 . . . . . . . . . 1 . . . . 1
#> Asia        . . . . . . . 1 . . . . . 1 . .
#> Europe      . . 1 1 . 1 1 . . . . 1 1 . 1 .
#> Oceania     . 1 . . . . . . . . . . . . . .
#> nonEU       . . . . . . 1 . . . . . 1 . . .
#> nonSchengen . . 1 . . . . . . . . . 1 . . .
#> EU          . . 1 1 . 1 0 . . . . 1 0 . 1 .
#> Schengen    . . 0 1 . 1 1 . . . . 1 0 . 1 .

# Extend the hierarchy table to illustrate the effect of unionComplement
h2 <- rbind(data.frame(mapsFrom = c("EU", "Schengen"), mapsTo = "EUandSchengen", 
                       sign = 1, level = 3), h)

DummyHierarchy(h2$mapsFrom, h2$mapsTo, h2$sign, h2$level)
#> 10 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names ‘Argentina’, ‘Australia’, ‘Croatia’ ... ]]
#>                                              
#> Africa        . . . . 1 . . . 1 1 . . . . . .
#> America       1 . . . . . . . . . 1 . . . . 1
#> Asia          . . . . . . . 1 . . . . . 1 . .
#> Europe        . . 1 1 . 1 1 . . . . 1 1 . 1 .
#> Oceania       . 1 . . . . . . . . . . . . . .
#> nonEU         . . . . . . 1 . . . . . 1 . . .
#> nonSchengen   . . 1 . . . . . . . . . 1 . . .
#> EU            . . 1 1 . 1 0 . . . . 1 0 . 1 .
#> Schengen      . . 0 1 . 1 1 . . . . 1 0 . 1 .
#> EUandSchengen . . 1 2 . 2 1 . . . . 2 0 . 2 .
DummyHierarchy(h2$mapsFrom, h2$mapsTo, h2$sign, h2$level, unionComplement = TRUE)
#> 10 x 16 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 16 column names ‘Argentina’, ‘Australia’, ‘Croatia’ ... ]]
#>                                              
#> Africa        . . . . 1 . . . 1 1 . . . . . .
#> America       1 . . . . . . . . . 1 . . . . 1
#> Asia          . . . . . . . 1 . . . . . 1 . .
#> Europe        . . 1 1 . 1 1 . . . . 1 1 . 1 .
#> Oceania       . 1 . . . . . . . . . . . . . .
#> nonEU         . . . . . . 1 . . . . . 1 . . .
#> nonSchengen   . . 1 . . . . . . . . . 1 . . .
#> EU            . . 1 1 . 1 0 . . . . 1 0 . 1 .
#> Schengen      . . 0 1 . 1 1 . . . . 1 0 . 1 .
#> EUandSchengen . . 1 1 . 1 1 . . . . 1 0 . 1 .

# Extend mapsInput - leading to zero columns.
DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level,
               mapsInput = c(h$mapsFrom[!(h$mapsFrom %in% h$mapsTo)], "Norway", "Finland"))
#> 9 x 18 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 18 column names ‘Argentina’, ‘Australia’, ‘Croatia’ ... ]]
#>                                                
#> Africa      . . . . 1 . . . . 1 1 . . . . . . .
#> America     1 . . . . . . . . . . . 1 . . . . 1
#> Asia        . . . . . . . . 1 . . . . . . 1 . .
#> Europe      . . 1 1 . . 1 1 . . . . . 1 1 . 1 .
#> Oceania     . 1 . . . . . . . . . . . . . . . .
#> nonEU       . . . . . . . 1 . . . . . . 1 . . .
#> nonSchengen . . 1 . . . . . . . . . . . 1 . . .
#> EU          . . 1 1 . . 1 0 . . . . . 1 0 . 1 .
#> Schengen    . . 0 1 . . 1 1 . . . . . 1 0 . 1 .

# DummyHierarchies
DummyHierarchies(FindHierarchies(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")]), 
                 inputInOutput = c(FALSE, TRUE))
#> $geo
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>       Iceland Portugal Spain
#> EU          .        1     1
#> nonEU       1        .     .
#> Total       1        1     1
#> 
#> $age
#> 3 x 2 sparse Matrix of class "dgCMatrix"
#>        Y15-29 Y30-64
#> Y15-29      1      .
#> Y30-64      .      1
#> Total       1      1
#> 
```
