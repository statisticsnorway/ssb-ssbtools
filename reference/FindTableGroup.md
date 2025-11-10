# Finding table(s) of hierarchical variable groups

A single table or two linked tables are found

## Usage

``` r
FindTableGroup(
  x = NULL,
  findLinked = FALSE,
  mainName = TRUE,
  fCorr = FactorLevCorr(x),
  CheckHandling = warning
)
```

## Arguments

- x:

  Matrix or data frame containing the variables

- findLinked:

  When TRUE, two linked tables can be in output

- mainName:

  When TRUE the groupVarInd ouput is named according to first variable
  in group.

- fCorr:

  When non-null x is not needed as input.

- CheckHandling:

  Function (warning or stop) to be used in problematic situations.

## Value

Output is a list with items

- groupVarInd:

  List defining the hierarchical variable groups. First variable has
  most levels.

- table:

  List containing one or two tables. These tables are coded as indices
  referring to elements of groupVarInd.

## Author

Øyvind Langsrud

## Examples

``` r
 x <- rep(c('A','B','C'),3)
 y <- rep(c(11,22,11),3)
 z <- c(1,1,1,2,2,2,3,3,3)
 zy <- paste(z,y,sep='')
 m <- cbind(x,y,z,zy)
 FindTableGroup(m)
#> Warning: Not a single unique table
#> $groupVarInd
#> $groupVarInd$x
#> [1] 1 2
#> 
#> $groupVarInd$zy
#> [1] 4 2
#> 
#> $groupVarInd$zy
#> [1] 4 3
#> 
#> 
#> $table
#> $table$ind1
#> [1] 1 3
#> 
#> 
 FindTableGroup(m,findLinked=TRUE)
#> $groupVarInd
#> $groupVarInd$x
#> [1] 1 2
#> 
#> $groupVarInd$zy
#> [1] 4 2
#> 
#> $groupVarInd$zy
#> [1] 4 3
#> 
#> 
#> $table
#> $table$ind1
#> [1] 1 3
#> 
#> $table$ind2
#> [1] 2
#> 
#> 
```
