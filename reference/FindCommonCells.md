# Finding commonCells

Finding lists defining common cells as needed for the input parameter
commonCells to the function protectLinkedTables in package sdcTable. The
function handles two tables based on the same main variables but
possibly different aggregating variables.

## Usage

``` r
FindCommonCells(dimList1, dimList2)
```

## Arguments

- dimList1:

  As input parameter dimList to the function makeProblem in package
  sdcTable.

- dimList2:

  Another dimList with the same names and using the same level names.

## Value

Output is a list according to the specifications in sdcTable.

## Author

Øyvind Langsrud

## Examples

``` r
 x <- rep(c('A','B','C'),3)
 y <- rep(c(11,22,11),3)
 z <- c(1,1,1,2,2,2,3,3,3)
 zy <- paste(z,y,sep='')
 m <- cbind(x,y,z,zy)
 fg <- FindTableGroup(m,findLinked=TRUE)
 dimLists <- FindDimLists(m,fg$groupVarInd)
 # Using table1 and table2 in this example cause error,
 # but in other cases this may work well
 try(FindCommonCells(dimLists[fg$table$table1],dimLists[fg$table$table2]))
#> named list()
 FindCommonCells(dimLists[c(1,2)],dimLists[c(1,3)])
#> $x
#> $x[[1]]
#> [1] "x"
#> 
#> $x[[2]]
#> [1] "x"
#> 
#> $x[[3]]
#> [1] "All"
#> 
#> 
#> $zy
#> $zy[[1]]
#> [1] "zy"
#> 
#> $zy[[2]]
#> [1] "zy"
#> 
#> $zy[[3]]
#> [1] "Total" "111"   "211"   "311"   "122"   "222"   "322"  
#> 
#> $zy[[4]]
#> [1] "Total" "111"   "211"   "311"   "122"   "222"   "322"  
#> 
#> 
```
