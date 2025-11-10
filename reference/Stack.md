# Stack columns from a data frame and include variables.

Stack columns from a data frame and include variables.

## Usage

``` r
Stack(
  data,
  stackVar = 1:NCOL(data),
  blockVar = integer(0),
  rowData = data.frame(stackVar)[, integer(0), drop = FALSE],
  valueName = "values",
  indName = "ind"
)
```

## Arguments

- data:

  A data frame

- stackVar:

  Indices of variables to be stacked

- blockVar:

  Indices of variables to be replicated

- rowData:

  A separate data frame where NROW(rowData)=length(stackVar) such that
  each row may contain multiple information of each stackVar variable.
  The output data frame will contain an extended variant of rowData.

- valueName:

  Name of the stacked/concatenated output variable

- indName:

  Name of the output variable with information of which vector in x the
  observation originated. When indName is NULL this variable is not
  included in output.

## Value

A data frame where the variable ordering corresponds to: blockVar,
rowData, valueName, indName

## See also

[`Unstack`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Unstack.md)

## Author

Øyvind Langsrud

## Examples

``` r
 z <- data.frame(n=c(10,20,30), ssb=c('S','S','B'),
 Ayes=1:3,Ano=4:6,Byes=7:9,Bno=10:12)
 zRow <- data.frame(letter=c('A','A','B','B'),answer=c('yes','no','yes','no') )
 
 x <- Stack(z,3:6,1:2,zRow)
 
 Unstack(x,6,3:4,numeric(0),1:2)
#> $data
#>    n ssb A_yes A_no B_yes B_no
#> 1 10   S     1    4     7   10
#> 2 20   S     2    5     8   11
#> 3 30   B     3    6     9   12
#> 
#> $rowData
#>       letter answer
#> A_yes      A    yes
#> A_no       A     no
#> B_yes      B    yes
#> B_no       B     no
#> 
 Unstack(x,6,5,numeric(0),1:2)
#> $data
#>    n ssb Ayes Ano Byes Bno
#> 1 10   S    1   4    7  10
#> 2 20   S    2   5    8  11
#> 3 30   B    3   6    9  12
#> 
#> $rowData
#>       ind
#> Ayes Ayes
#> Ano   Ano
#> Byes Byes
#> Bno   Bno
#> 
 Unstack(x,6,3:4,5,1:2)
#> $data
#>    n ssb A_yes A_no B_yes B_no
#> 1 10   S     1    4     7   10
#> 2 20   S     2    5     8   11
#> 3 30   B     3    6     9   12
#> 
#> $rowData
#>       letter answer  ind
#> A_yes      A    yes Ayes
#> A_no       A     no  Ano
#> B_yes      B    yes Byes
#> B_no       B     no  Bno
#> 
```
