# Finding hierarchical variable groups

According to the (factor) levels of the variables

## Usage

``` r
HierarchicalGroups(
  x = NULL,
  mainName = TRUE,
  eachName = FALSE,
  fCorr = FactorLevCorr(x)
)
```

## Arguments

- x:

  Matrix or data frame containing the variables

- mainName:

  When TRUE output list is named according to first variable in group.

- eachName:

  When TRUE variable names in output instead of indices.

- fCorr:

  When non-null, x is not needed as input.

## Value

Output is a list containing the groups. First variable has most levels.

## Author

Øyvind Langsrud

## Examples

``` r
dataset <- SSBtoolsData("example1")
HierarchicalGroups(dataset[1:2], eachName = TRUE)
#> $age
#> [1] "age"
#> 
#> $geo
#> [1] "geo"
#> 
HierarchicalGroups(dataset[2:3])
#> $geo
#> [1] 1 2
#> 
HierarchicalGroups(dataset[1:4], eachName = TRUE)
#> $age
#> [1] "age"
#> 
#> $geo
#> [1] "geo" "eu" 
#> 
#> $year
#> [1] "year"
#> 

HierarchicalGroups(SSBtoolsData("magnitude1")[1:4])
#> $sector4
#> [1] 1 2
#> 
#> $geo
#> [1] 3 4
#> 

 x <- rep(c("A","B","C"),3)
 y <- rep(c(11,22,11),3)
 z <- c(1,1,1,2,2,2,3,3,3)
 zy <- paste(z,y,sep="")
 m <- cbind(x,y,z,zy)
 HierarchicalGroups(m)
#> $x
#> [1] 1 2
#> 
#> $zy
#> [1] 4 2
#> 
#> $zy
#> [1] 4 3
#> 
```
