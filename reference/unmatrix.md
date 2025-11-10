# Transform data frame with embedded matrices

Transform data frame with embedded matrices

## Usage

``` r
unmatrix(data, sep = "_")
```

## Arguments

- data:

  data frame

- sep:

  A character string used when variable names are generated.

## Value

data frame

## Examples

``` r
a <- aggregate(1:6, list(rep(1:3, 2)), range)
b <- unmatrix(a)

a
#>   Group.1 x.1 x.2
#> 1       1   1   4
#> 2       2   2   5
#> 3       3   3   6
b
#>   Group.1 x_1 x_2
#> 1       1   1   4
#> 2       2   2   5
#> 3       3   3   6

dim(a)
#> [1] 3 2
dim(b)
#> [1] 3 3

names(a)
#> [1] "Group.1" "x"      
names(b)
#> [1] "Group.1" "x_1"     "x_2"    

class(a[, 2])
#> [1] "matrix" "array" 
class(b[, 2])
#> [1] "integer"
```
