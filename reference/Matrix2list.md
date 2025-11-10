# Convert matrix to sparse list

Convert matrix to sparse list

## Usage

``` r
Matrix2list(x)

Matrix2listInt(x)
```

## Arguments

- x:

  Input matrix

## Value

A two-element list: List of row numbers (r) and a list of numeric or
integer values (x)

## Details

Within the function, the input matrix is first converted to a dgTMatrix
matrix (Matrix package).

## Note

`Matrix2listInt` convers the values to integers by `as.integer` and no
checking is performed. Thus, zeros are possible.

## Author

Øyvind Langsrud

## Examples

``` r
m = matrix(c(0.5, 1.1, 3.14, 0, 0, 0, 0, 4, 5), 3, 3)
Matrix2list(m)
#> $r
#> $r[[1]]
#> [1] 1 2 3
#> 
#> $r[[2]]
#> integer(0)
#> 
#> $r[[3]]
#> [1] 2 3
#> 
#> 
#> $x
#> $x[[1]]
#> [1] 0.50 1.10 3.14
#> 
#> $x[[2]]
#> numeric(0)
#> 
#> $x[[3]]
#> [1] 4 5
#> 
#> 
Matrix2listInt(m)
#> $r
#> $r[[1]]
#> [1] 1 2 3
#> 
#> $r[[2]]
#> integer(0)
#> 
#> $r[[3]]
#> [1] 2 3
#> 
#> 
#> $x
#> $x[[1]]
#> [1] 0 1 3
#> 
#> $x[[2]]
#> integer(0)
#> 
#> $x[[3]]
#> [1] 4 5
#> 
#> 
```
