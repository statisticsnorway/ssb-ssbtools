# Round values that are close two whole numbers

Round values that are close two whole numbers

## Usage

``` r
RoundWhole(x, digits = 9, onlyZeros = FALSE)
```

## Arguments

- x:

  vector or matrix

- digits:

  parameter to [`round`](https://rdrr.io/r/base/Round.html)

- onlyZeros:

  Only round values close to zero

## Value

Modified x

## Details

When `digits` is `NA`, `Inf` or `NULL`, input is returned unmodified.
When there is more than one element in `digits` or `onlyZeros`, rounding
is performed column-wise.

## Author

Øyvind Langsrud

## Examples

``` r
x <- c(0.0002, 1.00003, 3.00014)
RoundWhole(x)     # No values rounded
#> [1] 0.00020 1.00003 3.00014
RoundWhole(x, 4)  # One value rounded
#> [1] 0.00020 1.00000 3.00014
RoundWhole(x, 3)  # All values rounded
#> [1] 0 1 3
RoundWhole(x, NA) # No values rounded (always)
#> [1] 0.00020 1.00003 3.00014
RoundWhole(x, 3, TRUE)  # One value rounded
#> [1] 0.00000 1.00003 3.00014
RoundWhole(cbind(x, x, x), digits = c(3, 4, NA))
#>      x       x       x
#> [1,] 0 0.00020 0.00020
#> [2,] 1 1.00000 1.00003
#> [3,] 3 3.00014 3.00014
RoundWhole(cbind(x, x), digits = 3, onlyZeros = c(FALSE, TRUE))
#>      x       x
#> [1,] 0 0.00000
#> [2,] 1 1.00003
#> [3,] 3 3.00014
```
