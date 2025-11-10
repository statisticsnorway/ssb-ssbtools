# Table all integers from 1 to n

Table all integers from 1 to n

## Usage

``` r
table_all_integers(x, n)
```

## Arguments

- x:

  A vector of integers.

- n:

  The maximum integer value.

## Value

A 1D array of class `"table"` representing the frequency of each integer
from 1 to n.

## Examples

``` r
table_all_integers(c(2, 3, 5, 3, 5, 3), 7)
#> 
#> 1 2 3 4 5 6 7 
#> 0 1 3 0 2 0 0 
```
