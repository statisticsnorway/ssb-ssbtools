# Default progress indicator function

Default progress indicator function

## Usage

``` r
inc_default(i = 0L, n = 0L, steps = 25L, dot = ".")
```

## Arguments

- i:

  i in "i out of n"

- n:

  n in "i out of n"

- steps:

  Number of dots to print

- dot:

  dot

## Examples

``` r
for (i in 1:5) inc_default(i, 5)
#> .....
cat("\n")
#> 
for (i in 1:100) inc_default(i, 100)
#> .........................
cat("\n")
#> 
for (i in 1:1000) inc_default(i, 1000)
#> .........................
cat("\n")
#> 
for (i in 1:1000) inc_default(i, 1000, steps = 10)
#> ..........
cat("\n")
#> 
for (i in 1:10) inc_default()
#> ..........
cat("\n")
#> 
```
