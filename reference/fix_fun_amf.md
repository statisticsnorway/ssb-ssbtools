# Fix `fun` parameter to `aggregate_multiple_fun`

Fix `fun` parameter to `aggregate_multiple_fun`

## Usage

``` r
fix_fun_amf(fun)
```

## Arguments

- fun:

  fun

## Value

fun

## Examples

``` r
identical(fix_fun_amf("median"), c(median = median))
#> [1] TRUE

identical(fix_fun_amf(c("sum", "median")), c(sum = sum, median = median))
#> [1] TRUE

ff <- c("sum", "median", "cor")
names(ff) <- c("", NA, "Correlation")
identical(fix_fun_amf(ff), c(sum, median = median, Correlation = cor))
#> [1] TRUE

identical(fix_fun_amf(structure("median", names = "")), fix_fun_amf(median))
#> [1] TRUE
```
