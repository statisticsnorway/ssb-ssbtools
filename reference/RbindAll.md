# Combining several data frames when the columns don't match

Combining several data frames when the columns don't match

## Usage

``` r
RbindAll(...)
```

## Arguments

- ...:

  Several data frames as several input parameters or a list of data
  frames

## Value

A single data frame

## Note

The function is an extended version of rbind.all.columns at
<https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/>

## See also

[`CbindIdMatch`](https://statisticsnorway.github.io/ssb-ssbtools/reference/CbindIdMatch.md)
(same example data)

## Author

Øyvind Langsrud

## Examples

``` r
zA <- data.frame(idA = 1:10, idB = rep(10 * (1:5), 2), idC = rep(c(100, 200), 5), 
                 idC2 = c(100, rep(200, 9)), idC3 = rep(100, 10), 
                 idD = 99, x = round(rnorm(10), 3), xA = round(runif(10), 2))
zB <- data.frame(idB = 10 * (1:5), x = round(rnorm(5), 3), xB = round(runif(5), 2))
zC <- data.frame(idC = c(100, 200), x = round(rnorm(2), 3), xC = round(runif(2), 2))
zD <- data.frame(idD = 99, x = round(rnorm(1), 3), xD = round(runif(1), 2))
RbindAll(zA, zB, zC, zD)
#>    idA idB idC idC2 idC3 idD      x   xA   xB   xC   xD
#> 1    1  10 100  100  100  99  0.817 0.72   NA   NA   NA
#> 2    2  20 200  200  100  99 -0.048 0.18   NA   NA   NA
#> 3    3  30 100  200  100  99  1.340 0.90   NA   NA   NA
#> 4    4  40 200  200  100  99  0.306 0.43   NA   NA   NA
#> 5    5  50 100  200  100  99 -0.634 0.45   NA   NA   NA
#> 6    6  10 200  200  100  99  0.279 0.56   NA   NA   NA
#> 7    7  20 100  200  100  99  1.189 0.98   NA   NA   NA
#> 8    8  30 200  200  100  99  0.775 0.55   NA   NA   NA
#> 9    9  40 100  200  100  99  0.363 0.32   NA   NA   NA
#> 10  10  50 200  200  100  99 -0.036 0.00   NA   NA   NA
#> 11  NA  10  NA   NA   NA  NA -0.505   NA 0.33   NA   NA
#> 12  NA  20  NA   NA   NA  NA  1.051   NA 0.66   NA   NA
#> 13  NA  30  NA   NA   NA  NA -0.256   NA 0.77   NA   NA
#> 14  NA  40  NA   NA   NA  NA  0.414   NA 0.51   NA   NA
#> 15  NA  50  NA   NA   NA  NA -1.684   NA 0.82   NA   NA
#> 16  NA  NA 100   NA   NA  NA  0.192   NA   NA 0.49   NA
#> 17  NA  NA 200   NA   NA  NA -0.086   NA   NA 0.38   NA
#> 18  NA  NA  NA   NA   NA  99  0.932   NA   NA   NA 0.86
RbindAll(list(zA, zB, zC, zD))
#>    idA idB idC idC2 idC3 idD      x   xA   xB   xC   xD
#> 1    1  10 100  100  100  99  0.817 0.72   NA   NA   NA
#> 2    2  20 200  200  100  99 -0.048 0.18   NA   NA   NA
#> 3    3  30 100  200  100  99  1.340 0.90   NA   NA   NA
#> 4    4  40 200  200  100  99  0.306 0.43   NA   NA   NA
#> 5    5  50 100  200  100  99 -0.634 0.45   NA   NA   NA
#> 6    6  10 200  200  100  99  0.279 0.56   NA   NA   NA
#> 7    7  20 100  200  100  99  1.189 0.98   NA   NA   NA
#> 8    8  30 200  200  100  99  0.775 0.55   NA   NA   NA
#> 9    9  40 100  200  100  99  0.363 0.32   NA   NA   NA
#> 10  10  50 200  200  100  99 -0.036 0.00   NA   NA   NA
#> 11  NA  10  NA   NA   NA  NA -0.505   NA 0.33   NA   NA
#> 12  NA  20  NA   NA   NA  NA  1.051   NA 0.66   NA   NA
#> 13  NA  30  NA   NA   NA  NA -0.256   NA 0.77   NA   NA
#> 14  NA  40  NA   NA   NA  NA  0.414   NA 0.51   NA   NA
#> 15  NA  50  NA   NA   NA  NA -1.684   NA 0.82   NA   NA
#> 16  NA  NA 100   NA   NA  NA  0.192   NA   NA 0.49   NA
#> 17  NA  NA 200   NA   NA  NA -0.086   NA   NA 0.38   NA
#> 18  NA  NA  NA   NA   NA  99  0.932   NA   NA   NA 0.86
```
