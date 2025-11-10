# varGroups-attribute to Extend0, Example functions

Setting `attr(varGroups, "FunctionExtend0")` to a function makes
`Extend0` behave differently

## Usage

``` r
Extend0rnd1(data, varGroups, k = 1, rndSeed = 123)

Extend0rnd2(...)

Extend0rnd1b(...)
```

## Arguments

- data:

  data.frame within
  [`Extend0`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)

- varGroups:

  argument to
  [`Extend0`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)

- k:

  Number of rows generated is approx. `k*nrow(data)`

- rndSeed:

  Internal random seed to be used

- ...:

  Extra unused parameters

## Value

a data frame

## Details

The point is to create a function that takes `data` and `varGroups` as
input and that returns a data frame with a limited number of
combinations of the elements in `varGroups`. The example function here
is limited to two varGroups elements.

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")[c(1, 5, 8, 14), ]
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"

varGroups <- list(c("year", "geo", "eu"), data.frame(age = c("middle", "old")))
Extend0(z, varGroups = varGroups)
#>       age     geo year ths_per    eu freq
#> 1   young   Spain 2014    66.9    EU    1
#> 2     old Iceland 2014     1.5 nonEU    1
#> 3   young Iceland 2015     1.9 nonEU    1
#> 4   young Iceland 2016     1.9 nonEU    1
#> 5  middle   Spain 2014     0.0    EU    0
#> 6  middle Iceland 2014     0.0 nonEU    0
#> 7  middle Iceland 2015     0.0 nonEU    0
#> 8  middle Iceland 2016     0.0 nonEU    0
#> 9     old   Spain 2014     0.0    EU    0
#> 10    old Iceland 2015     0.0 nonEU    0
#> 11    old Iceland 2016     0.0 nonEU    0

attr(varGroups, "FunctionExtend0") <- Extend0rnd1
Extend0(z, varGroups = varGroups)
#>      age     geo year ths_per    eu freq
#> 1  young   Spain 2014    66.9    EU    1
#> 2    old Iceland 2014     1.5 nonEU    1
#> 3  young Iceland 2015     1.9 nonEU    1
#> 4  young Iceland 2016     1.9 nonEU    1
#> 5 middle   Spain 2014     0.0    EU    0
#> 6 middle Iceland 2015     0.0 nonEU    0
#> 7    old Iceland 2016     0.0 nonEU    0

attr(varGroups, "FunctionExtend0") <- Extend0rnd1b
Extend0(z, varGroups = varGroups)
#>      age     geo year ths_per    eu freq
#> 1  young   Spain 2014    66.9    EU    1
#> 2    old Iceland 2014     1.5 nonEU    1
#> 3  young Iceland 2015     1.9 nonEU    1
#> 4  young Iceland 2016     1.9 nonEU    1
#> 5 middle   Spain 2014     0.0    EU    0
#> 6 middle Iceland 2014     0.0 nonEU    0
#> 7    old Iceland 2015     0.0 nonEU    0
#> 8    old Iceland 2016     0.0 nonEU    0

attr(varGroups, "FunctionExtend0") <- Extend0rnd2
Extend0(z, varGroups = varGroups)
#>      age     geo year ths_per    eu freq
#> 1  young   Spain 2014    66.9    EU    1
#> 2    old Iceland 2014     1.5 nonEU    1
#> 3  young Iceland 2015     1.9 nonEU    1
#> 4  young Iceland 2016     1.9 nonEU    1
#> 5 middle   Spain 2014     0.0    EU    0
#> 6    old   Spain 2014     0.0    EU    0
#> 7 middle Iceland 2015     0.0 nonEU    0
#> 8 middle Iceland 2016     0.0 nonEU    0
#> 9    old Iceland 2016     0.0 nonEU    0

# To see what's going on internally. Data used only via nrow 
varGroups <- list(data.frame(ab = rep(c("a", "b"), each = 4), abcd = c("a", "b", "c", "d")), 
                  data.frame(AB = rep(c("A", "B"), each = 3), ABC = c("A", "B", "C"))) 
a <- Extend0rnd1(data.frame(1:5), varGroups)
table(a[[1]], a[[2]])
#>    
#>     a b c d
#>   a 1 1 1 1
#>   b 1 1 1 1
table(a[[3]], a[[4]])
#>    
#>     A B C
#>   A 1 1 2
#>   B 1 1 2
a <- Extend0rnd1b(data.frame(1:5), varGroups)
table(a[[1]], a[[2]])
#>    
#>     a b c d
#>   a 1 1 1 1
#>   b 1 1 1 1
table(a[[3]], a[[4]])
#>    
#>     A B C
#>   A 2 1 1
#>   B 2 1 1
a <- Extend0rnd2(data.frame(1:5), varGroups[2:1])
table(a[[1]], a[[2]])
#>    
#>     A B C
#>   A 2 2 2
#>   B 2 2 2
table(a[[3]], a[[4]])
#>    
#>     a b c d
#>   a 1 1 2 1
#>   b 1 2 2 2
a <- Extend0rnd1(data.frame(1:100), varGroups)
table(a[[1]], a[[2]]) # Maybe smaller numbers than expected since duplicates were removed
#>    
#>     a b c d
#>   a 5 6 5 6
#>   b 6 6 5 6
table(a[[3]], a[[4]])
#>    
#>     A B C
#>   A 8 7 7
#>   B 8 7 8
```
