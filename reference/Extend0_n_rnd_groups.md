# Add zero-frequency rows using complete and sampled group combinations

`Extend0_with_n_rnd_groups` is a function that calls
[`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)
with `Extend0_n_rnd_groups()` as the `varGroups` attribute. The data are
extended in the usual way based on some of the `varGroups` elements. For
the remaining elements, combinations are sampled.

## Usage

``` r
Extend0_n_rnd_groups(
  data,
  varGroups,
  n_rnd_groups = 0,
  rnd_rep = 1,
  rndSeed = 123
)

Extend0_with_n_rnd_groups(data, varGroups, non_rnd, rnd_rep = 1, rndSeed = 123)
```

## Arguments

- data:

  data.frame within
  [`Extend0`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)

- varGroups:

  argument to
  [`Extend0`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)

- n_rnd_groups:

  The last `n_rnd_groups` elements of `varGroups` are used as the basis
  for sampling.

- rnd_rep:

  When `rnd_rep = 1` (the default), the usual `Extend0` extension is
  performed for the selected `varGroups` elements. When `rnd_rep > 1`,
  this result is replicated to allow more combinations to be sampled.

- rndSeed:

  Internal random seed to be used

- non_rnd:

  In `Extend0_with_n_rnd_groups`, the `varGroups` elements that should
  not be sampled are specified by name. In this case, `varGroups` must
  be a named list.

## Value

A data frame.

## See also

[`Extend0rnd1()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0rnd1.md)

## Examples

``` r

# Data to be extended 
d <- SSBtoolsData("barcelona2025")[c(3, 6, 12, 18), -5]
rownames(d) <- NULL
d$year <- 2025:2026
d$month <- c("January", rep("August", 3))
d
#>   country  city   age    sex year   month
#> 1 Denmark  <NA> young   male 2025 January
#> 2 Finland  <NA>   old female 2026  August
#> 3  France Paris   old female 2025  August
#> 4  France  <NA>   old female 2026  August

# varGroups as a named list
varGroups <- list(geo = c("country", "city"), 
                  age = "age", 
                  sex = "sex", 
                  time = c("month", "year"))

a0 <- Extend0(d, varGroups = varGroups)
dim(a0)  # all combinations, 48 rows
#> [1] 48  7
 
a1 <- Extend0_with_n_rnd_groups(d, varGroups = varGroups, non_rnd = c("time", "geo"))
a1
#>    country  city   age    sex year   month freq
#> 1  Denmark  <NA> young   male 2025 January    1
#> 2  Finland  <NA>   old female 2026  August    1
#> 3   France Paris   old female 2025  August    1
#> 4   France  <NA>   old female 2026  August    1
#> 5  Finland  <NA>   old   male 2025 January    0
#> 6   France Paris   old   male 2025 January    0
#> 7   France  <NA>   old   male 2025 January    0
#> 8  Denmark  <NA>   old female 2026  August    0
#> 9  Finland  <NA> young   male 2026  August    0
#> 10  France Paris young   male 2026  August    0
#> 11 Denmark  <NA> young female 2025  August    0
#> 12 Finland  <NA>   old female 2025  August    0
#> 13  France Paris young female 2025  August    0
#> 14  France  <NA> young female 2025  August    0
dim(unique(a1[c("month", "year", "country", "city")])) # all combinations of time and geo  
#> [1] 12  4

unique(a1[c("sex", "month", "year")])        # not all combinations
#>      sex   month year
#> 1   male January 2025
#> 2 female  August 2026
#> 3 female  August 2025
#> 9   male  August 2026


a2 <- Extend0_with_n_rnd_groups(d, varGroups = varGroups, non_rnd = c("sex", "time", "geo"))
dim(a2)
#> [1] 26  7

# all combinations of selected variables 
dim(unique(a2[c("sex", "month", "year", "country", "city")]))
#> [1] 24  5
 
# not all combinations of selected variables 
dim(unique(a2[c("age", "month", "year", "country", "city")]))  
#> [1] 21  5
 

# effect of rnd_rep
for (rnd_rep in c(1, 2, 3, 5, 10, 50)) 
   print(dim(Extend0_with_n_rnd_groups(d, 
           varGroups = varGroups, non_rnd = c("time", "geo"), rnd_rep = rnd_rep)))
#> [1] 14  7
#> [1] 22  7
#> [1] 29  7
#> [1] 38  7
#> [1] 45  7
#> [1] 48  7
   
```
