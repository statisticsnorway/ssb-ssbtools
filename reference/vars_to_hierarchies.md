# Transform hierarchies coded as Variables to "to-from" format

A kind of reverse operation of
[`hierarchies_as_vars`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md)

## Usage

``` r
vars_to_hierarchies(var_hierarchies)
```

## Arguments

- var_hierarchies:

  As output from
  [`hierarchies_as_vars`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md)

## Value

List of hierarchies

## Examples

``` r
a <- hierarchies_as_vars(list(f = 
       c("AB = A + B", "CD = C + D", "AC = A + C", "ABCD = AB + CD")))
a
#> $f
#>   f f_level_1 f_level_2 f_level_3
#> 1 A        AB        AC      ABCD
#> 2 B        AB      <NA>      ABCD
#> 3 C        CD        AC      ABCD
#> 4 D        CD      <NA>      ABCD
#> 

vars_to_hierarchies(a)
#> $f
#>    mapsFrom mapsTo sign level
#> 1         A     AB    1     1
#> 2         B     AB    1     1
#> 3         C     CD    1     1
#> 4         D     CD    1     1
#> 5         A     AC    1     2
#> 6         C     AC    1     2
#> 7         A   ABCD    1     3
#> 8         B   ABCD    1     3
#> 9         C   ABCD    1     3
#> 10        D   ABCD    1     3
#> 
```
