# DimList2Hierarchy

From hierarchy/dimList as in sdcTable to to-from coded hierarchy

## Usage

``` r
DimList2Hierarchy(x)
```

## Arguments

- x:

  An element of a dimList as in sdcTable

## Value

Data frame with to-from coded hierarchy

## See also

[`DimList2Hrc`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hrc.md),
[`Hierarchy2Formula`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchy2Formula.md),
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).

## Author

Øyvind Langsrud

## Examples

``` r
# First generate a dimList element 
x <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu")], , total = "Europe")[[1]]
x
#>   levels    codes
#> 1      @   Europe
#> 2     @@       EU
#> 3    @@@ Portugal
#> 4    @@@    Spain
#> 5     @@    nonEU
#> 6    @@@  Iceland

DimList2Hierarchy(x)
#>   mapsFrom mapsTo sign level
#> 1       EU Europe    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU Europe    1     2
#> 5  Iceland  nonEU    1     1
```
