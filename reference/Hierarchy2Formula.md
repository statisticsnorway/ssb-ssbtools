# Hierarchy2Formula

Conversion between to-from coded hierarchy and formulas written with
=, - and +.

## Usage

``` r
Hierarchy2Formula(
  x,
  hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level =
    "level")
)

Formula2Hierarchy(s)

Hierarchies2Formulas(x, ...)
```

## Arguments

- x:

  Data frame with to-from coded hierarchy

- hierarchyVarNames:

  Variable names in the hierarchy tables as in
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md).

- s:

  Character vector of formulas written with =, - and +.

- ...:

  Extra parameters. Only `hierarchyVarNames` is relevant.

## Value

See Arguments

## Note

`Hierarchies2Formulas` is a wrapper for
`lapply(x, Hierarchy2Formula, ...)`

## See also

[`DimList2Hierarchy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hierarchy.md),
[`DimList2Hrc`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hrc.md),
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).

## Author

Øyvind Langsrud

## Examples

``` r
x <- SSBtoolsData("sprt_emp_geoHier")
s <- Hierarchy2Formula(x)
s
#> [1] "Europe = Iceland + Portugal + Spain" "nonEU = Iceland"                    
#> [3] "EU = Europe - nonEU"                
Formula2Hierarchy(s)
#>   mapsFrom mapsTo sign level
#> 1  Iceland Europe    1     1
#> 2 Portugal Europe    1     1
#> 3    Spain Europe    1     1
#> 4  Iceland  nonEU    1     1
#> 5   Europe     EU    1     2
#> 6    nonEU     EU   -1     2

# Demonstrate Hierarchies2Formulas and problems 
hi <- FindHierarchies(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")])
hi
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU  Total    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU  Total    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29  Total    1     1
#> 2   Y30-64  Total    1     1
#> 
Hierarchies2Formulas(hi) # problematic formula since minus sign in coding 
#> $geo
#> [1] "Total = EU + nonEU"    "EU = Portugal + Spain" "nonEU = Iceland"      
#> 
#> $age
#> [1] "Total = Y15-29 + Y30-64"
#> 
AutoHierarchies(Hierarchies2Formulas(hi)) # Not same as hi because of problems 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU  Total    1     2
#> 2    nonEU  Total    1     2
#> 3 Portugal     EU    1     1
#> 4    Spain     EU    1     1
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1      Y15  Total    1     1
#> 2       29  Total   -1     1
#> 3      Y30  Total    1     1
#> 4       64  Total   -1     1
#> 

# Change coding to avoid problems 
hi$age$mapsFrom <- gsub("-", "_", hi$age$mapsFrom)
hi
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU  Total    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU  Total    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15_29  Total    1     1
#> 2   Y30_64  Total    1     1
#> 
Hierarchies2Formulas(hi)
#> $geo
#> [1] "Total = EU + nonEU"    "EU = Portugal + Spain" "nonEU = Iceland"      
#> 
#> $age
#> [1] "Total = Y15_29 + Y30_64"
#> 
AutoHierarchies(Hierarchies2Formulas(hi))
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU  Total    1     2
#> 2    nonEU  Total    1     2
#> 3 Portugal     EU    1     1
#> 4    Spain     EU    1     1
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15_29  Total    1     1
#> 2   Y30_64  Total    1     1
#> 
```
