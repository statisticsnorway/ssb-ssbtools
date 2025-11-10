# DimList2Hrc/Hrc2DimList

Conversion between hierarchies/dimList as in sdcTable and TauArgus coded
hierarchies

## Usage

``` r
DimList2Hrc(dimList)

Hrc2DimList(hrc, total = "Total")
```

## Arguments

- dimList:

  List of data frames according to the specifications in sdcTable

- hrc:

  List of character vectors

- total:

  String used to name totals.

## Value

See Arguments

## See also

[`DimList2Hierarchy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hierarchy.md),
[`Hierarchy2Formula`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchy2Formula.md),
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).

## Author

Øyvind Langsrud

## Examples

``` r
# First generate dimList
dimList <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")])
dimList
#> $geo
#>   levels    codes
#> 1      @    Total
#> 2     @@       EU
#> 3    @@@ Portugal
#> 4    @@@    Spain
#> 5     @@    nonEU
#> 6    @@@  Iceland
#> 
#> $age
#>   levels  codes
#> 1      @  Total
#> 2     @@ Y15-29
#> 3     @@ Y30-64
#> 
hrc <- DimList2Hrc(dimList)
hrc
#> $geo
#> [1] "EU"        "@Portugal" "@Spain"    "nonEU"     "@Iceland" 
#> 
#> $age
#> [1] "Y15-29" "Y30-64"
#> 
dimList2 <- Hrc2DimList(hrc)
identical(dimList, dimList2)
#> [1] TRUE
```
