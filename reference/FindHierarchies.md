# Finding hierarchies automatically from data

[`FindDimLists`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDimLists.md)
and
[`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)
wrapped into a single function.

## Usage

``` r
FindHierarchies(data, total = "Total")
```

## Arguments

- data:

  Matrix or data frame containing the variables (micro data or cell
  counts data).

- total:

  String used to name totals. A vector of length `ncol(data)` is also
  possible (see examples).

## Value

List of hierarchies

## Author

Øyvind Langsrud

## Examples

``` r
dataset <- SSBtoolsData("example1")
FindHierarchies(dataset[1:2])
#> $age
#>   mapsFrom mapsTo sign level
#> 1      old  Total    1     1
#> 2    young  Total    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1  Iceland  Total    1     1
#> 2 Portugal  Total    1     1
#> 3    Spain  Total    1     1
#> 
FindHierarchies(dataset[2:3])
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU  Total    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU  Total    1     2
#> 5  Iceland  nonEU    1     1
#> 
FindHierarchies(dataset[1:4])
#> $age
#>   mapsFrom mapsTo sign level
#> 1      old  Total    1     1
#> 2    young  Total    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU  Total    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU  Total    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $year
#>   mapsFrom mapsTo sign level
#> 1     2014  Total    1     1
#> 2     2015  Total    1     1
#> 3     2016  Total    1     1
#> 

FindHierarchies(SSBtoolsData("magnitude1")[1:4], 
                total = c("TOTAL", "unused1", "Europe", "unused2"))
#> $sector4
#>        mapsFrom  mapsTo sign level
#> 1       private   TOTAL    1     2
#> 2   Agriculture private    1     1
#> 3 Entertainment private    1     1
#> 4      Industry private    1     1
#> 5        public   TOTAL    1     2
#> 6  Governmental  public    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU Europe    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU Europe    1     2
#> 5  Iceland  nonEU    1     1
#> 

x <- rep(c("A", "B", "C"), 3)
y <- rep(c(11, 22, 11), 3)
z <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
zy <- paste(z, y, sep = "")
m <- cbind(x, y, z, zy)
FindHierarchies(m)
#> $x
#>   mapsFrom mapsTo sign level
#> 1       11  Total    1     2
#> 2        A     11    1     1
#> 3        C     11    1     1
#> 4       22  Total    1     2
#> 5        B     22    1     1
#> 
#> $zy
#>    mapsFrom mapsTo sign level
#> 1       111     11    1     1
#> 2       111  Total    1     1
#> 3       111      1    1     1
#> 4       122     22    1     1
#> 5       122  Total    1     1
#> 6       122      1    1     1
#> 7       211     11    1     1
#> 8       211  Total    1     1
#> 9       211      2    1     1
#> 10      222     22    1     1
#> 11      222  Total    1     1
#> 12      222      2    1     1
#> 13      311     11    1     1
#> 14      311  Total    1     1
#> 15      311      3    1     1
#> 16      322     22    1     1
#> 17      322  Total    1     1
#> 18      322      3    1     1
#> 
FindHierarchies(m, total = paste0("A", 1:4))
#> $x
#>   mapsFrom mapsTo sign level
#> 1       11     A1    1     2
#> 2        A     11    1     1
#> 3        C     11    1     1
#> 4       22     A1    1     2
#> 5        B     22    1     1
#> 
#> $zy
#>    mapsFrom mapsTo sign level
#> 1       111     11    1     1
#> 2       111     A4    1     1
#> 3       111      1    1     1
#> 4       122     22    1     1
#> 5       122     A4    1     1
#> 6       122      1    1     1
#> 7       211     11    1     1
#> 8       211     A4    1     1
#> 9       211      2    1     1
#> 10      222     22    1     1
#> 11      222     A4    1     1
#> 12      222      2    1     1
#> 13      311     11    1     1
#> 14      311     A4    1     1
#> 15      311      3    1     1
#> 16      322     22    1     1
#> 17      322     A4    1     1
#> 18      322      3    1     1
#> 
```
