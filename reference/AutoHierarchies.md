# Ensure standardized coding of hierarchies

Automatic convert list of hierarchies coded in different ways to
standardized to-from coding

## Usage

``` r
AutoHierarchies(
  hierarchies,
  data = NULL,
  total = "Total",
  hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level =
    "level"),
  combineHierarchies = TRUE,
  unionComplement = FALSE,
  autoLevel = TRUE,
  autoNames = c(to = "from", parentCode = "code", parent = "child", root = "leaf"),
  ...
)
```

## Arguments

- hierarchies:

  List of hierarchies

- data:

  Matrix or data frame with data containing codes of relevant variables

- total:

  Within `AutoHierarchies`: Vector of total codes (possibly recycled)
  used when running
  [`Hrc2DimList`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hrc.md)
  or
  [`FindDimLists`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDimLists.md).

- hierarchyVarNames:

  Variable names in the hierarchy tables as in
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md).
  However:

  - `level` is by default not required (see `autoLevel` below).

  - If the `sign` variable is missing, it defaults to a variable of 1s.

  - Common 'from-to' variable names are recognized (see `autoNames`
    below).

- combineHierarchies:

  Whether to combine several hierarchies for same variable into a single
  hierarchy (see examples).

- unionComplement:

  Logical vector as in
  [`Hierarchies2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchies2ModelMatrix.md).
  The parameter is only in use when hierarchies are combined.

- autoLevel:

  When TRUE (default), the level is computed automatically, ignoring the
  input level variable. This parameter is passed to
  [`HierarchyFix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyFix.md)..

- autoNames:

  Named character vector of 'from-to' variable names to be automatically
  recognized. These names do not need to be specified in
  `hierarchyVarNames`. Thus, `autoNames` can serve as an alternative to
  `hierarchyVarNames`.

- ...:

  Extra unused parameters

## Value

List of hierarchies

## Details

Input can be to-from coded hierarchies, hierarchies/dimList as in
sdcTable, TauArgus coded hierarchies or formulas. Automatic coding from
data is also supported. Output is on a from ready for input to
[`HierarchyCompute`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.md).
A single string as hierarchy input is assumed to be a total code. Then,
the hierarchy is created as a simple hierarchy where all codes in data
sum up to this total. For consistence with `HierarchyCompute`, the codes
`"rowFactor"` and `"colFactor"` are unchanged. An empty string is
recoded to `"rowFactor"`.

A special possibility is to include character vector(s) as unnamed list
element(s) of `hierarchies`. Then the elements of the character
vector(s) must be variable names within data. This will cause
hierarchies to be created from selected data columns by running
[`FindDimLists`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDimLists.md).
Total coded can be specified by parameter `total` or by naming the
character vector. See examples.

## See also

[`FindHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindHierarchies.md),
[`DimList2Hierarchy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hierarchy.md),
[`DimList2Hrc`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DimList2Hrc.md),
[`Hierarchy2Formula`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Hierarchy2Formula.md),
[`DummyHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DummyHierarchy.md).

## Author

Øyvind Langsrud

## Examples

``` r
# First, create different types of input
z <- SSBtoolsData("sprt_emp_withEU")
yearFormula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
yearHier <- Formula2Hierarchy(yearFormula)
geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
geoDimList2 <- FindDimLists(z[, c("geo", "eu")])[[1]]
geoHrc <- DimList2Hrc(geoDimList)
ageHier <- SSBtoolsData("sprt_emp_ageHier")

h1 <- AutoHierarchies(list(age = ageHier, geo = geoDimList, year = yearFormula))
h2 <- AutoHierarchies(list(age = "Y15-64", geo = geoHrc, year = yearHier), data = z, 
                      total = "Europe")
h3 <- AutoHierarchies(list(age = "Total", geo = geoDimList2, year = "Total"), data = z)
h4 <- FindHierarchies(z[, c(1, 2, 3, 5)])
h5 <- AutoHierarchies(list(age = "Total", geo = "", year = "colFactor"), data = z)
identical(h1, h2)
#> [1] TRUE
identical(h3, h4)
#> [1] TRUE

# Print the resulting hierarchies
h1 # = h2
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29 Y15-64    1     1
#> 2   Y30-64 Y15-64    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU Europe    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU Europe    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $year
#>   mapsFrom  mapsTo sign level
#> 1     2014    y_14    1     1
#> 2    y_all y_15_16    1     2
#> 3     y_14 y_15_16   -1     2
#> 4     2014   y_all    1     1
#> 5     2015   y_all    1     1
#> 6     2016   y_all    1     1
#> 
h3 # = h4
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29  Total    1     1
#> 2   Y30-64  Total    1     1
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
h5
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29  Total    1     1
#> 2   Y30-64  Total    1     1
#> 
#> $geo
#> [1] "rowFactor"
#> 
#> $year
#> [1] "colFactor"
#> 

FindHierarchies(z[, c("geo", "eu", "age")])
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


# ===================================================================== 
#   Examples illustrating the combineHierarchies parameter
# =====================================================================

# First, create data
d <- SSBtoolsData("d2ws")[1:3]
d$isCounty1 <- "NO"
d$isCounty1[d$county == "county-1"] <- "YES"
d
#>   region   county  size isCounty1
#> 1      A county-1   BIG       YES
#> 2      B county-2   BIG        NO
#> 3      C county-2   BIG        NO
#> 4      D county-1 small       YES
#> 5      E county-3 small        NO
#> 6      F county-3 small        NO

# sdcTable coding showing two tree-shaped hierarchies
dimList <- FindDimLists(d)
dimList
#> $region
#>    levels    codes
#> 1       @    Total
#> 2      @@       NO
#> 3     @@@ county-2
#> 4    @@@@        B
#> 5    @@@@        C
#> 6     @@@ county-3
#> 7    @@@@        E
#> 8    @@@@        F
#> 9      @@      YES
#> 10    @@@ county-1
#> 11   @@@@        A
#> 12   @@@@        D
#> 
#> $region
#>   levels codes
#> 1      @ Total
#> 2     @@   BIG
#> 3    @@@     A
#> 4    @@@     B
#> 5    @@@     C
#> 6     @@ small
#> 7    @@@     D
#> 8    @@@     E
#> 9    @@@     F
#> 

# Two tree-shaped hierarchies can still be seen 
# Hierarchies with three and two levels
hA <- AutoHierarchies(dimList, combineHierarchies = FALSE)
hA
#> $region
#>    mapsFrom   mapsTo sign level
#> 1        NO    Total    1     3
#> 2  county-2       NO    1     2
#> 3         B county-2    1     1
#> 4         C county-2    1     1
#> 5  county-3       NO    1     2
#> 6         E county-3    1     1
#> 7         F county-3    1     1
#> 8       YES    Total    1     3
#> 9  county-1      YES    1     2
#> 10        A county-1    1     1
#> 11        D county-1    1     1
#> 
#> $region
#>   mapsFrom mapsTo sign level
#> 1      BIG  Total    1     2
#> 2        A    BIG    1     1
#> 3        B    BIG    1     1
#> 4        C    BIG    1     1
#> 5    small  Total    1     2
#> 6        D  small    1     1
#> 7        E  small    1     1
#> 8        F  small    1     1
#> 

# A single hierarchy with only one level 
# Contains the information needed to create a dummy matrix
hB <- AutoHierarchies(dimList)
hB
#> $region
#>    mapsFrom   mapsTo sign level
#> 1         A county-1    1     1
#> 2         A      YES    1     1
#> 3         A    Total    1     1
#> 4         A      BIG    1     1
#> 5         B county-2    1     1
#> 6         B       NO    1     1
#> 7         B    Total    1     1
#> 8         B      BIG    1     1
#> 9         C county-2    1     1
#> 10        C       NO    1     1
#> 11        C    Total    1     1
#> 12        C      BIG    1     1
#> 13        D county-1    1     1
#> 14        D      YES    1     1
#> 15        D    Total    1     1
#> 16        D    small    1     1
#> 17        E county-3    1     1
#> 18        E       NO    1     1
#> 19        E    Total    1     1
#> 20        E    small    1     1
#> 21        F county-3    1     1
#> 22        F       NO    1     1
#> 23        F    Total    1     1
#> 24        F    small    1     1
#> 

# Dummy matrices from the hierarchies
DummyHierarchies(hA)
#> $region
#> 6 x 6 sparse Matrix of class "dgCMatrix"
#>          A B C D E F
#> county-1 1 . . 1 . .
#> county-2 . 1 1 . . .
#> county-3 . . . . 1 1
#> NO       . 1 1 . 1 1
#> YES      1 . . 1 . .
#> Total    1 1 1 1 1 1
#> 
#> $region
#> 3 x 6 sparse Matrix of class "dgCMatrix"
#>       A B C D E F
#> BIG   1 1 1 . . .
#> small . . . 1 1 1
#> Total 1 1 1 1 1 1
#> 
DummyHierarchies(hB)
#> $region
#> 8 x 6 sparse Matrix of class "dgCMatrix"
#>          A B C D E F
#> BIG      1 1 1 . . .
#> NO       . 1 1 . 1 1
#> Total    1 1 1 1 1 1
#> YES      1 . . 1 . .
#> county-1 1 . . 1 . .
#> county-2 . 1 1 . . .
#> county-3 . . . . 1 1
#> small    . . . 1 1 1
#> 


# ===================================================================== 
#   Special examples with character vector(s) as unnamed list elements
# =====================================================================

# Same output as FindHierarchies above
AutoHierarchies(list(c("geo", "eu", "age")), data = z)
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

# Now combined with a named list element 
AutoHierarchies(list(year = yearHier, c("geo", "eu", "age")), data = z)
#> $year
#>   mapsFrom  mapsTo sign level
#> 1     2014    y_14    1     1
#> 2    y_all y_15_16    1     2
#> 3     y_14 y_15_16   -1     2
#> 4     2014   y_all    1     1
#> 5     2015   y_all    1     1
#> 6     2016   y_all    1     1
#> 
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

# Total codes by unnamed list element as named character vector 
AutoHierarchies(list(year = yearHier, c(Europe = "geo", "eu", All = "age")), data = z)
#> $year
#>   mapsFrom  mapsTo sign level
#> 1     2014    y_14    1     1
#> 2    y_all y_15_16    1     2
#> 3     y_14 y_15_16   -1     2
#> 4     2014   y_all    1     1
#> 5     2015   y_all    1     1
#> 6     2016   y_all    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU Europe    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU Europe    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29    All    1     1
#> 2   Y30-64    All    1     1
#> 

# Two types of year input. Total codes by using the parameter `total`. 
AutoHierarchies(list("year", year = yearHier, c("geo", "eu", "age")), data = z, 
                total = c("allYears", "unused", "Tot"))
#> $year
#>   mapsFrom   mapsTo sign level
#> 1     2014 allYears    1     1
#> 2     2014     y_14    1     1
#> 3     2014    y_all    1     1
#> 4     2015 allYears    1     1
#> 5     2015    y_all    1     1
#> 6     2015  y_15_16    1     1
#> 7     2016 allYears    1     1
#> 8     2016    y_all    1     1
#> 9     2016  y_15_16    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU    Tot    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU    Tot    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29    Tot    1     1
#> 2   Y30-64    Tot    1     1
#> 

# Avoid combineHierarchies to see effect of each year input separately 
# (even earlier return possible with `combineHierarchies = NA`)
AutoHierarchies(list("year", year = yearHier, c("geo", "eu", "age")), data = z, 
                total = c("allYears", "unused", "Tot"), combineHierarchies = FALSE)
#> $year
#>   mapsFrom   mapsTo sign level
#> 1     2014 allYears    1     1
#> 2     2015 allYears    1     1
#> 3     2016 allYears    1     1
#> 
#> $year
#>   mapsFrom  mapsTo sign level
#> 1     2014    y_14    1     1
#> 2    y_all y_15_16    1     2
#> 3     y_14 y_15_16   -1     2
#> 4     2014   y_all    1     1
#> 5     2015   y_all    1     1
#> 6     2016   y_all    1     1
#> 
#> $geo
#>   mapsFrom mapsTo sign level
#> 1       EU    Tot    1     2
#> 2 Portugal     EU    1     1
#> 3    Spain     EU    1     1
#> 4    nonEU    Tot    1     2
#> 5  Iceland  nonEU    1     1
#> 
#> $age
#>   mapsFrom mapsTo sign level
#> 1   Y15-29    Tot    1     1
#> 2   Y30-64    Tot    1     1
#> 
```
