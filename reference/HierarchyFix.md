# Change the hierarchy table to follow the standard

Make sure that variable names and sign coding follow an internal
standard. Level may be computed automatically

## Usage

``` r
HierarchyFix(
  hierarchy,
  hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level =
    "level"),
  autoLevel = TRUE
)
```

## Arguments

- hierarchy:

  data frame with hierarchy table

- hierarchyVarNames:

  variable names

- autoLevel:

  When TRUE, level is computed by automatic method

## Value

data frame with hierarchy table

## Author

Øyvind Langsrud

## Examples

``` r
# Make input data by changing variable names and sign coding.
h <- SSBtoolsData("FIFA2018ABCD")[, 1:3]
names(h)[1:2] <- c("from", "to")
minus <- h$sign < 0
h$sign <- "+"
h$sign[minus] <- "-"

# Run HierarchyFix - Two levels created
HierarchyFix(h, c(mapsFrom = "from", mapsTo = "to", sign = "sign"))
#>        mapsFrom      mapsTo sign level
#> 1     Australia     Oceania    1     1
#> 2          Iran        Asia    1     1
#> 3  Saudi Arabia        Asia    1     1
#> 4         Egypt      Africa    1     1
#> 5       Morocco      Africa    1     1
#> 6       Nigeria      Africa    1     1
#> 7     Argentina     America    1     1
#> 8          Peru     America    1     1
#> 9       Uruguay     America    1     1
#> 10      Croatia      Europe    1     1
#> 11      Denmark      Europe    1     1
#> 12       France      Europe    1     1
#> 13      Iceland      Europe    1     1
#> 14     Portugal      Europe    1     1
#> 15       Russia      Europe    1     1
#> 16        Spain      Europe    1     1
#> 17      Iceland       nonEU    1     1
#> 18       Russia       nonEU    1     1
#> 19       Russia nonSchengen    1     1
#> 20      Croatia nonSchengen    1     1
#> 21       Europe          EU    1     2
#> 22        nonEU          EU   -1     2
#> 23       Europe    Schengen    1     2
#> 24  nonSchengen    Schengen   -1     2

# Extend the hierarchy table
h2 <- rbind(data.frame(from = c("Oceania", "Asia", "Africa", "America", "Europe"),
                       to = "World", sign = "+"),
           data.frame(from = c("World", "Europe"),
                      to = "nonEurope", sign = c("+", "-")), h)

# Run HierarchyFix - Three levels created
HierarchyFix(h2, c(mapsFrom = "from", mapsTo = "to", sign = "sign"))
#>        mapsFrom      mapsTo sign level
#> 1       Oceania       World    1     2
#> 2          Asia       World    1     2
#> 3        Africa       World    1     2
#> 4       America       World    1     2
#> 5        Europe       World    1     2
#> 6         World   nonEurope    1     3
#> 7        Europe   nonEurope   -1     3
#> 8     Australia     Oceania    1     1
#> 9          Iran        Asia    1     1
#> 10 Saudi Arabia        Asia    1     1
#> 11        Egypt      Africa    1     1
#> 12      Morocco      Africa    1     1
#> 13      Nigeria      Africa    1     1
#> 14    Argentina     America    1     1
#> 15         Peru     America    1     1
#> 16      Uruguay     America    1     1
#> 17      Croatia      Europe    1     1
#> 18      Denmark      Europe    1     1
#> 19       France      Europe    1     1
#> 20      Iceland      Europe    1     1
#> 21     Portugal      Europe    1     1
#> 22       Russia      Europe    1     1
#> 23        Spain      Europe    1     1
#> 24      Iceland       nonEU    1     1
#> 25       Russia       nonEU    1     1
#> 26       Russia nonSchengen    1     1
#> 27      Croatia nonSchengen    1     1
#> 28       Europe          EU    1     2
#> 29        nonEU          EU   -1     2
#> 30       Europe    Schengen    1     2
#> 31  nonSchengen    Schengen   -1     2
```
