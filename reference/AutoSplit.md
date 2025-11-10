# Creating variables by splitting the elements of a character vector without needing a split string

Creating variables by splitting the elements of a character vector
without needing a split string

## Usage

``` r
AutoSplit(
  s,
  split = NULL,
  border = "_",
  revBorder = FALSE,
  noSplit = FALSE,
  varNames = paste("var", 1:100, sep = ""),
  tryReverse = TRUE
)
```

## Arguments

- s:

  The character vector

- split:

  Split string. When NULL (default), automatic splitting without a split
  string.

- border:

  A split character or an integer (move split) to be used when the exact
  split position is not unique.

- revBorder:

  When border is integer the split position is moved from the other
  side.

- noSplit:

  No splitting when TRUE.

- varNames:

  Variable names of the created variables (too many is ok)

- tryReverse:

  When TRUE, the automatic method tries to find more variables by
  splitting from reversed strings.

## Value

A data frame with s as row names.

## Author

Øyvind Langsrud

## Examples

``` r
s <- c("A12-3-A-x","A12-3-B-x","B12-3-A-x","B12-3-B-x",
       "A12-3-A-y","A12-3-B-y","B12-3-A-y","B12-3-B-y")
AutoSplit(s)
#>             var1 var2 var3
#> A12-3-A-x A12-3-   A-    x
#> A12-3-B-x A12-3-   B-    x
#> B12-3-A-x B12-3-   A-    x
#> B12-3-B-x B12-3-   B-    x
#> A12-3-A-y A12-3-   A-    y
#> A12-3-B-y A12-3-   B-    y
#> B12-3-A-y B12-3-   A-    y
#> B12-3-B-y B12-3-   B-    y
AutoSplit(s,border="-")
#>           var1 var2 var3
#> A12-3-A-x  A12  3-A    x
#> A12-3-B-x  A12  3-B    x
#> B12-3-A-x  B12  3-A    x
#> B12-3-B-x  B12  3-B    x
#> A12-3-A-y  A12  3-A    y
#> A12-3-B-y  A12  3-B    y
#> B12-3-A-y  B12  3-A    y
#> B12-3-B-y  B12  3-B    y
AutoSplit(s,split="-")
#>           var1 var2 var3 var4
#> A12-3-A-x  A12    3    A    x
#> A12-3-B-x  A12    3    B    x
#> B12-3-A-x  B12    3    A    x
#> B12-3-B-x  B12    3    B    x
#> A12-3-A-y  A12    3    A    y
#> A12-3-B-y  A12    3    B    y
#> B12-3-A-y  B12    3    A    y
#> B12-3-B-y  B12    3    B    y
AutoSplit(s,border=1)
#>           var1   var2 var3
#> A12-3-A-x   A1 2-3-A-    x
#> A12-3-B-x   A1 2-3-B-    x
#> B12-3-A-x   B1 2-3-A-    x
#> B12-3-B-x   B1 2-3-B-    x
#> A12-3-A-y   A1 2-3-A-    y
#> A12-3-B-y   A1 2-3-B-    y
#> B12-3-A-y   B1 2-3-A-    y
#> B12-3-B-y   B1 2-3-B-    y
AutoSplit(s,border=2)
#>           var1  var2 var3
#> A12-3-A-x  A12 -3-A-    x
#> A12-3-B-x  A12 -3-B-    x
#> B12-3-A-x  B12 -3-A-    x
#> B12-3-B-x  B12 -3-B-    x
#> A12-3-A-y  A12 -3-A-    y
#> A12-3-B-y  A12 -3-B-    y
#> B12-3-A-y  B12 -3-A-    y
#> B12-3-B-y  B12 -3-B-    y
AutoSplit(s,border=2,revBorder=TRUE)
#>           var1 var2 var3
#> A12-3-A-x A12-  3-A   -x
#> A12-3-B-x A12-  3-B   -x
#> B12-3-A-x B12-  3-A   -x
#> B12-3-B-x B12-  3-B   -x
#> A12-3-A-y A12-  3-A   -y
#> A12-3-B-y A12-  3-B   -y
#> B12-3-A-y B12-  3-A   -y
#> B12-3-B-y B12-  3-B   -y
AutoSplit(s,noSplit=TRUE)
#>                var1
#> A12-3-A-x A12-3-A-x
#> A12-3-B-x A12-3-B-x
#> B12-3-A-x B12-3-A-x
#> B12-3-B-x B12-3-B-x
#> A12-3-A-y A12-3-A-y
#> A12-3-B-y A12-3-B-y
#> B12-3-A-y B12-3-A-y
#> B12-3-B-y B12-3-B-y
AutoSplit(s,varNames=c("A","B","C","D"))
#>                A  B C
#> A12-3-A-x A12-3- A- x
#> A12-3-B-x A12-3- B- x
#> B12-3-A-x B12-3- A- x
#> B12-3-B-x B12-3- B- x
#> A12-3-A-y A12-3- A- y
#> A12-3-B-y A12-3- B- y
#> B12-3-A-y B12-3- A- y
#> B12-3-B-y B12-3- B- y
```
