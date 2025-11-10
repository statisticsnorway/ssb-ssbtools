# Create numbering according to unique rows

Create numbering according to unique rows

## Usage

``` r
RowGroups(
  x,
  returnGroups = FALSE,
  returnGroupsId = FALSE,
  NAomit = FALSE,
  pkg = "base"
)
```

## Arguments

- x:

  Data frame or matrix

- returnGroups:

  When TRUE unique rows are returned

- returnGroupsId:

  When TRUE Index of unique rows are returned

- NAomit:

  When `TRUE`, rows containing NAs are omitted, and the corresponding
  index numbers are set to `NA`.

- pkg:

  A character string indicating which package to use. Must be either
  `"base"` for base R or `"data.table"` for `data.table`. Default is
  `"base"`.

## Value

A vector with the numbering or, according to the arguments, a list with
more output.

## Author

Øyvind Langsrud

## Examples

``` r
a <- data.frame(x = c("a", "b"), y = c("A", "B", "A"), z = rep(1:4, 3))
RowGroups(a)
#>  [1] 1 7 2 6 3 5 2 8 1 5 4 6
RowGroups(a, TRUE)
#> $idx
#>  [1] 1 7 2 6 3 5 2 8 1 5 4 6
#> 
#> $groups
#>   x y z
#> 1 a A 1
#> 2 a A 3
#> 3 a B 1
#> 4 a B 3
#> 5 b A 2
#> 6 b A 4
#> 7 b B 2
#> 8 b B 4
#> 
RowGroups(a[, 1:2], TRUE, TRUE)
#> $idx
#>  [1] 1 4 1 3 2 3 1 4 1 3 2 3
#> 
#> $groups
#>   x y
#> 1 a A
#> 2 a B
#> 3 b A
#> 4 b B
#> 
#> $idg
#> [1] 1 5 4 2
#> 
RowGroups(a[, 1, drop = FALSE], TRUE)
#> $idx
#>  [1] 1 2 1 2 1 2 1 2 1 2 1 2
#> 
#> $groups
#>   x
#> 1 a
#> 2 b
#> 
```
