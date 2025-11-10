# Sorting rows of a matrix or data frame

Sorting rows of a matrix or data frame

## Usage

``` r
SortRows(m, cols = 1:dim(m)[2], index.return = FALSE)
```

## Arguments

- m:

  matrix or data frame

- cols:

  Indexes of columns, in the desired order, used for sorting.

- index.return:

  logical indicating if the ordering index vector should be returned
  instead of sorted input.

## Value

sorted `m` or a row index vector

## Author

Øyvind Langsrud

## Examples

``` r
d <- SSBtoolsData("d2w")
SortRows(d[4:7])
#>    other wages assistance pensions
#> 9      3     0          9        2
#> 11     4     2         18       11
#> 3      5     8         35       25
#> 7      6     4         22        8
#> 2      7     1         29       18
#> 10     9     0         32       20
#> 8      9     3         38       15
#> 5      9    14         63       52
#> 1     11    11         55       36
#> 6     12     9         24       22
#> 4     13     2         17       13
SortRows(d, cols = 4:7)
#>    region county k_group other wages assistance pensions
#> 9       I      1     400     3     0          9        2
#> 11      K     10     400     4     2         18       11
#> 3       C      5     300     5     8         35       25
#> 7       G      8     300     6     4         22        8
#> 2       B      4     300     7     1         29       18
#> 10      J     10     400     9     0         32       20
#> 8       H      8     300     9     3         38       15
#> 5       E      6     300     9    14         63       52
#> 1       A      1     300    11    11         55       36
#> 6       F      6     300    12     9         24       22
#> 4       D      5     300    13     2         17       13
SortRows(d, cols = c(2, 4))
#>    region county k_group other wages assistance pensions
#> 9       I      1     400     3     0          9        2
#> 1       A      1     300    11    11         55       36
#> 2       B      4     300     7     1         29       18
#> 3       C      5     300     5     8         35       25
#> 4       D      5     300    13     2         17       13
#> 5       E      6     300     9    14         63       52
#> 6       F      6     300    12     9         24       22
#> 7       G      8     300     6     4         22        8
#> 8       H      8     300     9     3         38       15
#> 11      K     10     400     4     2         18       11
#> 10      J     10     400     9     0         32       20

SortRows(matrix(sample(1:3,15,TRUE),5,3))
#>      [,1] [,2] [,3]
#> [1,]    1    1    1
#> [2,]    1    1    1
#> [3,]    1    2    3
#> [4,]    3    1    2
#> [5,]    3    2    2
```
