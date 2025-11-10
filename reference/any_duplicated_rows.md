# Fast alternative to `anyDuplicated()`

Implemented similarly to
[`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md).

## Usage

``` r
any_duplicated_rows(x, cols = names(x))
```

## Arguments

- x:

  A data frame, tibble, or data.table.

- cols:

  Columns to check for duplicates.

## Value

Index of the first duplicate row, if any; otherwise 0.

## Details

With `data.table` input and the data.table package available,
`anyDuplicated.data.table()` will be used.

## Examples

``` r
z <- SSBtoolsData("power10to2")
head(z, 12)
#>      a    A  b    B
#> 1   a1 A100 b1 B100
#> 2   a2 A100 b1 B100
#> 3   a3 A200 b1 B100
#> 4   a4 A200 b1 B100
#> 5   a5 A200 b1 B100
#> 6   a6 A300 b1 B100
#> 7   a7 A300 b1 B100
#> 8   a8 A300 b1 B100
#> 9   a9 A300 b1 B100
#> 10 a10 A300 b1 B100
#> 11  a1 A100 b2 B100
#> 12  a2 A100 b2 B100
tail(z)
#>       a    A   b    B
#> 95   a5 A200 b10 B300
#> 96   a6 A300 b10 B300
#> 97   a7 A300 b10 B300
#> 98   a8 A300 b10 B300
#> 99   a9 A300 b10 B300
#> 100 a10 A300 b10 B300

any_duplicated_rows(z, c("A", "B"))
#> [1] 2
any_duplicated_rows(z, c("a", "A", "B"))
#> [1] 11
any_duplicated_rows(z, c("a", "A", "b"))
#> [1] 0
```
