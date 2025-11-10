# Check for empty matrix columns (or rows)

More generally, checks that both row/col sums and sums of absolute
values equal a target. For `value = 0`, this means all entries are zero.
`single_col()` is a wrapper with `value = 1`, often used to check for
dummy columns/rows with exactly one element that is `1`.

## Usage

``` r
zero_col(x, rows = FALSE, value = 0)

single_col(..., value = 1)
```

## Arguments

- x:

  Numeric matrix. Sparse matrices from the Matrix package are also
  supported.

- rows:

  Logical; if `TRUE` check rows, else columns.

- value:

  Numeric target (default `0`).

- ...:

  Passed to `zero_col()`.

## Value

Logical vector.

## Details

Memory usage is reduced by applying
[`abs()`](https://rdrr.io/r/base/MathFun.html) checks only to
rows/columns whose total sum is already the target.

## Examples

``` r
m <- matrix(c(
  0,  0, 0, 0, 
  1, -1, 0, 0,
  0,  0, 1, 0
), nrow = 3, byrow = TRUE)

zero_col(m)
#> [1] FALSE FALSE FALSE  TRUE
zero_col(m, rows = TRUE)
#> [1]  TRUE FALSE FALSE
single_col(m)
#> [1]  TRUE FALSE  TRUE FALSE
single_col(m, rows = TRUE)
#> [1] FALSE FALSE  TRUE
```
