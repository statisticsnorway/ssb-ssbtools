# Duplicated columns in dummy matrix

The algorithm is based on `crossprod(x)` or `crossprod(x, u)` where `u`
is a vector of random numbers

## Usage

``` r
DummyDuplicated(x, idx = FALSE, rows = FALSE, rnd = FALSE)
```

## Arguments

- x:

  A matrix

- idx:

  Indices returned when TRUE

- rows:

  Duplicated rows instead when TRUE

- rnd:

  Algorithm based on cross product with random numbers when TRUE (dummy
  matrix not required)

## Value

Logical vectors specifying duplicated columns or vector of indices
(first match)

## Details

The efficiency of the default algorithm depends on the sparsity of
`crossprod(x)`. The random values are generated locally within the
function without affecting the random value stream in R.

## Author

Øyvind Langsrud

## Examples

``` r
x <- cbind(1, rbind(diag(2), diag(2)), diag(4)[, 1:2])
z <- Matrix::Matrix(x[c(1:4, 2:3), c(1, 2, 1:5, 5, 2)])

DummyDuplicated(z)
#> [1] FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE
which(DummyDuplicated(z, rows = TRUE))
#> [1] 5 6

# Four ways to obtain the same result
DummyDuplicated(z, idx = TRUE)
#> [1] 1 2 1 2 5 6 7 7 2
DummyDuplicated(z, idx = TRUE, rnd = TRUE)
#> [1] 1 2 1 2 5 6 7 7 2
DummyDuplicated(Matrix::t(z), idx = TRUE, rows = TRUE)
#> [1] 1 2 1 2 5 6 7 7 2
DummyDuplicated(Matrix::t(z), idx = TRUE, rows = TRUE, rnd = TRUE)
#> [1] 1 2 1 2 5 6 7 7 2

# The unique values in four ways 
which(!DummyDuplicated(z), )
#> [1] 1 2 5 6 7
which(!DummyDuplicated(z, rnd = TRUE))
#> [1] 1 2 5 6 7
which(!DummyDuplicated(Matrix::t(z), rows = TRUE))
#> [1] 1 2 5 6 7
which(!DummyDuplicated(Matrix::t(z), rows = TRUE, rnd = TRUE))
#> [1] 1 2 5 6 7
```
