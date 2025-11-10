# Sequence within unique values

Sequence within unique values

## Usage

``` r
UniqueSeq(x, sortdata = matrix(1L, length(x), 0))
```

## Arguments

- x:

  vector

- sortdata:

  matrix or vector to determine sequence order

## Value

integer vector

## Author

Øyvind Langsrud

## Examples

``` r
# 1:4 within A and 1:2 within B
UniqueSeq(c("A", "A", "B", "B", "A", "A"))
#> [1] 1 2 1 2 3 4

# Ordered differently
UniqueSeq(c("A", "A", "B", "B", "A", "A"), c(4, 5, 20, 10, 3, 0))
#> [1] 3 4 2 1 2 1
```
