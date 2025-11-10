# Simulate Matlab's ':'

Functions to generate increasing sequences

## Usage

``` r
matlabColon(from, to)

SeqInc(from, to)
```

## Arguments

- from:

  numeric. The start value

- to:

  numeric. The end value.

## Value

A numeric vector, possibly empty.

## Details

matlabColon(a,b) returns a:b (R's version) unless a \> b, in which case
it returns integer(0). SeqInc(a,b) is similar, but results in error when
the calculated length of the sequence (1+to-from) is negative.

## See also

[`seq`](https://rdrr.io/r/base/seq.html)

## Author

Bjørn-Helge Mevik (matlabColon) and Øyvind Langsrud (SeqInc)

## Examples

``` r
identical(3:5, matlabColon(3, 5)) ## => TRUE
#> [1] TRUE
3:1 ## => 3 2 1
#> [1] 3 2 1
matlabColon(3, 1) ## => integer(0)
#> integer(0)
try(SeqInc(3, 1)) ## => Error
#> Error in SeqInc(3, 1) : 
#>   Length of sequence (1+to-from) must be non-negative
SeqInc(3, 2)      ## => integer(0)
#> integer(0)
```
