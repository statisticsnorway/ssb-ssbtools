# Non-negative regression fits with a sparse overparameterized model matrix

Assuming `z = t(x) %*% y + noise`, a non-negatively modified least
squares estimate of `t(x) %*% y` is made.

## Usage

``` r
LSfitNonNeg(x, z, limit = 1e-10, viaQR = FALSE, printInc = TRUE)
```

## Arguments

- x:

  A matrix

- z:

  A single column matrix

- limit:

  Lower limit for non-zero fits. Set to `NULL` or `-Inf` to avoid the
  non-zero restriction.

- viaQR:

  Least squares fits obtained using
  [`qr`](https://rdrr.io/r/base/qr.html) when `TRUE`.

- printInc:

  Printing "..." to console when `TRUE`.

## Value

A fitted version of `z`

## Details

The problem is first reduced by elimination some rows of `x` (elements
of `y`) using
[`GaussIndependent`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussIndependent.md).
Thereafter least squares fits are obtained using
[`solve`](https://rdrr.io/r/base/solve.html) or
[`qr`](https://rdrr.io/r/base/qr.html). Possible negative fits will be
forced to zero in the next estimation iteration(s).

## Author

Øyvind Langsrud

## Examples

``` r
set.seed(123)
data2 <- SSBtoolsData("z2")
x <- ModelMatrix(data2, formula = ~fylke + kostragr * hovedint - 1)
z <- Matrix::t(x) %*% data2$ant + rnorm(ncol(x), sd = 3)
LSfitNonNeg(x, z)
#> (-)..........-z......
#> 20 x 1 Matrix of class "dgeMatrix"
#>                           [,1]
#> 1-Total-Total        124.16486
#> 4-Total-Total         53.15576
#> 5-Total-Total        121.52242
#> 6-Total-Total        204.05782
#> 8-Total-Total        104.23415
#> 10-Total-Total        99.99149
#> Total-300-Total      599.12021
#> Total-400-Total      108.00628
#> Total-Total-annet     87.11441
#> Total-Total-arbeid    52.20565
#> Total-Total-soshjelp 344.18318
#> Total-Total-trygd    223.62325
#> Total-300-annet       71.44359
#> Total-300-arbeid      52.20565
#> Total-300-soshjelp   282.23779
#> Total-300-trygd      193.23318
#> Total-400-annet       15.67083
#> Total-400-arbeid       0.00000
#> Total-400-soshjelp    61.94538
#> Total-400-trygd       30.39007
LSfitNonNeg(x, z, limit = NULL)
#> (-)..........
#> 20 x 1 Matrix of class "dgeMatrix"
#>                            [,1]
#> 1-Total-Total        124.083059
#> 4-Total-Total         53.073953
#> 5-Total-Total        121.440610
#> 6-Total-Total        203.976011
#> 8-Total-Total        104.152349
#> 10-Total-Total        99.909680
#> Total-300-Total      599.283822
#> Total-400-Total      107.351839
#> Total-Total-annet     87.332560
#> Total-Total-arbeid    51.060386
#> Total-Total-soshjelp 344.401322
#> Total-Total-trygd    223.841393
#> Total-300-annet       71.143636
#> Total-300-arbeid      53.269117
#> Total-300-soshjelp   281.937840
#> Total-300-trygd      192.933228
#> Total-400-annet       16.188924
#> Total-400-arbeid      -2.208732
#> Total-400-soshjelp    62.463482
#> Total-400-trygd       30.908165

if (FALSE) { # \dontrun{
mf <- ~region*mnd + hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd
data4 <- SSBtoolsData("sosialFiktiv")
x <- ModelMatrix(data4, formula = mf)
z <- Matrix::t(x) %*% data4$ant + rnorm(ncol(x), sd = 3)
zFit <- LSfitNonNeg(x, z)
} # }
```
