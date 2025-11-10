# Transform to TsparseMatrix/dgTMatrix

To implement adaption needed after Matrix ver. 1.4-2 since
`as(from, "dgTMatrix")` no longer allowed.

## Usage

``` r
As_TsparseMatrix(from, do_drop0 = TRUE)
```

## Arguments

- from:

  A matrix

- do_drop0:

  whether to run `drop0`

## Value

A matrix. Virtual class is `TsparseMatrix`. Class `dgTMatrix` expected.

## Details

This function is made to replace `as(from, "dgTMatrix")` and
`as(drop0(from), "dgTMatrix")` in `SSBtools` and related packages.

## Note

`Matrix:::.as.via.virtual` in development version of package `Matrix`
(date 2022-08-13) used to generate code.
