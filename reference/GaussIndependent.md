# Linearly independent rows and columns by Gaussian elimination

The function is written primarily for large sparse matrices with
integers and even more correctly it is primarily written for dummy
matrices (0s and 1s in input matrix).

## Usage

``` r
GaussIndependent(
  x,
  printInc = FALSE,
  tolGauss = (.Machine$double.eps)^(1/2),
  testMaxInt = 0,
  allNumeric = FALSE
)

GaussRank(x, printInc = FALSE)
```

## Arguments

- x:

  A (sparse) matrix

- printInc:

  Printing "..." to console when `TRUE`

- tolGauss:

  A tolerance parameter for sparse Gaussian elimination and linear
  dependency. This parameter is used only in cases where integer
  calculation cannot be used.

- testMaxInt:

  Parameter for testing: The Integer overflow situation will be forced
  when testMaxInt is exceeded

- allNumeric:

  Parameter for testing: All calculations use numeric algorithm (as
  integer overflow) when TRUE

## Value

List of logical vectors specifying independent rows and columns

## Details

GaussRank returns the rank

## Note

The main algorithm is based on integers and exact calculations. When
integers cannot be used (because of input or overflow), the algorithm
switches. With `printInc = TRUE` as a parameter, `.....` change to
`-----` when switching to numeric algorithm. With numeric algorithm, a
kind of tolerance for linear dependency is included. This tolerance is
designed having in mind that the input matrix is a dummy matrix.

## Examples

``` r
x <- ModelMatrix(SSBtoolsData("z2"), formula = ~fylke + kostragr * hovedint - 1)

GaussIndependent(x)
#> $rows
#>  [1]  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE
#> [25] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE
#> [37] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
#> 
#> $columns
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE
#> [13]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
#> 
GaussRank(x)
#> [1] 13
GaussRank(Matrix::t(x))
#> [1] 13

if (FALSE) { # \dontrun{
# For comparison, qr-based rank may not work
rankMatrix(x, method = "qr")

# Dense qr works 
qr(as.matrix(x))$rank
} # }
```
