# Reducing a non-negative regression problem

The linear equation problem, `z = t(x) %*% y` with y non-negative and x
as a design (dummy) matrix, is reduced to a smaller problem by
identifying elements of `y` that can be found exactly from `x` and `z`.

## Usage

``` r
Reduce0exact(
  x,
  z = NULL,
  reduceByColSums = FALSE,
  reduceByLeverage = FALSE,
  leverageLimit = 0.999999,
  digitsRoundWhole = 9,
  y = NULL,
  yStart = NULL,
  printInc = FALSE
)
```

## Arguments

- x:

  A matrix

- z:

  A single column matrix

- reduceByColSums:

  See Details

- reduceByLeverage:

  See Details

- leverageLimit:

  Limit to determine perfect fit

- digitsRoundWhole:

  [`RoundWhole`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RoundWhole.md)
  parameter for fitted values (when `leverageLimit` and `y` not in
  input)

- y:

  A single column matrix. With `y` in input, `z` in input can be omitted
  and estimating `y` (when `leverageLimit`) is avoided.

- yStart:

  A starting estimate when this function is combined with iterative
  proportional fitting. Zeros in yStart will be used to reduce the
  problem.

- printInc:

  Printing iteration information to console when TRUE

## Value

A list of five elements:

- `x`: A reduced version of input `x`

- `z`: Corresponding reduced `z`

- `yKnown`: Logical, specifying known values of `y`

- `y`: A version of `y` with known values correct and others zero

- `zSkipped`: Logical, specifying omitted columns of `x`

## Details

Exact elements can be identified in three ways in an iterative manner:

1.  By zeros in `z`. This is always done.

2.  By columns in x with a singe nonzero value. Done when
    `reduceByColSums` or `reduceByLeverage` is `TRUE`.

3.  By exact linear regression fit (when leverage is one). Done when
    `reduceByLeverage` is `TRUE`. The leverages are computed by
    `hat(as.matrix(x), intercept = FALSE)`, which can be very time and
    memory consuming. Furthermore, without `y` in input, known values
    will be computed by
    [`ginv`](https://rdrr.io/pkg/MASS/man/ginv.html).

## Author

Øyvind Langsrud

## Examples

``` r
# Make a special data set
d <- SSBtoolsData("sprt_emp")
d$ths_per <- round(d$ths_per)
d <- rbind(d, d)
d$year <- as.character(rep(2014:2019, each = 6))
to0 <- rep(TRUE, 36)
to0[c(6, 14, 17, 18, 25, 27, 30, 34, 36)] <- FALSE
d$ths_per[to0] <- 0

# Values as a single column matrix
y <- Matrix::Matrix(d$ths_per, ncol = 1)

# A model matrix using a special year hierarchy
x <- Hierarchies2ModelMatrix(d, hierarchies = list(geo = "", age = "", year = 
    c("y1418 = 2014+2015+2016+2017+2018", "y1519 = 2015+2016+2017+2018+2019", 
      "y151719 = 2015+2017+2019", "yTotal = 2014+2015+2016+2017+2018+2019")), 
      inputInOutput = FALSE)

# Aggregates 
z <- Matrix::t(x) %*% y
sum(z == 0)  # 5 zeros
#> [1] 5

# From zeros in z
a <- Reduce0exact(x, z)
sum(a$yKnown)   # 17 zeros in y is known
#> [1] 17
dim(a$x)        # Reduced x, without known y and z with zeros 
#> [1] 19 19
dim(a$z)        # Corresponding reduced z 
#> [1] 19  1
sum(a$zSkipped) # 5 elements skipped 
#> [1] 5
Matrix::t(a$y)          # Just zeros (known are 0 and unknown set to 0) 
#> 1 x 36 sparse Matrix of class "dgCMatrix"
#>                                                                             
#> [1,] . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# It seems that three additional y-values can be found directly from z
sum(Matrix::colSums(a$x) == 1)
#> [1] 3

# But it is the same element of y (row 18)
a$x[18, Matrix::colSums(a$x) == 1]
#> Spain:Y30-64:y151719   Spain:Y30-64:y1519  Spain:Y30-64:yTotal 
#>                    1                    1                    1 

# Make use of ones in colSums
a2 <- Reduce0exact(x, z, reduceByColSums = TRUE)
sum(a2$yKnown)          # 18 values in y is known
#> [1] 18
dim(a2$x)               # Reduced x
#> [1] 18 16
dim(a2$z)               # Corresponding reduced z
#> [1] 16  1
a2$y[which(a2$yKnown)]  # The known values of y (unknown set to 0)
#>  [1]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 122   0

# Six ones in leverage values 
# Thus six extra elements in y can be found by linear estimation
hat(as.matrix(a2$x), intercept = FALSE)
#>  [1] 1.0 1.0 1.0 1.0 1.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 1.0

# Make use of ones in leverages (hat-values)
a3 <- Reduce0exact(x, z, reduceByLeverage = TRUE)
sum(a3$yKnown)          # 26 values in y is known (more than 6 extra)
#> [1] 26
dim(a3$x)               # Reduced x
#> [1] 10 15
dim(a3$z)               # Corresponding reduced z
#> [1] 15  1
a3$y[which(a3$yKnown)]  # The known values of y (unknown set to 0)
#>  [1]   0   0   0   0   0  20   0   0   0   0   0   0   0   0   0   0   0   0   0
#> [20]   0   0   0   0 122   0  26

# More than 6 extra is caused by iteration 
# Extra checking of zeros in z after reduction by leverages 
# Similar checking performed also after reduction by colSums
```
