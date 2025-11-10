# Iterative proportional fitting from matrix input

The linear equation, `z = t(x) %*% y`, is (hopefully) solved for `y` by
iterative proportional fitting

## Usage

``` r
Mipf(
  x,
  z = NULL,
  iter = 100,
  yStart = matrix(1, nrow(x), 1),
  eps = 0.01,
  tol = 1e-10,
  reduceBy0 = FALSE,
  reduceByColSums = FALSE,
  reduceByLeverage = FALSE,
  returnDetails = FALSE,
  y = NULL
)
```

## Arguments

- x:

  a matrix

- z:

  a single column matrix

- iter:

  maximum number of iterations

- yStart:

  a starting estimate of `y`

- eps:

  stopping criterion. Maximum allowed value of
  `max(abs(z - t(x) %*% yHat))`

- tol:

  Another stopping criterion. Maximum absolute difference between two
  iterations.

- reduceBy0:

  When TRUE,
  [`Reduce0exact`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Reduce0exact.md)
  used within the function

- reduceByColSums:

  Parameter to
  [`Reduce0exact`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Reduce0exact.md)
  (when TRUE)

- reduceByLeverage:

  Parameter to
  [`Reduce0exact`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Reduce0exact.md)
  (when TRUE)

- returnDetails:

  More output when TRUE.

- y:

  It is possible to set `z` to NULL and supply original `y` instead
  (`z = t(x) %*% y`)

## Value

`yHat`, the estimate of `y`

## Details

The algorithm will work similar to
[`loglin`](https://rdrr.io/r/stats/loglin.html) when the input x-matrix
is a overparameterized model matrix – as can be created by
[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
and
[`FormulaSums`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md).
See Examples.

## Author

Øyvind Langsrud

## Examples

``` r
if (FALSE) { # \dontrun{
data2 <- SSBtoolsData("z2")
x <- ModelMatrix(data2, formula = ~fylke + kostragr * hovedint - 1)
z <- Matrix::t(x) %*% data2$ant  # same as FormulaSums(data2, ant~fylke + kostragr * hovedint -1)
yHat <- Mipf(x, z)

#############################
# loglm comparison  
#############################

if (require(MASS)){

# Increase accuracy
yHat <- Mipf(x, z, eps = 1e-04)

# Run loglm and store fitted values in a data frame
outLoglm <- loglm(ant ~ fylke + kostragr * hovedint, data2, eps = 1e-04, iter = 100)
dfLoglm <- as.data.frame.table(fitted(outLoglm))

# Problem 1: Variable region not in output, but instead the variable .Within.  
# Problem 2: Extra zeros since hierarchy not treated. Impossible combinations in output.

# By sorting data, it becomes clear that the fitted values are the same.
max(abs(sort(dfLoglm$Freq, decreasing = TRUE)[1:nrow(data2)] - sort(yHat, decreasing = TRUE)))

# Modify so that region is in output. Problem 1 avoided.
x <- ModelMatrix(data2, formula = ~region + kostragr * hovedint - 1)
z <- Matrix::t(x) %*% data2$ant  # same as FormulaSums(data2, ant~fylke + kostragr * hovedint -1)
yHat <- Mipf(x, z, eps = 1e-04)
outLoglm <- loglm(ant ~ region + kostragr * hovedint, data2, eps = 1e-04, iter = 100)
dfLoglm <- as.data.frame.table(fitted(outLoglm))

# Now it is possible to merge data
merg <- merge(cbind(data2, yHat), dfLoglm)

# Identical output
max(abs(merg$yHat - merg$Freq))

}
} # }

#############################
# loglin comparison  
#############################


# Generate input data for loglin
n <- 5:9
tab <- array(sample(1:prod(n)), n)

# Input parameters
iter <- 20
eps <- 1e-05

# Estimate yHat by loglin
out <- loglin(tab, list(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3, 4), c(3, 4, 5)), 
              fit = TRUE, iter = iter, eps = eps)
#> 7 iterations: deviation 4.223548e-07 
yHatLoglin <- matrix(((out$fit)), ncol = 1)

# Transform the data for input to Mipf
df <- as.data.frame.table(tab)
names(df)[1:5] <- c("A", "B", "C", "D", "E")
x <- ModelMatrix(df, formula = ~A:B + A:C + A:D + A:E + B:C:D + C:D:E - 1)
z <- Matrix::t(x) %*% df$Freq

# Estimate yHat by Mipf
yHatPMipf <- Mipf(x, z, iter = iter, eps = eps)
#> :......  6 iterations: deviation 4.228204e-07

# Maximal absolute difference
max(abs(yHatPMipf - yHatLoglin))
#> [1] 1.391527e-09

# Note: loglin reports one iteration extra 

# Another example. Only one iteration needed.
max(abs(Mipf(x = FormulaSums(df, ~A:B + C - 1), 
             z = FormulaSums(df, Freq ~ A:B + C -1)) 
             - matrix(loglin(tab, list(1:2, 3), fit = TRUE)$fit, ncol = 1)))
#> :.  1 iterations: deviation 8.940697e-082 iterations: deviation 9.126961e-08 
#> [1] 9.367795e-11


#########################################
# Examples utilizing Reduce0exact 
#########################################

z3 <- SSBtoolsData("z3")
x <- ModelMatrix(z3, formula = ~region + kostragr * hovedint + region * mnd2 + fylke * mnd + 
                     mnd * hovedint + mnd2 * fylke * hovedint - 1)

# reduceBy0, but no iteration improvement. Identical results.
t <- 360
y <- z3$ant
y[round((1:t) * 432/t)] <- 0
z <- Matrix::t(x) %*% y
a1 <- Mipf(x, z, eps = 0.1)
#> :...  4 iterations: deviation 0.04788942
a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 0.1)
#> -z(432*216->72*116):...  4 iterations: deviation 0.04788942
a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 0.1)
#> -z(432*216->72*116):...  4 iterations: deviation 0.04788942
max(abs(a1 - a2))
#> [1] 0
max(abs(a1 - a3))
#> [1] 0


if (FALSE) { # \dontrun{
# Improvement by reduceByColSums. Changing eps and iter give more similar results.
t <- 402
y <- z3$ant
y[round((1:t) * 432/t)] <- 0
z <- Matrix::t(x) %*% y
a1 <- Mipf(x, z, eps = 1)
a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 1)
a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 1)
max(abs(a1 - a2))
max(abs(a1 - a3))


# Improvement by ReduceByLeverage. Changing eps and iter give more similar results.
t <- 378
y <- z3$ant
y[round((1:t) * 432/t)] <- 0
z <- Matrix::t(x) %*% y
a1 <- Mipf(x, z, eps = 1)
a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 1)
a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 1)
a4 <- Mipf(x, z, reduceByLeverage = TRUE, eps = 1)
max(abs(a1 - a2))
max(abs(a1 - a3))
max(abs(a1 - a4))


# Example with small eps and "Iteration stopped since tol reached"
t <- 384
y <- z3$ant
y[round((1:t) * 432/t)] <- 0
z <- Matrix::t(x) %*% y
a1 <- Mipf(x, z, eps = 1e-14)
a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 1e-14)
a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 1e-14)
max(abs(a1 - a2))
max(abs(a1 - a3))
} # }

# All y-data found by reduceByColSums (0 iterations). 
t <- 411
y <- z3$ant
y[round((1:t) * 432/t)] <- 0
z <- Matrix::t(x) %*% y
a1 <- Mipf(x, z)
#> :.  1 iterations: deviation 4.440892e-16
a2 <- Mipf(x, z, reduceBy0 = TRUE)
#> -z(432*216->13*84):.  1 iterations: deviation 0
a3 <- Mipf(x, z, reduceByColSums = TRUE)
#> -zx-(432*216->0*0)   0 iterations
max(abs(a1 - y))
#> [1] 4.440892e-16
max(abs(a2 - y))
#> [1] 0
max(abs(a3 - y))
#> [1] 0
```
