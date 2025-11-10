# `aggregate_multiple_fun` using a dummy matrix

Wrapper to
[`aggregate_multiple_fun`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.md)
that uses a dummy matrix instead of the `by` parameter. Functionality
for non-dummy matrices as well.

## Usage

``` r
dummy_aggregate(
  data,
  x,
  vars,
  fun = NULL,
  dummy = TRUE,
  when_non_dummy = warning,
  keep_names = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame containing data to be aggregated

- x:

  A (sparse) dummy matrix

- vars:

  A named vector or list of variable names in `data`. The elements are
  named by the names of `fun`. All the pairs of variable names and
  function names thus define all the result variables to be generated.

  - Parameter `vars` will converted to an internal standard by the
    function
    [`fix_vars_amf`](https://statisticsnorway.github.io/ssb-ssbtools/reference/fix_vars_amf.md).
    Thus, function names and also output variable names can be coded in
    different ways. Multiple output variable names can be coded using
    `multi_sep`. See examples and examples in
    [`fix_vars_amf`](https://statisticsnorway.github.io/ssb-ssbtools/reference/fix_vars_amf.md).
    Indices instead of variable names are allowed.

  - Omission of (some) names is possible since names can be omitted for
    one function (see `fun` below).

  - A special possible feature is the combination of a single unnamed
    variable and all functions named. In this case, all functions are
    run and output variable names will be identical to the function
    names.

- fun:

  A named list of functions. These names will be used as suffixes in
  output variable names. Name can be omitted for one function. A vector
  of function as strings is also possible. When unnamed, these function
  names will be used directly. See the examples of
  [`fix_fun_amf`](https://statisticsnorway.github.io/ssb-ssbtools/reference/fix_fun_amf.md),
  which is the function used to convert `fun`. Without specifying `fun`,
  the functions, as strings, are taken from the function names coded in
  `vars`.

- dummy:

  When `TRUE`, only 0s and 1s are assumed in `x`. When `FALSE`, non-0s
  in `x` are passed as an additional first input parameter to the `fun`
  functions. Thus, the same result as matrix multiplication is achieved
  with `fun = function(x, y) sum(x * y)`. In this case, the data will
  not be subjected to `unlist`. See
  [`aggregate_multiple_fun`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.md).

- when_non_dummy:

  Function to be called when `dummy` is `TRUE` and when `x` is
  non-dummy. Supply `NULL` to do nothing.

- keep_names:

  When `TRUE`, output row names are inherited from column names in `x`.

- ...:

  Further arguments passed to `aggregate_multiple_fun`

## Value

data frame

## Details

Internally this function make use of the `ind` parameter to
`aggregate_multiple_fun`

## See also

[`aggregate_multiple_fun`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.md)

## Examples

``` r
# Code that generates output similar to the 
# last example in aggregate_multiple_fun

d2 <- SSBtoolsData("d2")
set.seed(12)
d2$y <- round(rnorm(nrow(d2)), 2)
d <- d2[sample.int(nrow(d2), size = 20), ]

x <- ModelMatrix(d, formula = ~main_income:k_group - 1)

# with specified output variable names
my_range <- function(x) c(min = min(x), max = max(x))
dummy_aggregate(
   data = d, 
   x = x, 
   vars = list("freq", "y", 
               `freqmin,freqmax` = list(ra = "freq"), 
                yWmean  = list(wmean  = c("y", "freq"))),
   fun = c(sum, ra = my_range, wmean = weighted.mean))
#>                freq     y freqmin freqmax      yWmean
#> assistance-300   84  0.64      22      38  0.23928571
#> assistance-400   32  2.07      32      32  2.07000000
#> other-300        27 -1.70       5       9 -0.50592593
#> other-400        13 -0.35       4       9  0.05769231
#> pensions-300     39 -1.10       8      18 -0.28589744
#> pensions-400     20  0.58      20      20  0.58000000
#> wages-300        35 -2.13       2      14 -0.69171429
#> wages-400         2  1.72       0       2  2.01000000


# Make a non-dummy matrix 
x2 <- x
x2[17, 2:5] <- c(-1, 3, 0, 10)
x2[, 4] <- 0

# Now warning 
# Result is not same as t(x2) %*% d[["freq"]]
dummy_aggregate(data = d, x = x2, vars = "freq", fun = sum)
#> Warning: All non-0s in x are treated as 1s. Use dummy = FALSE?
#>                freq
#> assistance-300   84
#> assistance-400   50
#> other-300        45
#> other-400         0
#> pensions-300     39
#> pensions-400     20
#> wages-300        35
#> wages-400         2

# Now same as t(x2) %*% d[["freq"]]
dummy_aggregate(data = d, x = x2, 
                vars = "freq", dummy = FALSE,
                fun = function(x, y) sum(x * y))
#>                freq
#> assistance-300   84
#> assistance-400   14
#> other-300        81
#> other-400         0
#> pensions-300    201
#> pensions-400     20
#> wages-300        35
#> wages-400         2


# Same as t(x2) %*% d[["freq"]]  + t(x2^2) %*% d[["y"]] 
dummy_aggregate(data = d, x = x2, 
                vars = list(c("freq", "y")), dummy = FALSE,
                fun = function(x, y1, y2) {sum(x * y1) + sum(x^2 * y2)})
#>                freq:y
#> assistance-300  84.64
#> assistance-400  15.70
#> other-300       75.97
#> other-400        0.00
#> pensions-300   163.27
#> pensions-400    20.58
#> wages-300       32.87
#> wages-400        3.72
                
```
