# Wrapper to `aggregate`

Wrapper to [`aggregate`](https://rdrr.io/r/stats/aggregate.html) that
allows multiple functions and functions of several variables

## Usage

``` r
aggregate_multiple_fun(
  data,
  by,
  vars,
  fun = NULL,
  ind = NULL,
  ...,
  name_sep = "_",
  seve_sep = ":",
  multi_sep = ",",
  forward_dots = FALSE,
  dots2dots = FALSE,
  do_unmatrix = TRUE,
  do_unlist = TRUE,
  inc_progress = FALSE
)
```

## Arguments

- data:

  A data frame containing data to be aggregated

- by:

  A data frame defining grouping

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

- ind:

  When non-NULL, a data frame of indices. When NULL, this variable will
  be generated internally as `data.frame(ind = seq_len(nrow(data)))`.
  The parameter is useful for advanced use involving model/dummy
  matrices. For special use (`dummy = FALSE` in
  [`dummy_aggregate`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.md))
  `ind` can also be a two-column data frame.

- ...:

  Further arguments passed to `aggregate` and, depending on
  `forward_dots`/`dots2dots`, forwarded to the functions in `fun` (see
  details).

- name_sep:

  A character string used when output variable names are generated.

- seve_sep:

  A character string used when output variable names are generated from
  functions of several variables.

- multi_sep:

  A character string used when multiple output variable names are sent
  as input.

- forward_dots:

  Logical vector (possibly recycled) for each element of `fun` that
  determines whether `...` should be forwarded (see details).

- dots2dots:

  Logical vector (possibly recycled) specifying the behavior when
  `forward_dots = TRUE` (see details).

- do_unmatrix:

  By default (`TRUE`), the implementation uses
  [`unmatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/unmatrix.md)
  before returning output. For special use this can be omitted
  (`FALSE`).

- do_unlist:

  By default (`TRUE`), the implementation uses
  [`unlist`](https://rdrr.io/r/base/unlist.html) to combine output from
  multiple functions. For special use this can be omitted (`FALSE`).

- inc_progress:

  logigal, `NULL` (same as `FALSE`) or a progress indicator function
  taking two parameters (i and n). `TRUE` means the same as
  [`inc_default`](https://statisticsnorway.github.io/ssb-ssbtools/reference/inc_default.md).
  Note that this feature is implemented in a hacky manner as
  internal/hidden variables are grabbed from
  [`aggregate`](https://rdrr.io/r/stats/aggregate.html).

## Value

A data frame

## Details

One intention of `aggregate_multiple_fun` is to be a true generalization
of `aggregate`. However, when many functions are involved, passing extra
parameters can easily lead to errors. Therefore `forward_dots` and
`dots2dots` are set to `FALSE` by default. When `forward_dots = TRUE`
and `dots2dots = FALSE`, parameters will be forwarded, but only
parameters that are explicitly defined in the specific `fun` function.
For the `sum` function, this means that a possible `na.rm` parameter is
forwarded but not others. When `forward_dots = TRUE` and
`dots2dots = TRUE`, other parameters will also be forwarded to `fun`
functions where `...` is included. For the `sum` function, this means
that such extra parameters will, probably erroneously, be included in
the summation (see examples).

For the function to work with
[`dummy_aggregate`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.md),
the data is subject to [`unlist`](https://rdrr.io/r/base/unlist.html)
before the `fun` functions are called. This does not apply in the
special case where `ind` is a two-column data frame. Then, in the case
of list data, the `fun` functions have to handle this themselves.

A limitation when default output, when `do_unlist = TRUE`, is that
variables in output are forced to have the same class. This is caused by
the [`unlist`](https://rdrr.io/r/base/unlist.html) function being run on
the output. This means, for example, that all the variables will become
numeric when they should have been both integer and numeric.

## Examples

``` r
d2 <- SSBtoolsData("d2")
set.seed(12)
d2$y <- round(rnorm(nrow(d2)), 2)
d <- d2[sample.int(nrow(d2), size = 20), ]
aggregate_multiple_fun(
   data = d, 
   by = d[c("k_group", "main_income")], 
   vars = c("freq", "y", median = "freq", median = "y", e1 = "freq"),
   fun = c(sum, median = median, e1 = function(x) x[1])  
)
#>   k_group main_income freq     y freq_median y_median freq_e1
#> 1     300  assistance   84  0.64        24.0    0.150      38
#> 2     400  assistance   32  2.07        32.0    2.070      32
#> 3     300       other   27 -1.70         6.5   -0.640       6
#> 4     400       other   13 -0.35         6.5   -0.175       9
#> 5     300    pensions   39 -1.10        13.0   -0.370       8
#> 6     400    pensions   20  0.58        20.0    0.580      20
#> 7     300       wages   35 -2.13         9.5   -0.425       2
#> 8     400       wages    2  1.72         1.0    0.860       2

# With functions as named strings 
aggregate_multiple_fun(
   data = d, 
   by = d[c("k_group", "main_income")], 
   vars = c(sum = "y", med = "freq", med = "y"),
   fun = c(sum = "sum", med = "median")
)
#>   k_group main_income y_sum freq_med  y_med
#> 1     300  assistance  0.64     24.0  0.150
#> 2     400  assistance  2.07     32.0  2.070
#> 3     300       other -1.70      6.5 -0.640
#> 4     400       other -0.35      6.5 -0.175
#> 5     300    pensions -1.10     13.0 -0.370
#> 6     400    pensions  0.58     20.0  0.580
#> 7     300       wages -2.13      9.5 -0.425
#> 8     400       wages  1.72      1.0  0.860

# Without specifying functions 
# - equivalent to `fun = c("sum", "median")` 
aggregate_multiple_fun(
   data = d, 
   by = d[c("k_group", "main_income")], 
   vars = c(sum = "y", median = "freq", median = "y")
)
#>   k_group main_income y_sum freq_median y_median
#> 1     300  assistance  0.64        24.0    0.150
#> 2     400  assistance  2.07        32.0    2.070
#> 3     300       other -1.70         6.5   -0.640
#> 4     400       other -0.35         6.5   -0.175
#> 5     300    pensions -1.10        13.0   -0.370
#> 6     400    pensions  0.58        20.0    0.580
#> 7     300       wages -2.13         9.5   -0.425
#> 8     400       wages  1.72         1.0    0.860

# The single unnamed variable feature. Also functions as strings. 
aggregate_multiple_fun(
   data = d, 
   by = d[c("k_group", "main_income")], 
   vars = "y",
   fun = c("sum", "median", "min", "max")
) 
#>   k_group main_income   sum median   min  max
#> 1     300  assistance  0.64  0.150  0.13 0.36
#> 2     400  assistance  2.07  2.070  2.07 2.07
#> 3     300       other -1.70 -0.640 -2.00 1.58
#> 4     400       other -0.35 -0.175 -0.78 0.43
#> 5     300    pensions -1.10 -0.370 -1.00 0.27
#> 6     400    pensions  0.58  0.580  0.58 0.58
#> 7     300       wages -2.13 -0.425 -1.29 0.01
#> 8     400       wages  1.72  0.860 -0.29 2.01

# with multiple outputs (function my_range)
# and with function of two variables (weighted.mean(y, freq))
my_range <- function(x) c(min = min(x), max = max(x))
aggregate_multiple_fun(
   data = d, 
   by = d[c("k_group", "main_income")], 
   vars = list("freq", "y", ra = "freq", wmean  = c("y", "freq")),
   fun = c(sum, ra = my_range, wmean = weighted.mean)
)
#>   k_group main_income freq     y freq_ra.min freq_ra.max y:freq_wmean
#> 1     300  assistance   84  0.64          22          38   0.23928571
#> 2     400  assistance   32  2.07          32          32   2.07000000
#> 3     300       other   27 -1.70           5           9  -0.50592593
#> 4     400       other   13 -0.35           4           9   0.05769231
#> 5     300    pensions   39 -1.10           8          18  -0.28589744
#> 6     400    pensions   20  0.58          20          20   0.58000000
#> 7     300       wages   35 -2.13           2          14  -0.69171429
#> 8     400       wages    2  1.72           0           2   2.01000000

# with specified output variable names
my_range <- function(x) c(min = min(x), max = max(x))
aggregate_multiple_fun(
   data = d, 
   by = d[c("k_group", "main_income")], 
   vars = list("freq", "y", 
               `freqmin,freqmax` = list(ra = "freq"), 
                yWmean  = list(wmean  = c("y", "freq"))),
   fun = c(sum, ra = my_range, wmean = weighted.mean)
)
#>   k_group main_income freq     y freqmin freqmax      yWmean
#> 1     300  assistance   84  0.64      22      38  0.23928571
#> 2     400  assistance   32  2.07      32      32  2.07000000
#> 3     300       other   27 -1.70       5       9 -0.50592593
#> 4     400       other   13 -0.35       4       9  0.05769231
#> 5     300    pensions   39 -1.10       8      18 -0.28589744
#> 6     400    pensions   20  0.58      20      20  0.58000000
#> 7     300       wages   35 -2.13       2      14 -0.69171429
#> 8     400       wages    2  1.72       0       2  2.01000000


# To illustrate forward_dots and dots2dots
q <- d[1, ]
q$w <- 100 * rnorm(1)
for (dots2dots in c(FALSE, TRUE)) for (forward_dots in c(FALSE, TRUE)) {
  cat("\n=======================================\n")
  cat("forward_dots =", forward_dots, ", dots2dots =", dots2dots)
  out <- aggregate_multiple_fun(
    data = q, by = q["k_group"], 
    vars = c(sum = "freq", round = "w"), fun = c("sum", "round"),  
    digits = 3, forward_dots = forward_dots, dots2dots = dots2dots)
  cat("\n")
  print(out)
}
#> 
#> =======================================
#> forward_dots = FALSE , dots2dots = FALSE
#>   k_group freq_sum w_round
#> 1     300        2    -138
#> 
#> =======================================
#> forward_dots = TRUE , dots2dots = FALSE
#>   k_group freq_sum  w_round
#> 1     300        2 -138.293
#> 
#> =======================================
#> forward_dots = FALSE , dots2dots = TRUE
#>   k_group freq_sum w_round
#> 1     300        2    -138
#> 
#> =======================================
#> forward_dots = TRUE , dots2dots = TRUE
#>   k_group freq_sum  w_round
#> 1     300        5 -138.293
# In last case digits forwarded to sum (as ...) 
# and wrongly included in the summation
 
```
