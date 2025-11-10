# Hierarchical aggregation via model specification

Internally a dummy/model matrix is created according to the model
specification. This model matrix is used in the aggregation process via
matrix multiplication and/or the function
[`aggregate_multiple_fun`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.md).

## Usage

``` r
model_aggregate(
  data,
  sum_vars = NULL,
  fun_vars = NULL,
  fun = NULL,
  hierarchies = NULL,
  formula = NULL,
  dim_var = NULL,
  total = NULL,
  input_in_output = NULL,
  remove_empty = NULL,
  avoid_hierarchical = NULL,
  preagg_var = NULL,
  dummy = TRUE,
  pre_aggregate = dummy,
  aggregate_pkg = "base",
  aggregate_na = TRUE,
  aggregate_base_order = FALSE,
  list_return = FALSE,
  pre_return = FALSE,
  verbose = TRUE,
  mm_args = NULL,
  ...
)
```

## Arguments

- data:

  Input data containing data to be aggregated, typically a data frame,
  tibble, or data.table. If data is not a classic data frame, it will be
  coerced to one internally.

- sum_vars:

  Variables to be summed. This will be done via matrix multiplication.

- fun_vars:

  Variables to be aggregated by supplied functions. This will be done
  via
  [`aggregate_multiple_fun`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.md)
  and
  [`dummy_aggregate`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.md)
  and `fun_vars` is specified as the parameter `vars`.

- fun:

  The `fun` parameter to
  [`aggregate_multiple_fun`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.md)

- hierarchies:

  The `hierarchies` parameter to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)

- formula:

  The `formula` parameter to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)

- dim_var:

  The `dimVar` parameter to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)

- total:

  When non-NULL, the `total` parameter to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).
  Thus, the actual default value is `"Total"`.

- input_in_output:

  When non-NULL, the `inputInOutput` parameter to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).
  Thus, the actual default value is `TRUE`.

- remove_empty:

  When non-NULL, the `removeEmpty` parameter to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).
  Thus, the actual default value is `TRUE` with formula input without
  hierarchy and otherwise `FALSE` (see
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)).

- avoid_hierarchical:

  When non-NULL, the `avoidHierarchical` parameter to
  [`Formula2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md),
  which is an underlying function of
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).

- preagg_var:

  Extra variables to be used as grouping elements in the pre-aggregate
  step

- dummy:

  The `dummy` parameter to
  [`dummy_aggregate`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.md).
  When `TRUE`, only 0s and 1s are assumed in the generated model matrix.
  When `FALSE`, non-0s in this matrix are passed as an additional first
  input parameter to the `fun` functions.

- pre_aggregate:

  Whether to pre-aggregate data to reduce the dimension of the model
  matrix. Note that all original `fun_vars` observations are retained in
  the aggregated dataset and `pre_aggregate` does not affect the final
  result. However, `pre_aggregate` must be set to `FALSE` when the
  `dummy_aggregate` parameter `dummy` is set to `FALSE` since then
  [`unlist`](https://rdrr.io/r/base/unlist.html) will not be run. An
  exception to this is if the `fun` functions are written to handle list
  data.

- aggregate_pkg:

  Package used to pre-aggregate. Parameter `pkg` to
  [`aggregate_by_pkg`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.md).

- aggregate_na:

  Whether to include NAs in the grouping variables while preAggregate.
  Parameter `include_na` to
  [`aggregate_by_pkg`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.md).

- aggregate_base_order:

  Parameter `base_order` to
  [`aggregate_by_pkg`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.md),
  used when pre-aggregate. The default is set to `FALSE` to avoid
  unnecessary sorting operations. When `TRUE`, an attempt is made to
  return the same result with `data.table` as with base R. This cannot
  be guaranteed due to potential variations in sorting behavior across
  different systems.

- list_return:

  Whether to return a list of separate components including the model
  matrix `x`.

- pre_return:

  Whether to return the pre-aggregate data as a two-component list. Can
  also be combined with `list_return` (see examples).

- verbose:

  Whether to print information during calculations.

- mm_args:

  List of further arguments passed to `ModelMatrix`.

- ...:

  Further arguments passed to `dummy_aggregate`.

## Value

A data frame or a list.

## Details

With formula input, limited output can be achieved by
[`formula_selection`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.md)
(see example). An attribute called `startCol` has been added to the
output data frame to make this functionality work.

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"
names(z)[names(z) == "ths_per"] <- "ths"
z$y <- 1:18

my_range <- function(x) c(min = min(x), max = max(x))

out <- model_aggregate(z, 
   formula = ~age:year + geo, 
   sum_vars = c("y", "ths"), 
   fun_vars = c(sum = "ths", mean = "y", med = "y", ra = "ths"), 
   fun = c(sum = sum, mean = mean, med = median, ra = my_range))
#> [pre_aggregate 18*6->18*7] [ModelMatrix] [crossprod] [dummy_aggregate] [cbind]

out
#>      age  year      geo   y   ths ths_sum y_mean y_med ths_ra.min ths_ra.max
#> 1  Total Total    Total 171 680.8   680.8    9.5   9.5        1.5      122.1
#> 2  Total Total  Iceland  57  10.6    10.6    9.5   9.5        1.5        1.9
#> 3  Total Total Portugal  63 108.8   108.8   10.5  10.5       11.6       25.8
#> 4  Total Total    Spain  51 561.4   561.4    8.5   8.5       63.4      122.1
#> 5    old  2014    Total  15 142.0   142.0    5.0   5.0        1.5      120.3
#> 6    old  2015    Total  33 145.5   145.5   11.0  11.0        1.6      119.6
#> 7    old  2016    Total  51 149.8   149.8   17.0  17.0        1.9      122.1
#> 8  young  2014    Total   6  80.3    80.3    2.0   2.0        1.8       66.9
#> 9  young  2015    Total  24  79.5    79.5    8.0   8.0        1.9       63.4
#> 10 young  2016    Total  42  83.7    83.7   14.0  14.0        1.9       69.1

# Limited output can be achieved by formula_selection
formula_selection(out, ~geo)
#>     age  year      geo   y   ths ths_sum y_mean y_med ths_ra.min ths_ra.max
#> 1 Total Total    Total 171 680.8   680.8    9.5   9.5        1.5      122.1
#> 2 Total Total  Iceland  57  10.6    10.6    9.5   9.5        1.5        1.9
#> 3 Total Total Portugal  63 108.8   108.8   10.5  10.5       11.6       25.8
#> 4 Total Total    Spain  51 561.4   561.4    8.5   8.5       63.4      122.1


# Using the single unnamed variable feature.
model_aggregate(z, formula = ~age, fun_vars = "y", 
                fun = c(sum = sum, mean = mean, med = median, n = length))
#> [pre_aggregate 18*6->2*2] [ModelMatrix] [dummy_aggregate] [cbind]
#>     age sum mean  med  n
#> 1 Total 171  9.5  9.5 18
#> 2   old  99 11.0 11.0  9
#> 3 young  72  8.0  8.0  9


# To illustrate list_return and pre_return 
for (pre_return in c(FALSE, TRUE)) for (list_return in c(FALSE, TRUE)) {
  cat("\n=======================================\n")
  cat("list_return =", list_return, ", pre_return =", pre_return, "\n\n")
  out <- model_aggregate(z, formula = ~age:year, 
                         sum_vars = c("ths", "y"), 
                         fun_vars = c(mean = "y", ra = "y"), 
                         fun = c(mean = mean, ra = my_range), 
                         list_return = list_return,
                         pre_return = pre_return)
  cat("\n")
  print(out)
}
#> 
#> =======================================
#> list_return = FALSE , pre_return = FALSE 
#> 
#> [pre_aggregate 18*6->6*5] [ModelMatrix] [crossprod] [dummy_aggregate] [cbind]
#> 
#>     age  year   ths   y y_mean y_ra.min y_ra.max
#> 1 Total Total 680.8 171    9.5        1       18
#> 2   old  2014 142.0  15    5.0        4        6
#> 3   old  2015 145.5  33   11.0       10       12
#> 4   old  2016 149.8  51   17.0       16       18
#> 5 young  2014  80.3   6    2.0        1        3
#> 6 young  2015  79.5  24    8.0        7        9
#> 7 young  2016  83.7  42   14.0       13       15
#> 
#> =======================================
#> list_return = TRUE , pre_return = FALSE 
#> 
#> [pre_aggregate 18*6->6*5] [ModelMatrix] [crossprod] [dummy_aggregate] 
#> 
#> $cross_table
#>     age  year
#> 1 Total Total
#> 2   old  2014
#> 3   old  2015
#> 4   old  2016
#> 5 young  2014
#> 6 young  2015
#> 7 young  2016
#> 
#> $sum_data
#>               ths   y
#> Total-Total 680.8 171
#> old-2014    142.0  15
#> old-2015    145.5  33
#> old-2016    149.8  51
#> young-2014   80.3   6
#> young-2015   79.5  24
#> young-2016   83.7  42
#> 
#> $fun_data
#>   y_mean y_ra.min y_ra.max
#> 1    9.5        1       18
#> 2    5.0        4        6
#> 3   11.0       10       12
#> 4   17.0       16       18
#> 5    2.0        1        3
#> 6    8.0        7        9
#> 7   14.0       13       15
#> 
#> $x
#> 6 x 7 sparse Matrix of class "dgCMatrix"
#>      Total-Total old-2014 old-2015 old-2016 young-2014 young-2015 young-2016
#> [1,]           1        1        .        .          .          .          .
#> [2,]           1        .        .        .          1          .          .
#> [3,]           1        .        1        .          .          .          .
#> [4,]           1        .        .        .          .          1          .
#> [5,]           1        .        .        1          .          .          .
#> [6,]           1        .        .        .          .          .          1
#> 
#> 
#> =======================================
#> list_return = FALSE , pre_return = TRUE 
#> 
#> [pre_aggregate 18*6->6*5] 
#> 
#> $pre_data
#>     age year          y
#> 1   old 2014    4, 5, 6
#> 2 young 2014    1, 2, 3
#> 3   old 2015 10, 11, 12
#> 4 young 2015    7, 8, 9
#> 5   old 2016 16, 17, 18
#> 6 young 2016 13, 14, 15
#> 
#> $pre_sum
#>     ths  y
#> 1 142.0 15
#> 2  80.3  6
#> 3 145.5 33
#> 4  79.5 24
#> 5 149.8 51
#> 6  83.7 42
#> 
#> 
#> =======================================
#> list_return = TRUE , pre_return = TRUE 
#> 
#> [pre_aggregate 18*6->6*5] [ModelMatrix] [crossprod] [dummy_aggregate] 
#> 
#> $pre_data
#>     age year          y
#> 1   old 2014    4, 5, 6
#> 2 young 2014    1, 2, 3
#> 3   old 2015 10, 11, 12
#> 4 young 2015    7, 8, 9
#> 5   old 2016 16, 17, 18
#> 6 young 2016 13, 14, 15
#> 
#> $pre_sum
#>     ths  y
#> 1 142.0 15
#> 2  80.3  6
#> 3 145.5 33
#> 4  79.5 24
#> 5 149.8 51
#> 6  83.7 42
#> 
#> $cross_table
#>     age  year
#> 1 Total Total
#> 2   old  2014
#> 3   old  2015
#> 4   old  2016
#> 5 young  2014
#> 6 young  2015
#> 7 young  2016
#> 
#> $sum_data
#>               ths   y
#> Total-Total 680.8 171
#> old-2014    142.0  15
#> old-2015    145.5  33
#> old-2016    149.8  51
#> young-2014   80.3   6
#> young-2015   79.5  24
#> young-2016   83.7  42
#> 
#> $fun_data
#>   y_mean y_ra.min y_ra.max
#> 1    9.5        1       18
#> 2    5.0        4        6
#> 3   11.0       10       12
#> 4   17.0       16       18
#> 5    2.0        1        3
#> 6    8.0        7        9
#> 7   14.0       13       15
#> 
#> $x
#> 6 x 7 sparse Matrix of class "dgCMatrix"
#>      Total-Total old-2014 old-2015 old-2016 young-2014 young-2015 young-2016
#> [1,]           1        1        .        .          .          .          .
#> [2,]           1        .        .        .          1          .          .
#> [3,]           1        .        1        .          .          .          .
#> [4,]           1        .        .        .          .          1          .
#> [5,]           1        .        .        1          .          .          .
#> [6,]           1        .        .        .          .          .          1
#> 


# To illustrate preagg_var 
model_aggregate(z, formula = ~age:year, 
sum_vars = c("ths", "y"), 
fun_vars = c(mean = "y", ra = "y"), 
fun = c(mean = mean, ra = my_range), 
preagg_var = "eu",
pre_return = TRUE)[["pre_data"]]
#> [pre_aggregate 18*6->12*6] 
#>      age year    eu      y
#> 1    old 2014    EU   4, 6
#> 2  young 2014    EU   1, 3
#> 3    old 2015    EU 10, 12
#> 4  young 2015    EU   7, 9
#> 5    old 2016    EU 16, 18
#> 6  young 2016    EU 13, 15
#> 7    old 2014 nonEU      5
#> 8  young 2014 nonEU      2
#> 9    old 2015 nonEU     11
#> 10 young 2015 nonEU      8
#> 11   old 2016 nonEU     17
#> 12 young 2016 nonEU     14


# To illustrate hierarchies 
geo_hier <- SSBtoolsData("sprt_emp_geoHier")
model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier), 
                sum_vars = "y", 
                fun_vars = c(sum = "y"))
#> [pre_aggregate 18*6->6*4] [ModelMatrix] [crossprod] [dummy_aggregate] [cbind]
#>      age      geo   y y_sum
#> 1    All       EU 114   114
#> 2    All   Europe 171   171
#> 3    All    nonEU  57    57
#> 4    All  Iceland  57    57
#> 5    All Portugal  63    63
#> 6    All    Spain  51    51
#> 7    old       EU  66    66
#> 8    old   Europe  99    99
#> 9    old    nonEU  33    33
#> 10   old  Iceland  33    33
#> 11   old Portugal  36    36
#> 12   old    Spain  30    30
#> 13 young       EU  48    48
#> 14 young   Europe  72    72
#> 15 young    nonEU  24    24
#> 16 young  Iceland  24    24
#> 17 young Portugal  27    27
#> 18 young    Spain  21    21

####  Special non-dummy cases illustrated below  ####

# Extend the hierarchy to make non-dummy model matrix  
geo_hier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), 
                              mapsTo = "EUandSpain", sign = 1), geo_hier[, -4])

# Warning since non-dummy
# y and y_sum are different 
model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier2), 
                sum_vars = "y", 
                fun_vars = c(sum = "y"))
#> [pre_aggregate 18*6->6*4] [ModelMatrix] [crossprod] [dummy_aggregate
#> Warning: All non-0s in x are treated as 1s. Use dummy = FALSE?
#> ] [cbind]
#>      age        geo   y y_sum
#> 1    All EUandSpain 165   114
#> 2    All         EU 114   114
#> 3    All     Europe 171   171
#> 4    All      nonEU  57    57
#> 5    All    Iceland  57    57
#> 6    All   Portugal  63    63
#> 7    All      Spain  51    51
#> 8    old EUandSpain  96    66
#> 9    old         EU  66    66
#> 10   old     Europe  99    99
#> 11   old      nonEU  33    33
#> 12   old    Iceland  33    33
#> 13   old   Portugal  36    36
#> 14   old      Spain  30    30
#> 15 young EUandSpain  69    48
#> 16 young         EU  48    48
#> 17 young     Europe  72    72
#> 18 young      nonEU  24    24
#> 19 young    Iceland  24    24
#> 20 young   Portugal  27    27
#> 21 young      Spain  21    21

# No warning since dummy since unionComplement = TRUE (see ?HierarchyCompute)
# y and y_sum are equal   
model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier2), 
                sum_vars = "y", 
                fun_vars = c(sum = "y"),
                mm_args = list(unionComplement = TRUE))
#> [pre_aggregate 18*6->6*4] [ModelMatrix] [crossprod] [dummy_aggregate] [cbind]
#>      age        geo   y y_sum
#> 1    All EUandSpain 114   114
#> 2    All         EU 114   114
#> 3    All     Europe 171   171
#> 4    All      nonEU  57    57
#> 5    All    Iceland  57    57
#> 6    All   Portugal  63    63
#> 7    All      Spain  51    51
#> 8    old EUandSpain  66    66
#> 9    old         EU  66    66
#> 10   old     Europe  99    99
#> 11   old      nonEU  33    33
#> 12   old    Iceland  33    33
#> 13   old   Portugal  36    36
#> 14   old      Spain  30    30
#> 15 young EUandSpain  48    48
#> 16 young         EU  48    48
#> 17 young     Europe  72    72
#> 18 young      nonEU  24    24
#> 19 young    Iceland  24    24
#> 20 young   Portugal  27    27
#> 21 young      Spain  21    21

# Non-dummy again, but no warning since dummy = FALSE
# Then pre_aggregate is by default set to FALSE (error when TRUE) 
# fun with extra argument needed (see ?dummy_aggregate)
# y and y_sum2 are equal
model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier2), 
                sum_vars = "y", 
                fun_vars = c(sum2 = "y"),
                fun = c(sum2 = function(x, y) sum(x * y)),
                dummy = FALSE) 
#> [ModelMatrix] [crossprod] [dummy_aggregate] [cbind]
#>      age        geo   y y_sum2
#> 1    All EUandSpain 165    165
#> 2    All         EU 114    114
#> 3    All     Europe 171    171
#> 4    All      nonEU  57     57
#> 5    All    Iceland  57     57
#> 6    All   Portugal  63     63
#> 7    All      Spain  51     51
#> 8    old EUandSpain  96     96
#> 9    old         EU  66     66
#> 10   old     Europe  99     99
#> 11   old      nonEU  33     33
#> 12   old    Iceland  33     33
#> 13   old   Portugal  36     36
#> 14   old      Spain  30     30
#> 15 young EUandSpain  69     69
#> 16 young         EU  48     48
#> 17 young     Europe  72     72
#> 18 young      nonEU  24     24
#> 19 young    Iceland  24     24
#> 20 young   Portugal  27     27
#> 21 young      Spain  21     21
                
```
