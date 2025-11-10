# Aggregate by base R or data.table

This function aggregates data by specified grouping variables, using
either base R or `data.table`.

## Usage

``` r
aggregate_by_pkg(
  data,
  by,
  var,
  pkg = "base",
  include_na = FALSE,
  fun = sum,
  base_order = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame

- by:

  A character vector specifying the column names to group by.

- var:

  A character vector specifying the column names of the variables to be
  aggregated.

- pkg:

  A character string indicating which package to use for aggregation.
  Must be either `"base"` for base R or `"data.table"` for `data.table`.
  Default is `"base"`.

- include_na:

  A logical value indicating whether `NA` values in the grouping
  variables should be included in the aggregation. Default is `FALSE`.

- fun:

  The function to be applied for aggregation. Default is `sum`.

- base_order:

  A logical value indicating whether to attempt to return the results in
  the same order as base R when using `data.table`. Note that while the
  function strives to maintain this order, it cannot be guaranteed due
  to potential variations in sorting behavior across different systems.
  Default is `TRUE`.

- ...:

  Further arguments passed to
  [`aggregate`](https://rdrr.io/r/stats/aggregate.html) when `pkg` is
  `"base"`

## Value

A data.frame containing the aggregated results.

## Examples

``` r
d <- SSBtoolsData("d2")[1:20, ]
d[[2]] <- as.numeric(d[[2]])
d$y <- as.numeric(1:20)
d$y[2] <- NA
d$county[8:9] <- NA
d$main_income[11:12] <- NA
d$k_group[19:20] <- NA
by <- c("main_income", "county", "k_group")

a1 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"))
a2 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), 
                       include_na = TRUE)
a3 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), 
                       include_na = TRUE, fun = function(x) list(x))
 
if (requireNamespace("data.table", quietly = TRUE)) {  
                       
  b1 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), pkg = "data.table")
  b2 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), pkg = "data.table", 
                         include_na = TRUE)
  b3 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), pkg = "data.table", 
                         include_na = TRUE, fun = function(x) list(x))                        
                       
  print(identical(a1, b1))   # TRUE when base_order succeeds
  print(identical(a2, b2))
  print(identical(a3, b3))
  
}  else {
   print("The 'data.table' package is not installed.")
}
#> [1] TRUE
#> [1] TRUE
#> [1] TRUE
                        
```
