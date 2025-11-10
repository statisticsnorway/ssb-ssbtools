# Fix `vars` parameter to `aggregate_multiple_fun`

Fix `vars` parameter to `aggregate_multiple_fun`

## Usage

``` r
fix_vars_amf(
  vars,
  name_sep = "_",
  seve_sep = ":",
  multi_sep = ",",
  names_data = NULL,
  ...
)
```

## Arguments

- vars:

  vars

- name_sep:

  A character string used when output variable names are generated.

- seve_sep:

  A character string used when output variable names are generated from
  functions of several variables.

- multi_sep:

  A character string used when multiple output variable names are sent
  as input.

- names_data:

  `names(data)` to convert numeric input (indices)

- ...:

  unused parameters

## Value

vars

## Examples

``` r
f <- fix_vars_amf

f(c("freq", "y", median = "freq", median = "y", e1 = "freq"))
#> [[1]]
#>   name    fun        
#> "freq"     "" "freq" 
#> 
#> [[2]]
#> name  fun      
#>  "y"   ""  "y" 
#> 
#> [[3]]
#>          name           fun               
#> "freq_median"      "median"        "freq" 
#> 
#> [[4]]
#>       name        fun            
#> "y_median"   "median"        "y" 
#> 
#> [[5]]
#>      name       fun           
#> "freq_e1"      "e1"    "freq" 
#> 

v1 <- list(sum = "a", sum = "w", q = c("a", "w"), mean = c("b", "w"))
v2 <- list(c(fun = "sum", "a"), c(fun = "sum", "w"), c(fun = "q", "a", "w"), 
           c(fun = "mean", "b", "w"))
v3 <- list(sum = "a", sum = "w", q = c(name = "a:w_q", "a", "w"), 
           `b:w_mean` = list(mean = c("b", "w")))
v4 <- list(c(name = "a_sum", fun = "sum", "a"), 
           c(name = "w_sum", fun = "sum", "w"), 
           c(name = "a:w_q", fun = "q", "a", "w"), 
           c(name = "b:w_mean", fun = "mean", "b", "w"))
v5 <- list(a_sum = c(fun = "sum", "a"), 
           w_sum = c(fun = "sum", "w"), 
           `a:w_q` = c(fun = "q", "a", "w"), 
           `b:w_mean` = c(fun = "mean", "b", "w"))

identical(f(v1), f(v2))
#> [1] TRUE
identical(f(v1), f(v3))
#> [1] TRUE
identical(f(v1), f(v4))
#> [1] TRUE
identical(f(v1), f(v5))
#> [1] TRUE

identical(f(v1), f(f(v1)))
#> [1] TRUE
identical(f(v1), v4)
#> [1] TRUE
```
