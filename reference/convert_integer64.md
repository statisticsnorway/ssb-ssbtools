# Convert `integer64` data to base R numeric, integer, or character types

Converts `integer64` data (from the **bit64** package) in a
`data.frame`, list, or vector to base R numeric, integer, or character
types.

## Usage

``` r
convert_integer64(
  df,
  to_integer = "if_fits",
  precision_loss = "character",
  always_character = FALSE
)
```

## Arguments

- df:

  A `data.frame`, list, or vector to process.

- to_integer:

  Character string controlling how conversion to integer is handled. The
  rule is applied **variable by variable**. Must be one of:

  - `"never"` — always convert to `numeric`, never to `integer`

  - `"if_fits"` — convert to `integer` if all values fit within 32-bit
    range *(default)*

  - `"if_summable"` — convert to `integer` if the sum of absolute values
    fits within 32-bit range

  - `"always"` — always convert to `integer`, with potential coercion
    warnings

  - `"always_quiet"` — always convert to `integer`, suppressing coercion
    warnings

- precision_loss:

  Character string controlling what happens when 64-bit integers cannot
  be represented exactly as 64-bit floating-point numbers. Must be one
  of:

  - `"character"` — convert to `character` if the value cannot be
    represented exactly *(default)*

  - `"warn"` — convert to `numeric` and allow warnings about precision
    loss

  - `"quiet"` — convert to `numeric` but suppress such warnings

- always_character:

  Logical. If `TRUE`, all `integer64` values are converted directly to
  `character`, overriding both `to_integer` and `precision_loss`.
  Default is `FALSE`.

## Value

The same type of object as the input (`data.frame`, list, or vector),
with all `integer64` values converted to base R `integer`, `numeric`, or
`character` depending on settings.

## Details

Variables with class `integer64` often appear when reading data from
Arrow files, for example using `arrow::read_parquet()`. Arrow supports
64-bit integer values, while the R language (and thus all R packages,
including the tidyverse) only supports 32-bit integers and 64-bit
floating-point numbers. These 64-bit integers therefore need conversion
when loaded into R.

When the input is a `data.frame` or list, conversion is performed
**variable by variable**, and only those with class `integer64` are
modified.

Depending on settings, `integer64` data are converted to base R
`integer`, `numeric` (double), or `character`.

Note that a simpler helper that always converts directly to `numeric`,
without any checks or dependency tests, can be defined as:

                  convert_integer64_to_numeric <- function(df) {
                    df[] <- lapply(df, function(x) {
                      if (inherits(x, "integer64")) as.numeric(x) else x
                    })
                    df
                  }

## Note

This function is written and documented with help from ChatGPT.

## See also

[`bit64::as.integer64()`](https://rdrr.io/pkg/bit64/man/as.integer64.character.html)

## Examples

``` r
if (requireNamespace("bit64", quietly = TRUE)) {
  x  <- bit64::seq.integer64(2025, 10^9, 3 * 10^8)
  print(x)
  
  print(convert_integer64(x*4, "always_quiet"))
  
  df <- data.frame(a = 11:14, b = x, c = 2 * x, d = 3 * x, e = x * x, f = c(22, 23, 24, 25))
  print(df)

  df1 <- convert_integer64(df, "never")
  df2 <- convert_integer64(df, "if_fits")
  df3 <- convert_integer64(df, "if_summable")
  df4 <- convert_integer64(df, "always_quiet")

  print(sapply(df,  class))
  print(sapply(df1, class))
  print(sapply(df2, class))
  print(sapply(df3, class))
  print(sapply(df4, class))
  
  print(df2)
  print(df4)
  
  cat("# Examples showing that integer64 is problematic:\n")
      y <- bit64::seq.integer64(1, 3)
      print(y)
      print(0.5 * y)
      print(y * 0.5)
      matrix(y, 1, 3)   
}
#> integer64
#> [1] 2025      300002025 600002025 900002025
#> [1]       8100 1200008100         NA         NA
#>    a         b          c          d                  e  f
#> 1 11      2025       4050       6075            4100625 22
#> 2 12 300002025  600004050  900006075  90001215004100625 23
#> 3 13 600002025 1200004050 1800006075 360002430004100625 24
#> 4 14 900002025 1800004050 2700006075 810003645004100625 25
#>           a           b           c           d           e           f 
#>   "integer" "integer64" "integer64" "integer64" "integer64"   "numeric" 
#>           a           b           c           d           e           f 
#>   "integer"   "numeric"   "numeric"   "numeric" "character"   "numeric" 
#>           a           b           c           d           e           f 
#>   "integer"   "integer"   "integer"   "numeric" "character"   "numeric" 
#>           a           b           c           d           e           f 
#>   "integer"   "integer"   "numeric"   "numeric" "character"   "numeric" 
#>         a         b         c         d         e         f 
#> "integer" "integer" "integer" "integer" "integer" "numeric" 
#>    a         b          c          d                  e  f
#> 1 11      2025       4050       6075            4100625 22
#> 2 12 300002025  600004050  900006075  90001215004100625 23
#> 3 13 600002025 1200004050 1800006075 360002430004100625 24
#> 4 14 900002025 1800004050 2700006075 810003645004100625 25
#>    a         b          c          d       e  f
#> 1 11      2025       4050       6075 4100625 22
#> 2 12 300002025  600004050  900006075      NA 23
#> 3 13 600002025 1200004050 1800006075      NA 24
#> 4 14 900002025 1800004050         NA      NA 25
#> # Examples showing that integer64 is problematic:
#> integer64
#> [1] 1 2 3
#> integer64
#> [1] 0 0 0
#> integer64
#> [1] 1 1 2
#>               [,1]          [,2]          [,3]
#> [1,] 4.940656e-324 9.881313e-324 1.482197e-323
```
