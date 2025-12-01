# Combine formulas

Combining formulas by `+` or another operator. This is particularly
useful for linking tables in the case of table building with formulas.

## Usage

``` r
combine_formulas(lof, operator = "+", simplify = FALSE, env = parent.frame())
```

## Arguments

- lof:

  list or vector of formulas to be linked

- operator:

  `"+"` (default), `"*"`, `":"` or another operator

- simplify:

  logical value, default FALSE. Determines whether the formula should be
  expanded and simplified before output or not.

- env:

  the environment for the output formula

## Value

model formula

## Author

Daniel Lupp and Øyvind Langsrud

## Examples

``` r
lof1 <- c(~a+b, ~a:c, ~c*d)
combine_formulas(lof1)
#> ~(a + b) + (a:c) + (c * d)
#> <environment: 0x55b5edaba940>
combine_formulas(lof1, operator = "*")
#> ~(a + b) * (a:c) * (c * d)
#> <environment: 0x55b5edaba940>
combine_formulas(lof1, simplify = TRUE)
#> ~a + b + c + d + a:c + c:d
#> <environment: 0x55b5edaba940>

# Intercept is included when needed
lof2 <- c(~a+b -1, ~a:c -1, ~c*d)
combine_formulas(lof2)
#> ~(a + b - 1) + (a:c - 1) + (c * d) + 1
#> <environment: 0x55b5edaba940>
combine_formulas(lof2, simplify = TRUE)
#> ~a + b + c + d + a:c + c:d
#> <environment: 0x55b5edaba940>
combine_formulas(lof2[1:2])
#> ~(a + b - 1) + (a:c - 1)
#> <environment: 0x55b5edaba940>
combine_formulas(lof2[1:2], simplify = TRUE)
#> ~a + b + a:c - 1
#> <environment: 0x55b5edaba940>
```
