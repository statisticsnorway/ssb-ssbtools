# Replace variables in formula with sum of other variables

Replace variables in formula with sum of other variables

## Usage

``` r
substitute_formula_vars(
  f,
  replacements,
  simplify = FALSE,
  env = parent.frame()
)
```

## Arguments

- f:

  A model formula.

- replacements:

  A named list. The names of `replacements` must correspond to variables
  in `f`. Each element in `replacements` must be a character vector
  consisting of the variables you wish to replace.

- simplify:

  Logical, default is FALSE. Determines whether the formula should be
  expanded and simplified before output or not.

- env:

  The environment for the output formula.

## Value

model formula

## Author

Daniel Lupp and Øyvind Langsrud

## Examples

``` r
f <- ~b + a*c  + b:d
substitute_formula_vars(f, list(a = c("hello", "world", "b"), 
                                b = c("Q1", "Q2")))
#> ~(Q1 + Q2) + (hello + world + b) * c + (Q1 + Q2):d
#> <environment: 0x55a9bb146710>
```
