# Generate a difference formula

Returns a formula containing the terms in `formula_x` that are not
present in `formula_y`.

## Usage

``` r
formula_setdiff(formula_x, formula_y, env = parent.frame())
```

## Arguments

- formula_x:

  An R formula from which terms are selected.

- formula_y:

  An R formula specifying terms to be excluded.

- env:

  the environment for the output formula

## Value

An R formula containing the terms present in `formula_x` but not in
`formula_y`.

## Author

Øyvind Langsrud

## Examples

``` r
formula_x <- ~a * b + c
formula_y <- ~a + b
formula_setdiff(formula_x, formula_y)
#> ~c + a:b - 1
#> <environment: 0x58e6d1b3c6b8>

# The intercept is handled separately
formula_setdiff(~a + b, ~a - 1)
#> ~b
#> <environment: 0x58e6d1b3c6b8>
formula_setdiff(~a + b - 1, ~a)
#> ~b - 1
#> <environment: 0x58e6d1b3c6b8>

# Identical formulas
formula_setdiff(~a + b, ~a + b)
#> ~-1
#> <environment: 0x58e6d1b3c6b8>
```
