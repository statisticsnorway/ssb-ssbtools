# Generate model formula by specifying which variables have totals or not.

Generate model formula by specifying which variables have totals or not.

## Usage

``` r
formula_from_vars(
  nontotal_vars = NULL,
  total_vars = NULL,
  simplify = TRUE,
  env = parent.frame()
)
```

## Arguments

- nontotal_vars:

  character vector of the variable names without totals

- total_vars:

  character vector of the variable names with totals

- simplify:

  logical value, default TRUE. Determines whether the formula should be
  simplified before output or not.

- env:

  the environment for the output formula

## Value

model formula

## Author

Daniel Lupp

## Examples

``` r
formula_from_vars(c("a", "b", "c"), c("a"))
#> ~b:c + a:b:c
#> <environment: 0x55b5f5e783b8>
formula_from_vars(c("a", "b", "c"), c("a", "c"))
#> ~b + b:c + a:b + a:b:c
#> <environment: 0x55b5f5e783b8>
formula_from_vars(c("a", "b", "c"), c("a", "b", "c"))
#> ~a * b * c
#> <environment: 0x55b5f5e783b8>
formula_from_vars(c("a", "b", "c"), NULL)
#> ~a:b:c
#> <environment: 0x55b5f5e783b8>
formula_from_vars(NULL, c("a", "b", "c"))
#> ~a * b * c
#> <environment: 0x55b5f5e783b8>
formula_from_vars(c("a", "b"), c("d"))
#> ~a:b + a:b:d
#> <environment: 0x55b5f5e783b8>
```
