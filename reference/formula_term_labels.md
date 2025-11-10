# Retrieve term labels from a formula

This function extracts the term labels from the right-hand side of a
given R formula. If an intercept is to be included (and a name for the
intercept is provided), it will be added as the first element of the
returned vector.

## Usage

``` r
formula_term_labels(formula, intercept = "(Intercept)")
```

## Arguments

- formula:

  An R formula, e.g. ~ x1 \* x2.

- intercept:

  A character string indicating the name for the intercept. The default
  value is "(Intercept)". If NULL is provided, the intercept will not be
  included, even if present in the formula.

## Value

A character vector containing the term labels. If an intercept is
present and intercept is not NULL, the intercept is returned first,
followed by the remaining terms.

## Details

The default intercept value, "(Intercept)", is chosen to be consistent
with the intercept label returned by functions such as
[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
and similar modeling functions.

## Note

This function is documented by ChatGPT after some discussion.

## Examples

``` r
# With intercept:
formula_term_labels(~ x1 * x2)
#> [1] "(Intercept)" "x1"          "x2"          "x1:x2"      

# Without intercept:
formula_term_labels(~ x1 * x2, intercept = NULL)
#> [1] "x1"    "x2"    "x1:x2"
```
