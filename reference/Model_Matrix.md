# Overparameterized model matrix

All factor levels included

## Usage

``` r
Model_Matrix(
  formula,
  data = NULL,
  mf = model.frame(formula, data = data),
  allFactor = TRUE,
  sparse = FALSE
)
```

## Arguments

- formula:

  formula

- data:

  data frame

- mf:

  model frame (alternative input instead of data)

- allFactor:

  When TRUE all variables are coerced to factor

- sparse:

  When TRUE sparse matrix created by sparse.model.matrix()

## Value

model matrix created via model.matrix() or sparse.model.matrix()

## Details

Example:

`z <- SSBtoolsData("sp_emp_withEU")`

`SSBtools:::Model_Matrix(~age*year + geo, z)`
