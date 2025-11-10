# Make model formula from data taking into account hierarchical variables

Make model formula from data taking into account hierarchical variables

## Usage

``` r
MakeHierFormula(
  data = NULL,
  hGroups = HierarchicalGroups2(data),
  n = length(hGroups),
  sim = TRUE
)
```

## Arguments

- data:

  data frame

- hGroups:

  Output from HierarchicalGroups2()

- n:

  Interaction level or 0 (all levels)

- sim:

  Include "~" when TRUE

## Value

Formula as character string

## Author

Øyvind Langsrud

## Examples

``` r
x <- SSBtoolsData("sprt_emp_withEU")[, -4]
MakeHierFormula(x)
#> [1] "~ age*geo*year + age*eu*year"
MakeHierFormula(x, n = 2)
#> [1] "~ age*geo + age*eu + age*year + geo*year + eu*year"
MakeHierFormula(x, n = 0)
#> [1] "~ age + geo + eu + year + age:geo + age:eu + age:year + geo:year + eu:year + age:geo:year + age:eu:year"
```
