# Tabular Statistics Based on Formulas

This function acts as an overlay for functions that produce tabular
statistics through an interface utilizing the
[`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
function and its `formula` parameter. Each table (individual statistic)
is defined by a formula. The output is a single `data.frame` that
contains the results for all tables.

## Usage

``` r
tables_by_formulas(
  data,
  table_fun,
  ...,
  table_formulas,
  substitute_vars = NULL,
  auto_collapse = TRUE,
  collapse_vars = NULL,
  total = "Total",
  hierarchical_extend0 = TRUE,
  term_labels = FALSE
)
```

## Arguments

- data:

  The input data to be processed by `table_fun`.

- table_fun:

  The table-producing function to be used.

- ...:

  Additional arguments passed to `table_fun`.

- table_formulas:

  A named list of formulas, where each entry defines a specific table.

- substitute_vars:

  Allows formulas in `table_formulas` to be written in a simplified way.
  If `substitute_vars` is specified, the final formulas are generated
  using
  [`substitute_formula_vars()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/substitute_formula_vars.md)
  with `substitute_vars` as input.

- auto_collapse:

  Logical. If `TRUE`, variables are collapsed using
  [`total_collapse()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/total_collapse.md)
  with the `variables` parameter according to `substitute_vars`.

- collapse_vars:

  When specified,
  [`total_collapse()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/total_collapse.md)
  is called with `collapse_vars` as the `variables` parameter, after any
  call triggered by the `auto_collapse` parameter.

- total:

  A string used to name totals. Passed to both `table_fun` and
  [`total_collapse()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/total_collapse.md).

- hierarchical_extend0:

  Controls automatic hierarchy generation for
  [`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md).
  See "Details" for more information.

- term_labels:

  Logical. If `TRUE`, a `term_labels` column (as constructed by
  [`output_term_labels()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/output_term_labels.md))
  is included as the first column of the output.

## Value

A single `data.frame` containing results for all tables defined in
`table_formulas`.

## Details

To ensure full control over the generated output variables, `table_fun`
is called with `avoid_hierarchical` or `avoidHierarchical` set to
`TRUE`. Desired variables in the output are achieved using
`substitute_vars`, `auto_collapse`, and `collapse_vars`.

If `table_fun` automatically uses
[`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md),
the parameter `hierarchical_extend0` specifies the `hierarchical`
parameter in
[`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)
via
[`Extend0fromModelMatrixInput()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0fromModelMatrixInput.md).
When `hierarchical_extend0` is `TRUE`, hierarchies are generated
automatically. By default, it is set to `TRUE`, preventing excessive
data extension and aligning with the default behavior of
[`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md),
where `avoidHierarchical = FALSE`.

An attribute `table_formulas` is added to `formula` before `table_fun()`
is called. This attribute contains the version of `table_formulas` after
applying `substitute_vars`. This allows for special use in the function
`table_fun()`.

Note: The use of `total_collapse` internally allows handling of variable
names not present in the data. This ensures flexibility when modifying
the `table_formulas` parameter.

## See also

[`filter_by_variable`](https://statisticsnorway.github.io/ssb-ssbtools/reference/filter_by_variable.md)

## Examples

``` r
tables_by_formulas(SSBtoolsData("magnitude1"),
                   table_fun = model_aggregate, 
                   table_formulas = list(table_1 = ~region * sector2, 
                                         table_2 = ~region1:sector4 - 1, 
                                         table_3 = ~region + sector4 - 1), 
                   substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
                   collapse_vars = list(sector = c("sector2", "sector4")), 
                   sum_vars = "value", 
                   total = "T",
                   term_labels = TRUE)
#> [pre_aggregate 20*6->10*5] [ModelMatrix] [crossprod] [cbind]
#>    term_labels   region        sector value table_1 table_2 table_3
#> 1  (Intercept)        T             T 462.3    TRUE   FALSE   FALSE
#> 2          geo  Iceland             T  37.1    TRUE   FALSE    TRUE
#> 3          geo Portugal             T 162.5    TRUE   FALSE    TRUE
#> 4          geo    Spain             T 262.7    TRUE   FALSE    TRUE
#> 5           eu       EU             T 425.2    TRUE   FALSE    TRUE
#> 6           eu    nonEU             T  37.1    TRUE   FALSE    TRUE
#> 7      sector2        T       private 429.5    TRUE   FALSE   FALSE
#> 8      sector2        T        public  32.8    TRUE   FALSE   FALSE
#> 9      sector4        T   Agriculture 240.2   FALSE   FALSE    TRUE
#> 10     sector4        T Entertainment 131.5   FALSE   FALSE    TRUE
#> 11     sector4        T  Governmental  32.8   FALSE   FALSE    TRUE
#> 12     sector4        T      Industry  57.8   FALSE   FALSE    TRUE
#> 13 geo:sector2  Iceland       private  37.1    TRUE   FALSE   FALSE
#> 14 geo:sector2 Portugal       private 138.9    TRUE   FALSE   FALSE
#> 15 geo:sector2 Portugal        public  23.6    TRUE   FALSE   FALSE
#> 16 geo:sector2    Spain       private 253.5    TRUE   FALSE   FALSE
#> 17 geo:sector2    Spain        public   9.2    TRUE   FALSE   FALSE
#> 18  eu:sector2       EU       private 392.4    TRUE   FALSE   FALSE
#> 19  eu:sector2       EU        public  32.8    TRUE   FALSE   FALSE
#> 20  eu:sector2    nonEU       private  37.1    TRUE   FALSE   FALSE
#> 21  eu:sector4       EU   Agriculture 240.2   FALSE    TRUE   FALSE
#> 22  eu:sector4       EU Entertainment 114.7   FALSE    TRUE   FALSE
#> 23  eu:sector4       EU  Governmental  32.8   FALSE    TRUE   FALSE
#> 24  eu:sector4       EU      Industry  37.5   FALSE    TRUE   FALSE
#> 25  eu:sector4    nonEU Entertainment  16.8   FALSE    TRUE   FALSE
#> 26  eu:sector4    nonEU      Industry  20.3   FALSE    TRUE   FALSE
                   
```
