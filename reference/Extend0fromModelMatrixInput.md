# A specialized version of Extend0()

`Extend0fromModelMatrixInput()` is a specialized function that extends
the input data based on the provided parameters. It is designed
specifically to work with input to
[`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).

## Usage

``` r
Extend0fromModelMatrixInput(
  data,
  freqName,
  hierarchies,
  formula,
  dimVar,
  extend0,
  dVar = NULL,
  avoidHierarchical = FALSE,
  hierarchical_extend0 = !avoidHierarchical & is.null(hierarchies),
  ...
)

IsExtend0(extend0)
```

## Arguments

- data:

  Input data frame

- freqName:

  Name of (existing) frequency variable

- hierarchies:

  List of hierarchies, which can be converted by
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md).
  Thus, the variables can also be coded by `"rowFactor"` or `""`, which
  correspond to using the categories in the data.

- formula:

  A model formula

- dimVar:

  The main dimensional variables and additional aggregating variables.
  This parameter can be useful when hierarchies and formula are
  unspecified.

- extend0:

  When `extend0` is set to `TRUE`, the data is automatically extended.
  Additionally, `extend0` can be specified as a list, representing the
  `varGroups` parameter in the
  [`Extend0`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)
  function. Can also be set to `"all"` which means that input codes in
  hierarchies are considered in addition to those in data.

- dVar:

  Optional. Specifies the `dimVar` input for
  [`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md).
  If not provided, `dimVar` is calculated by the
  [`NamesFromModelMatrixInput()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
  function.

- avoidHierarchical:

  Parameter passed to
  [`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md)
  via
  [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).
  The default value (`FALSE`) is the same as in the receiving function.

- hierarchical_extend0:

  Specifies the `hierarchical` input to
  [`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md).
  By default, it is set to the opposite of `avoidHierarchical` when
  `hierarchies` is not provided. If `hierarchies` is provided,
  `hierarchical_extend0` is by default set to `FALSE`. This parameter
  allows the `hierarchical` input to
  [`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)
  to be specified manually, independent of the input provided to
  [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).

- ...:

  Further arguments to underlying functions.

## Value

Extended data frame

## Details

- `Extend0fromModelMatrixInput()`: The main function that processes and
  extends input data according to the specified parameters.

- `IsExtend0()`: A helper function that evaluates the `extend0`
  parameter and returns `TRUE` or `FALSE`, indicating whether the data
  should be extended.

## See also

[`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)
