# Changelog

## SSBtools 1.8.6

CRAN release: 2025-12-01

- Added support for named total vectors in
  [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
  and related functions.
  - [`FormulaSums()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md)/[`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md)
    can now accept multiple total codes via named total vectors.
  - See also the underlying functions
    [`FindDimLists()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDimLists.md)
    and
    [`AutoHierarchies()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md),
    which now also support named total vectors.
  - [`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.md)
    has been updated so that named total vectors are handled flexibly in
    connection with both `substitute_vars` and `collapse_vars`.
- New function
  [`convert_integer64()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/convert_integer64.md).
  - Helper for converting `integer64` data (from
    [bit64](https://cran.r-project.org/package=bit64) or
    [arrow](https://cran.r-project.org/package=arrow) imports) to base R
    `integer`, `numeric`, or `character`.
- [`max_contribution()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/max_contribution.md)
  now accepts integer vectors for `y`.
  - Previously, integer input caused an error due to dgTMatrix in the
    Matrix package requiring a numeric (double) `x` slot.

## SSBtools 1.8.4

CRAN release: 2025-10-31

- New functions
  [`diff_groups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/diff_groups.md)
  and
  [`data_diff_groups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/data_diff_groups.md):
  - Identify and describe relationships between paired codes, and add
    results back to data frames.
- New example datasets `code_pairs`, `barcelona2025`, `paris2025_freq`,
  and `paris2025_micro`.

## SSBtools 1.8.2

CRAN release: 2025-09-19

- New functions
  [`zero_col()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/zero_col.md)
  and
  [`single_col()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/zero_col.md):
  - Previously internal only, now exported for use in other packages.
- New helper function
  [`get_colnames()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/get_colnames.md):
  - Returns column names based on either indices or names, working
    consistently across `data.frame`, `tibble`, and `data.table`.

## SSBtools 1.8.1

CRAN release: 2025-08-18

- [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)
  updates:
  - Now handles negative values in the model matrix.  
  - Added new parameter `auto_subSumAny` to optionally disable the
    automatic switch of the singleton method.  
  - These changes prepare
    [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)
    for use in a new algorithm for linked tables.
- [`DummyDuplicated()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DummyDuplicated.md)
  improvements:
  - Now supports empty matrix input (0 columns and/or 0 rows).  
  - The documentation remark “(dummy matrix not required)” is now more
    accurate.
    - Previously, columns with a column sum of 0 were treated as if they
      contained only zeros.
- New function
  [`any_duplicated_rows()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/any_duplicated_rows.md):
  - Fast alternative to
    [`base::anyDuplicated()`](https://rdrr.io/r/base/duplicated.html),
    implemented similarly to
    [`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md).

## SSBtools 1.8.0

CRAN release: 2025-06-19

- The
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)
  function is updated with a new parameter: `cell_grouping`.
  - This allows certain cells to be suppressed or non-suppressed
    together.  
  - The change enables a consistent linked-table algorithm, achieved by
    using an `x` input structured as a block-diagonal matrix.  
  - See also the related new parameters: `table_id` and
    `auto_anySumNOTprimary`.
- New function
  [`formula_term_labels()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/formula_term_labels.md).
  - Part of the utility functions listed under
    [`?formula_utils`](https://statisticsnorway.github.io/ssb-ssbtools/reference/formula_utils.md).  
  - Extracts term labels from the right-hand side of a given R formula.
- Functionality to extract term labels from a data frame.
  - New function
    [`output_term_labels()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/output_term_labels.md).  
  - New parameter `term_labels` in
    [`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.md),
    used to include term labels in the output.

## SSBtools 1.7.5

CRAN release: 2025-03-14

- The title and description in the DESCRIPTION file have been updated  
  to avoid giving the impression that the package is strictly internal
  to Statistics Norway.
- `Matrix` are moved from Depends to Imports
  - To follow best practices for R packages.
- Removed dependency on `stringr`
  - Ensures that `SSBtools` has no dependencies beyond standard R
    packages.
  - Replaced `stringr::str_split()` with base R alternatives in
    [`WildcardGlobbingVector()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/WildcardGlobbingVector.md)
    and
    [`HierarchicalWildcardGlobbing()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchicalWildcardGlobbing.md).
- Fixed usage of the `invert` parameter in
  [`WildcardGlobbingVector()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/WildcardGlobbingVector.md)
  and
  [`HierarchicalWildcardGlobbing()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchicalWildcardGlobbing.md).
  - Previously, the default value was always used.
  - This change was made now because this code was reviewed as part of
    removing the stringr dependency.
- A small improvement to a singleton method in
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md),
  along with a minor fix.
  - See the note under
    [`?NumSingleton`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.md)
    for details on the improvement to `elimination` (4th character).  
  - A previous attempt for a similar improvement was not implemented
    correctly,  
    which in rare cases could lead to unnecessary secondary cells. This
    has now been fixed.
- Improved robustness of
  [`RbindAll()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RbindAll.md).
  - [`RbindAll()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RbindAll.md)
    now correctly handles data frames with 0 rows instead of producing
    an error.  
  - [`RbindAll()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RbindAll.md)
    now also accepts `NULL` as input.  
- A hexagon sticker logo is now visible on the [pkgdown
  website](https://statisticsnorway.github.io/ssb-ssbtools/) and the
  [GitHub repository](https://github.com/statisticsnorway/ssb-ssbtools).

## SSBtools 1.7.0

CRAN release: 2025-02-04

- **New pkgdown website for the package**
  - This package now has a documentation site at
    <https://statisticsnorway.github.io/ssb-ssbtools/>.
- **New function:
  [`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.md)**
  - This function acts as an overlay for functions that produce tabular
    statistics through an interface utilizing the
    [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
    function and its `formula` parameter.
  - Each table (individual statistic) is defined by a formula. The
    output is a single `data.frame` that contains the results for all
    tables.
- **Improvements to
  [`model_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.md)**
  - Now, `avoid_hierarchical`, `input_in_output`, and `total` are direct
    parameters to
    [`model_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.md).
    - Previously, the corresponding
      [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
      parameters (`avoidHierarchical`, `inputInOutput`, and `total`) had
      to be set via the `mm_args` parameter. Old code remains
      functional.  
  - Improved support for `tibble` and `data.table` input (parameter
    `data`).
    - Input is now explicitly coerced to a data frame using
      [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
      ensure consistent behavior.  
  - The pre-aggregation functionality in
    [`model_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.md)
    can now be speeded up.
    - Set the new parameter `aggregate_pkg = "data.table"` to utilize
      this possibility. Also note the related new parameter
      `aggregate_base_order`.  
  - Added a new parameter, `aggregate_na`, to control handling of
    missing values in grouping variables.
    - This is linked to the `NAomit` parameter to
      [`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md),
      which makes it meaningful to include NAs in the grouping
      variables.  
    - When `aggregate_na = TRUE`, NAs in grouping variables are retained
      during pre-aggregation.  
- **Improved
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)
  – now removes duplicate rows**
  - See the updated documentation for the `removeDuplicated`
    parameter.  
  - Previously, only duplicate columns were removed.  
  - This update improves speed, especially when the function is called
    through an interface based on
    [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
    that uses the `hierarchies` parameter together with
    `inputInOutput = FALSE`.  
  - Also note the related new parameter, `printXdim`, which can be used
    to print information about dimensional changes to the console.  
- **Improvements to
  [`map_hierarchies_to_data()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/map_hierarchies_to_data.md)**
  - Duplicate variable names are now handled. See the new parameter
    `when_overwritten`.  
  - A comment attribute is added to the output data frame, containing
    the names of the variables that were added. See the new parameter
    `add_comment`.  
- **Improvements to
  [`hierarchies_as_vars()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md)**
  - See the new parameters `drop_codes` and `include_codes`.  
- **Intercept problem in
  [`combine_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/combine_formulas.md)
  is fixed**
  - When combining formulas with and without intercept using the `"+"`
    operator,  
    it is now ensured that the resulting formula includes an
    intercept.  
- **Additional new functions**
  - [`filter_by_variable()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/filter_by_variable.md)
    and
    [`names_by_variable()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/filter_by_variable.md)
    are functions to  
    filter a list of items or retrieve names based on a variable.  
  - [`Extend0fromModelMatrixInput()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0fromModelMatrixInput.md),
    marked as internal, is a specialized version of
    [`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)  
    designed specifically to work with input to
    [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).

## SSBtools 1.6.0

CRAN release: 2024-12-04

- [`AutoHierarchies()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)
  has been updated to recognize common from-to names, and the `sign`
  variable is now optional.
  - See the new parameter `autoNames` for details on common from-to
    names.

  - Also note the new parameter `autoLevel`, with a default value
    (`TRUE`) that ensures the function behaves as it always has.

  - NAs in the ‘to’ variable are now allowed to support common
    hierarchies, and rows where ‘to’ == ‘from’ are also allowed. Such
    rows are removed before processing the hierarchy, with a warning
    when relevant (*Codes removed due to ‘to’ == ‘from’ or ‘to’ == NA*).

  - Output from functions like `get_klass()` in the [klassR
    package](https://cran.r-project.org/package=klassR) or
    `hier_create()` in the [sdcHierarchies
    package](https://cran.r-project.org/package=sdcHierarchies) can now
    be used directly as input.

  - Example of usage:

    ``` r
    a <- get_klass(classification = "24")
    b <- hier_create(root = "Total", nodes = LETTERS[1:5])
    AutoHierarchies(list(tree = a, letter = b))
    ```
- New hierarchy functionality with hierarchies coded as variables
  (minimal datasets):
  - New function
    [`hierarchies_as_vars()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md):
    - Hierarchies coded as variables.
  - New function
    [`vars_to_hierarchies()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/vars_to_hierarchies.md):
    - Transform hierarchies coded as variables to “to-from” format.
    - A kind of reverse operation of
      [`hierarchies_as_vars()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md).
  - New function
    [`map_hierarchies_to_data()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/map_hierarchies_to_data.md):
    - Add variables to dataset based on hierarchies.
    - Uses
      [`hierarchies_as_vars()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md)
      to transform hierarchies, followed by mapping to the dataset.
- New function
  [`max_contribution()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/max_contribution.md)
  with wrapper
  [`n_contributors()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/max_contribution.md).
  - Find major contributions to aggregates and count contributors.
  - Improved versions of `MaxContribution()` and `Ncontributors()`
    developed in the [GaussSuppression
    package](https://cran.r-project.org/package=GaussSuppression).
- New function
  [`table_all_integers()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/table_all_integers.md).
  - Table all integers from 1 to n
- New function
  [`total_collapse()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/total_collapse.md).
  - Collapse variables to single representation.
- New function
  [`substitute_formula_vars()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/substitute_formula_vars.md).
  - Part of the utility functions listed under
    [`?formula_utils`](https://statisticsnorway.github.io/ssb-ssbtools/reference/formula_utils.md).
  - An improved version of `formula_include_hierarchies()`, which has
    been renamed for clarity and corrected to produce the intended
    output.
- Allow “empty terms” in
  [`FormulaSums()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md)
  when `viaSparseMatrix = TRUE`.
  - “Empty terms” refer to cases where no columns exist in the model
    matrix due to `NAomit`.
  - The old method (`viaSparseMatrix = FALSE`) already handled this
    correctly.
- Minor improvement to `Extent0()`.
  - Now allows 0 input rows when `hierarchical = FALSE`.
- Minor improvement to
  [`FormulaSelection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.md)
  and its identical wrapper
  [`formula_selection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.md).
  - Now supports 0-length selections.

## SSBtools 1.5.5

CRAN release: 2024-10-21

- The function
  [`FormulaSelection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.md)
  and thereby the identical wrapper
  [`formula_selection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.md)
  have been generalized.
  - New parameter named `logical`: When `TRUE`, the logical selection
    vector is returned.
  - [`FormulaSelection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.md)
    is now a generic function, allowing methods for other input objects
    to be added.

## SSBtools 1.5.4

CRAN release: 2024-09-20

- The
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)
  function and related functionality have now been documented in a
  *“Privacy in Statistical Databases 2024”* paper.
  - The package description and function documentations have been
    updated with this reference [(Langsrud,
    2024)](https://doi.org/10.1007/978-3-031-69651-0_6).
- Now the `data.table` package is listed under *Suggests* and can be
  utilized in two functions. See below.
- New function,
  [`aggregate_by_pkg()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.md)
  - This function aggregates data by specified grouping variables, using
    either base R or `data.table`.
  - Note the parameter `include_na`: A logical value indicating whether
    `NA` values in the grouping variables should be included in the
    aggregation. Default is `FALSE`.
  - Will be used in packages depending on SSBtools.
- `NAomit` is new parameter to
  [`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md)
  and
  [`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md)/[`FormulaSums()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md).
  - This is about NAs in the grouping variables.
  - The parameter can be used as input to
    [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).
- `pkg` is new parameter to
  [`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md)
  - Must be either `"base"` (default) or `"data.table"` (for improved
    speed).
- Improved speed of
  [`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md)/[`FormulaSums()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.md).
  - Thus, improved speed of
    [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md).
  - Now, the model matrix is constructed by a single call to
    [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
    instead of building the transposed matrix with
    [`rbind()`](https://rdrr.io/r/base/cbind.html) based on numerous
    [`Matrix::fac2sparse()`](https://rdrr.io/pkg/Matrix/man/sparse.model.matrix.html)
    calls.
  - Further speed improvement can be achieved by setting the new
    parameter, `rowGroupsPackage`, to `data.table`.
- An efficiency bug in
  [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
  is fixed.
  - With `viaOrdinary = TRUE`,
    [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) or
    `sparse.model.matrix()` was called twice.
- [`combine_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/combine_formulas.md)
  is improved
  - A long string problem solved, when long formulas.
- Some technical changes in documentation to comply with standards.

## SSBtools 1.5.2

CRAN release: 2024-05-16

- The
  [`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
  function and related functionality for hierarchical computations have
  now been documented in a paper in The R Journal.
  - The package description has been updated with this reference
    [(Langsrud, 2023)](https://doi.org/10.32614/RJ-2023-088).
- Now, `remove_empty` is an explicit parameter to
  [`model_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.md).
  - Previously, this had to be done via the `mm_args` parameter. Old
    code works as before.
- Some tools for formula manipulation are included.
  - See
    [`?formula_utils`](https://statisticsnorway.github.io/ssb-ssbtools/reference/formula_utils.md)
- Minor change in
  [`Extend0()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0.md)
  to allow even more advanced possibilities by `varGroups`-attribute.
- Fix for a rare problem in
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md),
  - Could happen with parallel eliminations combined with integer
    overflow. Then warning message: *longer object length is not a
    multiple of shorter object length*
- Minor change to the singleton method `"anySum"` in
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)
  to align with best theory.
  - In practice, this rarely makes a difference.
  - The previous behavior can be ensured by setting `singletonMethod` to
    either `"anySumOld"` or `"anySumNOTprimaryOld"`.
- Fixed a zero-weight issue in
  [`quantile_weighted()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/quantile_weighted.md).
  - Now, `quantile_weighted(x=c(0,2,0), weights = c(1,1,0))` correctly
    outputs the 50% value as 1.  
- A function for checking function inputs has been included and can be
  used as either
  [`CheckInput()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/CheckInput.md)
  or
  [`check_input()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/CheckInput.md).
  - The function was originally created in 2016 and has been included in
    internal packages at Statistics Norway (SSB). Due to its widespread
    use, it was beneficial to include it in this CRAN package.

## SSBtools 1.5.0

CRAN release: 2024-01-08

- Last version before any news
