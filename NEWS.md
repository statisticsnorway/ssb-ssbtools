
## SSBtools x.x.x
* New function `max_contribution()` with wrapper `n_contributors()`.
  - Find major contributions to aggregates and count contributors.
  - Improved versions of `MaxContribution()` and `Ncontributors()` developed in the 
    [GaussSuppression package](https://cran.r-project.org/package=GaussSuppression).
* New function `table_all_integers()`.
  -  Table all integers from 1 to n
* New function `total_collapse()`.
  - Collapse variables to single representation.
* Allow "empty terms" in `FormulaSums()` when `viaSparseMatrix = TRUE`.
  - "Empty terms" refer to cases where no columns exist in the model matrix due to `NAomit`.
  - The old method (`viaSparseMatrix = FALSE`) already handled this correctly.
* Minor improvement to `Extent0()`.
  - Now allows 0 input rows when `hierarchical = FALSE`.
* Minor improvement to `FormulaSelection()` and its identical wrapper `formula_selection()`.
  - Now supports 0-length selections.


## SSBtools	1.5.5
* The function `FormulaSelection()` and thereby the identical wrapper `formula_selection()` have been generalized.
  - New parameter named `logical`: When `TRUE`, the logical selection vector is returned.
  - `FormulaSelection()` is now a generic function, allowing methods for other input objects to be added.


## SSBtools	1.5.4
* The `GaussSuppression()` function and related functionality have now been documented in a *"Privacy in Statistical Databases 2024"* paper. 
  - The package description and function documentations have been updated with this reference [(Langsrud, 2024)](https://doi.org/10.1007/978-3-031-69651-0_6). 
* Now the `data.table` package is listed under *Suggests* and can be utilized in two functions.
  See below.
* New function, `aggregate_by_pkg()`
  - This function aggregates data by specified grouping variables, using either base R or `data.table`.
  - Note the parameter `include_na`: A logical value indicating whether `NA` values in the grouping variables should be included in the aggregation. Default is `FALSE`.
  - Will be used in packages depending on SSBtools.
* `NAomit` is new parameter to `RowGroups()` and `Formula2ModelMatrix()`/`FormulaSums()`.
  - This is about NAs in the grouping variables.
  - The parameter can be used as input to `ModelMatrix()`.
* `pkg` is new parameter to `RowGroups()`
  - Must be either `"base"` (default)  or `"data.table"` (for improved speed). 
* Improved speed of `Formula2ModelMatrix()`/`FormulaSums()`.
  - Thus, improved speed of `ModelMatrix()`. 
  - Now, the model matrix is constructed by a single call to `Matrix::sparseMatrix()` 
    instead of building the transposed matrix with `rbind()` based on numerous `Matrix::fac2sparse()` calls.
  - Further speed improvement can be achieved by setting the new parameter, `rowGroupsPackage`, to `data.table`.
* An efficiency bug in `ModelMatrix()` is fixed.
  - With `viaOrdinary = TRUE`, `model.matrix()` or `sparse.model.matrix()` was called twice. 
* `combine_formulas()` is improved
  - A long string problem solved, when long formulas. 
* Some technical changes in documentation to comply with standards.  

  
  

## SSBtools	1.5.2
* The `ModelMatrix()` function and related functionality for hierarchical computations have now been documented in a paper in The R Journal. 
  - The package description has been updated with this reference [(Langsrud, 2023)](https://doi.org/10.32614/RJ-2023-088).
* Now, `remove_empty` is an explicit parameter to `model_aggregate()`. 
  - Previously, this had to be done via the `mm_args` parameter. Old code works as before.
* Some tools for formula manipulation are included.
  - See ` ?formula_utils`
* Minor change in `Extend0()` to allow even more advanced possibilities by `varGroups`-attribute. 
* Fix for a rare problem in `GaussSuppression()`, 
  - Could happen with parallel eliminations combined with integer overflow.
  Then warning message:  *longer object length is not a multiple of shorter object length*
* Minor change to the singleton method `"anySum"` in `GaussSuppression()` to align with best theory.
  - In practice, this rarely makes a difference.
  - The previous behavior can be ensured by setting `singletonMethod` to either `"anySumOld"` or `"anySumNOTprimaryOld"`.
* Fixed a zero-weight issue in `quantile_weighted()`. 
  - Now, `quantile_weighted(x=c(0,2,0), weights = c(1,1,0))` correctly outputs the 50% value as 1.  
* A function for checking function inputs has been included and can be used as either `CheckInput()` or `check_input()`.
  -   The function was originally created in 2016 and has been included in internal packages at Statistics Norway (SSB). Due to its widespread use, it was beneficial to include it in this CRAN package.

## SSBtools	1.5.0

* Last version before any news
