
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
* The function `CheckInput()` for checking function inputs has been included     
  -   The function was originally created in 2016 and has been included in internal packages at Statistics Norway (SSB). Due to its widespread use, it was beneficial to include it in this CRAN package.

## SSBtools	1.5.0

* Last version before any news
