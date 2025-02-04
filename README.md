# R package SSBtools


| [SSBtools on CRAN](https://cran.r-project.org/package=SSBtools) |  | [pkgdown website](https://statisticsnorway.github.io/ssb-ssbtools/) |  | [GitHub Repository](https://github.com/statisticsnorway/ssb-ssbtools) |
|----------------------|---|----------------------|---|----------------------|


***

### Description

Functions used by other packages from Statistics Norway are gathered. General data manipulation functions, algorithms for statistical disclosure control 
[(Langsrud, 2024)](https://doi.org/10.1007%2F978-3-031-69651-0_6) 
and functions for hierarchical computations by sparse model matrices are included 
[(Langsrud, 2023)](https://doi.org/10.32614%2FRJ-2023-088). 


***


### Installation

You can install SSBtools from CRAN with

```r
install.packages("SSBtools")
```

Alternatively install from GitHub by`devtools::install_github("statisticsnorway/SSBtools")` if you want to test the newest changes.


***

### Some of the functions

 Function        |   |
| ---------------------------- | -------------------------------------------------------------- |
| AddLeadingZeros | Add leading zeros to numbers while preserving other text |
| AutoHierarchies | Ensure standardized coding of hierarchies
| AutoSplit	| Creating variables by splitting the elements of a character vector without needing a split string |
| CbindIdMatch |	Combine several data frames by using id variables to match rows |
| DimList2Hrc/Hrc2DimList |	Conversion between hierarchy coding in sdcTable and TauArgus |
| GaussIndependent/GaussRank |  Linearly independent rows and columns by Gaussian elimination |
| GaussSuppression | Secondary suppression by Gaussian elimination |
| FactorLevCorr |	Factor level correlation |
| FindDimLists |	Finding dimList |
| FormulaSums/Formula2ModelMatrix |	Sums (aggregates) and/or sparse model matrix with possible cross table |
| HierarchicalGroups |	Finding hierarchical variable groups |
| HierarchicalWildcardGlobbing |	Find variable combinations by advanced wildcard/globbing specifications. |
| Hierarchies2ModelMatrix |	Model matrix representing crossed hierarchies
| HierarchiesAndFormula2ModelMatrix |	Model matrix representing crossed hierarchies according to a formula
| HierarchyCompute |	Hierarchical Computations |
| Match |	Matching rows in data frames |
| matlabColon |	Simulate Matlab's ':' |
| Mipf |	Iterative proportional fitting from matrix input |
| ModelMatrix |	Model matrix from hierarchies and/or a formula (common interface) |
| RbindAll |	Combining several data frames when the columns don't match |
| Reduce0exact |	Reducing a non-negative regression problem |
| RowGroups |	Create numbering according to unique rows |
| SSBtoolsData |	Function that returns a dataset |
| Stack |	Stack columns from a data frame and include variables. |
| Unstack |	Unstack a column from a data frame and include additional variables. |
| WildcardGlobbing |	Row selection by wildcard/globbing |
| WildcardGlobbingVector |	Selection of elements by wildcard/globbing |
| aggregate_multiple_fun |	Wrapper to aggregate |
| dummy_aggregate |	aggregate_multiple_fun using a dummy matrix |
| model_aggregate |	Hierarchical aggregation via model specification |
| quantile_weighted |	Weighted quantiles |


***


Official version on CRAN: [https://cran.r-project.org/package=SSBtools](https://cran.r-project.org/package=SSBtools)

