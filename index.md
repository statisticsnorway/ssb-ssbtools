# R package SSBtools

| [SSBtools on CRAN](https://cran.r-project.org/package=SSBtools) |     | [pkgdown website](https://statisticsnorway.github.io/ssb-ssbtools/) |     | [GitHub Repository](https://github.com/statisticsnorway/ssb-ssbtools) |
|-----------------------------------------------------------------|-----|---------------------------------------------------------------------|-----|-----------------------------------------------------------------------|

------------------------------------------------------------------------

[![Mentioned in Awesome Official
Statistics](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

### Description

Includes general data manipulation functions, algorithms for statistical
disclosure control [(Langsrud,
2024)](https://doi.org/10.1007%2F978-3-031-69651-0_6) and functions for
hierarchical computations by sparse model matrices  
[(Langsrud, 2023)](https://doi.org/10.32614%2FRJ-2023-088).

------------------------------------------------------------------------

### Installation

You can install SSBtools from CRAN with

``` r
install.packages("SSBtools")
```

Alternatively install from GitHub
by`devtools::install_github("statisticsnorway/SSBtools")` if you want to
test the newest changes.

------------------------------------------------------------------------

### Some of the functions

|                                                                                                                                                                                                   |                                                                          |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| [AutoHierarchies()](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.html)                                                                                               | Ensure standardized coding of hierarchies                                |
| [GaussIndependent()](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussIndependent.html)/[GaussRank()](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussRank.html)     | Linearly independent rows and columns by Gaussian elimination            |
| [GaussSuppression()](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)                                                                                             | Secondary suppression by Gaussian elimination                            |
| [FindHierarchies()](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindHierarchies.html)/[FindDimLists()](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDimLists.html) | Finding hierarchies automatically from data                              |
| [HierarchicalWildcardGlobbing()](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchicalWildcardGlobbing.html)                                                                     | Find variable combinations by advanced wildcard/globbing specifications. |
| [HierarchyCompute()](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchyCompute.html)                                                                                             | Hierarchical Computations                                                |
| [Match()](https://statisticsnorway.github.io/ssb-ssbtools/reference/Match.html)                                                                                                                   | Matching rows in data frames                                             |
| [Mipf()](https://statisticsnorway.github.io/ssb-ssbtools/reference/Mipf.html)                                                                                                                     | Iterative proportional fitting from matrix input                         |
| [ModelMatrix()](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)                                                                                                       | Model matrix from hierarchies and/or a formula (common interface)        |
| [RowGroups()](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.html)                                                                                                           | Create numbering according to unique rows                                |
| [SSBtoolsData()](https://statisticsnorway.github.io/ssb-ssbtools/reference/SSBtoolsData.html)                                                                                                     | Function that returns a dataset                                          |
| [aggregate_by_pkg()](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.html)                                                                                             | Aggregate by base R or data.table                                        |
| [aggregate_multiple_fun()](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.html)                                                                                 | Wrapper to aggregate                                                     |
| [map_hierarchies_to_data()](https://statisticsnorway.github.io/ssb-ssbtools/reference/map_hierarchies_to_data.html)                                                                               | Add variables to dataset based on hierarchies                            |
| [model_aggregate()](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.html)                                                                                               | Hierarchical aggregation via model specification                         |
| [tables_by_formulas()](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html)                                                                                         | Tabular Statistics Based on Formulas                                     |
| [quantile_weighted()](https://statisticsnorway.github.io/ssb-ssbtools/reference/quantile_weighted.html)                                                                                           | Weighted quantiles                                                       |

📌 See the [broader list of available
functions](https://statisticsnorway.github.io/ssb-ssbtools/reference/index.html).

------------------------------------------------------------------------

Official version on CRAN: <https://cran.r-project.org/package=SSBtools>
