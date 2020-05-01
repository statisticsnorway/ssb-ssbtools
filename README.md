# R package SSBtools

Tools used by other packages from Statistics Norway

Official version on CRAN: [https://cran.r-project.org/package=SSBtools](https://cran.r-project.org/package=SSBtools)


```r
install.packages("SSBtools")  # Install from CRAN 
library(SSBtools)             # Load SSBtools
?HierarchyCompute             # Help documentation of function HierarchyCompute

# Alternatively install from GitHub if you want to test the newest changes
devtools::install_github("statisticsnorway/SSBtools") 
```

### Some of the functions

 Function        |   |
| ---------------------------- | -------------------------------------------------------------- |
| AddLeadingZeros | Add leading zeros to numbers while preserving other text |
| AutoHierarchies | Ensure standardized coding of hierarchies
| AutoSplit	| Creating variables by splitting the elements of a character vector without needing a split string |
| CbindIdMatch |	Combine several data frames by using id variables to match rows |
| DimList2Hrc/Hrc2DimList |	Conversion between hierarchy coding in sdcTable and TauArgus |
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
| RbindAll |	Combining several data frames when the columns don't match |
| RowGroups |	Create numbering according to unique rows |
| SSBtoolsData |	Function that returns a dataset |
| Stack |	Stack columns from a data frame and include variables. |
| Unstack |	Unstack a column from a data frame and include additional variables. |
| WildcardGlobbing |	Row selection by wildcard/globbing |
| WildcardGlobbingVector |	Selection of elements by wildcard/globbing |