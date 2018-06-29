# R package SSBtools

Tools used by other packages from Statistics Norway

```r
library(devtools)                           # Load package containing install_github
install_github("statisticsnorway/SSBtools") # Install SSBtools from GitHub
library(SSBtools)                           # Load SSBtools
?HierarchyCompute                           # Help documentation of function HierarchyCompute
```

 Function        |   |
| ---------------------------- | -------------------------------------------------------------- |
| AddLeadingZeros | Add leading zeros to numbers while preserving other text |
| AutoSplit	| Creating variables by splitting the elements of a character vector without needing a split string |
| CbindIdMatch |	Combine several data frames by using id variables to match rows |
| DummyHierarchy |	Converting hierarchy specifications to a (signed) dummy matrix |
| FactorLevCorr |	Factor level correlation |
| FindCommonCells |	Finding commonCells |
| FindDimLists |	Finding dimList |
| FindTableGroup |	Finding table(s) of hierarchical variable groups |
| HierarchicalGroups |	Finding hierarchical variable groups |
| HierarchicalWildcardGlobbing |	Find variable combinations by advanced wildcard/globbing specifications. |
| HierarchyCompute |	Hierarchical Computations |
| HierarchyFix |	Change the hierarchy table to follow the standard |
| Match |	Matching rows in data frames |
| matlabColon |	Simulate Matlab's ':' |
| Number |	Adding leading zeros |
| RbindAll |	Combining several data frames when the columns don't match |
| RowGroups |	Create numbering according to unique rows |
| SSBtoolsData |	Function that returns a dataset |
| Stack |	Stack columns from a data frame and include variables. |
| Unstack |	Unstack a column from a data frame and include additional variables. |
| WildcardGlobbing |	Row selection by wildcard/globbing |
| WildcardGlobbingVector |	Selection of elements by wildcard/globbing |