# Finding dimList

Finding lists of level-hierarchy as needed for the input parameter
dimList to the function makeProblem in package sdcTable

## Usage

``` r
FindDimLists(
  x,
  groupVarInd = HierarchicalGroups(x = x),
  addName = FALSE,
  sep = ".",
  xReturn = FALSE,
  total = "Total"
)
```

## Arguments

- x:

  Matrix or data frame containing the variables (micro data or cell
  counts data).

- groupVarInd:

  List of vectors of indices defining the hierarchical variable groups.

- addName:

  When TRUE the variable name is added to the level names, except for
  variables with most levels.

- sep:

  A character string to separate when addName apply.

- xReturn:

  When TRUE x is also in output, possibly changed according to addName.

- total:

  String used to name totals. A vector of length `ncol(x)` is also
  possible (see examples).

## Value

Output is a list according to the specifications in sdcTable. When
xReturn is TRUE output has an extra list level and x is the first
element.

## Author

Øyvind Langsrud

## Examples

``` r
dataset <- SSBtoolsData("example1")
FindDimLists(dataset[1:2])
#> $age
#>   levels codes
#> 1      @ Total
#> 2     @@   old
#> 3     @@ young
#> 
#> $geo
#>   levels    codes
#> 1      @    Total
#> 2     @@  Iceland
#> 3     @@ Portugal
#> 4     @@    Spain
#> 
FindDimLists(dataset[2:3])
#> $geo
#>   levels    codes
#> 1      @    Total
#> 2     @@       EU
#> 3    @@@ Portugal
#> 4    @@@    Spain
#> 5     @@    nonEU
#> 6    @@@  Iceland
#> 
FindDimLists(dataset[1:4])
#> $age
#>   levels codes
#> 1      @ Total
#> 2     @@   old
#> 3     @@ young
#> 
#> $geo
#>   levels    codes
#> 1      @    Total
#> 2     @@       EU
#> 3    @@@ Portugal
#> 4    @@@    Spain
#> 5     @@    nonEU
#> 6    @@@  Iceland
#> 
#> $year
#>   levels codes
#> 1      @ Total
#> 2     @@  2014
#> 3     @@  2015
#> 4     @@  2016
#> 

FindDimLists(SSBtoolsData("magnitude1")[1:4], 
                total = c("TOTAL", "unused1", "Europe", "unused2"))
#> $sector4
#>   levels         codes
#> 1      @         TOTAL
#> 2     @@       private
#> 3    @@@   Agriculture
#> 4    @@@ Entertainment
#> 5    @@@      Industry
#> 6     @@        public
#> 7    @@@  Governmental
#> 
#> $geo
#>   levels    codes
#> 1      @   Europe
#> 2     @@       EU
#> 3    @@@ Portugal
#> 4    @@@    Spain
#> 5     @@    nonEU
#> 6    @@@  Iceland
#> 
                
 x <- rep(c('A','B','C'),3)
 y <- rep(c(11,22,11),3)
 z <- c(1,1,1,2,2,2,3,3,3)
 zy <- paste(z,y,sep='')
 m <- cbind(x,y,z,zy)
 FindDimLists(m)
#> $x
#>   levels codes
#> 1      @ Total
#> 2     @@    11
#> 3    @@@     A
#> 4    @@@     C
#> 5     @@    22
#> 6    @@@     B
#> 
#> $zy
#>   levels codes
#> 1      @ Total
#> 2     @@    11
#> 3    @@@   111
#> 4    @@@   211
#> 5    @@@   311
#> 6     @@    22
#> 7    @@@   122
#> 8    @@@   222
#> 9    @@@   322
#> 
#> $zy
#>    levels codes
#> 1       @ Total
#> 2      @@     1
#> 3     @@@   111
#> 4     @@@   122
#> 5      @@     2
#> 6     @@@   211
#> 7     @@@   222
#> 8      @@     3
#> 9     @@@   311
#> 10    @@@   322
#> 
 FindDimLists(m, total = paste0("A", 1:4))
#> $x
#>   levels codes
#> 1      @    A1
#> 2     @@    11
#> 3    @@@     A
#> 4    @@@     C
#> 5     @@    22
#> 6    @@@     B
#> 
#> $zy
#>   levels codes
#> 1      @    A4
#> 2     @@    11
#> 3    @@@   111
#> 4    @@@   211
#> 5    @@@   311
#> 6     @@    22
#> 7    @@@   122
#> 8    @@@   222
#> 9    @@@   322
#> 
#> $zy
#>    levels codes
#> 1       @    A4
#> 2      @@     1
#> 3     @@@   111
#> 4     @@@   122
#> 5      @@     2
#> 6     @@@   211
#> 7     @@@   222
#> 8      @@     3
#> 9     @@@   311
#> 10    @@@   322
#> 
```
