# Find directly disclosive cells

Function for determining which cells in a frequency table can lead to
direct disclosure of an identifiable individual, assuming an attacker
has the background knowledge to place themselves (or a coalition) in the
table.

## Usage

``` r
FindDisclosiveCells(
  data,
  freq,
  crossTable,
  primaryDims = names(crossTable),
  unknowns = rep(NA, length(primaryDims)),
  total = rep("Total", length(primaryDims)),
  unknown.threshold = 0,
  coalition = 1,
  suppressSmallCells = FALSE,
  ...
)
```

## Arguments

- data:

  the data set

- freq:

  vector containing frequencies

- crossTable:

  cross table of key variables produced by ModelMatrix in parent
  function

- primaryDims:

  dimensions to be considered for direct disclosure.

- unknowns:

  vector of unknown values for each of the primary dimensions. If a
  primary dimension does not contain unknown values, NA should be
  passed.

- total:

  string name for marginal values

- unknown.threshold:

  numeric for specifying a percentage for calculating safety of cells. A
  cell is "safe" in a row if the number of unknowns exceeds
  `unknown.threshold` percent of the row total.

- coalition:

  maximum number of units in a possible coalition, default 1

- suppressSmallCells:

  logical variable which determines whether small cells (\<= coalition)
  or large cells should be suppressed. Default FALSE.

- ...:

  parameters from main suppression method

## Value

list with two named elements, the first (\$primary) being a logical
vector marking directly disclosive cells, the second (\$numExtra) a
data.frame containing information regarding the dimensions in which the
cells are directly disclosive.

## Details

This function does not work on data containing hierarchical variables.

## Examples

``` r
extable <- data.frame(v1 = rep(c('a', 'b', 'c'), times = 4),
            v2 = c('i','i', 'i','h','h','h','i','i','i','h','h','h'),
            v3 = c('y', 'y', 'y', 'y', 'y', 'y','z','z', 'z', 'z', 'z', 'z'),
            freq = c(0,0,5,0,2,3,1,0,3,1,1,2))
ex_freq <- c(18,10,8,9,5,4,9,5,4,2,0,2,1,0,1,1,0,1,3,2,1,3,2,1,0,0,0,13,8,5,
             5,3,2,8,5,3)
cross <- ModelMatrix(extable,
                     dimVar = 1:3,
                     crossTable = TRUE)$crossTable

FindDisclosiveCells(extable, ex_freq, cross) 
#> $primary
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
#> [13]  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
#> [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
#> 
#> $numExtra
#>    v1-prikk v2-prikk v3-prikk
#> 1     FALSE    FALSE    FALSE
#> 2     FALSE    FALSE    FALSE
#> 3     FALSE    FALSE    FALSE
#> 4     FALSE    FALSE    FALSE
#> 5     FALSE    FALSE    FALSE
#> 6     FALSE    FALSE    FALSE
#> 7     FALSE    FALSE    FALSE
#> 8     FALSE    FALSE    FALSE
#> 9     FALSE    FALSE    FALSE
#> 10    FALSE    FALSE    FALSE
#> 11    FALSE    FALSE    FALSE
#> 12    FALSE    FALSE     TRUE
#> 13    FALSE     TRUE    FALSE
#> 14    FALSE    FALSE    FALSE
#> 15    FALSE     TRUE     TRUE
#> 16    FALSE     TRUE    FALSE
#> 17    FALSE    FALSE    FALSE
#> 18    FALSE     TRUE     TRUE
#> 19    FALSE    FALSE    FALSE
#> 20    FALSE    FALSE     TRUE
#> 21    FALSE    FALSE    FALSE
#> 22    FALSE     TRUE    FALSE
#> 23    FALSE     TRUE     TRUE
#> 24    FALSE     TRUE    FALSE
#> 25    FALSE    FALSE    FALSE
#> 26    FALSE    FALSE    FALSE
#> 27    FALSE    FALSE    FALSE
#> 28    FALSE    FALSE    FALSE
#> 29    FALSE    FALSE    FALSE
#> 30    FALSE    FALSE    FALSE
#> 31    FALSE    FALSE    FALSE
#> 32    FALSE    FALSE    FALSE
#> 33    FALSE    FALSE    FALSE
#> 34     TRUE    FALSE    FALSE
#> 35     TRUE    FALSE    FALSE
#> 36     TRUE    FALSE    FALSE
#> 
```
