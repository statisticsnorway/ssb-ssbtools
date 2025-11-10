# Unstack a column from a data frame and include additional variables.

Unstack a column from a data frame and include additional variables.

## Usage

``` r
Unstack(
  data,
  mainVar = 1,
  stackVar = (1:NCOL(data))[-mainVar],
  extraVar = integer(0),
  blockVar = integer(0),
  sep = "_",
  returnRowData = TRUE,
  sorted = FALSE
)
```

## Arguments

- data:

  A data frame

- mainVar:

  Index of the variable to be unstacked

- stackVar:

  Index of variables defining the unstack grouping

- extraVar:

  Indices of within-replicated variables to be added to the rowData
  output

- blockVar:

  Indices of between-replicated variables to be added to the data output

- sep:

  A character string to separate when creating variable names

- returnRowData:

  When FALSE output is no list, but only data

- sorted:

  When TRUE the created variables is in sorted order. Otherwise input
  order is used.

## Value

When returnRowData=TRUE output is list of two elements.

- data:

  Unstacked data

- rowData:

  A separate data frame with one row for each unstack grouping composed
  of the stackVar variables

## See also

[`Stack`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Stack.md)
(examples)

## Author

Øyvind Langsrud
