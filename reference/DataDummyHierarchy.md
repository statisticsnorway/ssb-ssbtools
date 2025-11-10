# Create a (signed) dummy matrix for hierarcical mapping of codes in data

Create a (signed) dummy matrix for hierarcical mapping of codes in data

## Usage

``` r
DataDummyHierarchy(dataVector, dummyHierarchy)

DataDummyHierarchies(data, dummyHierarchies, colNamesFromData = FALSE)
```

## Arguments

- dataVector:

  A vector of codes in data

- dummyHierarchy:

  Output from
  [`DummyHierarchy`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DummyHierarchy.md)

- data:

  data

- dummyHierarchies:

  Output from
  [`DummyHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/DummyHierarchy.md)

- colNamesFromData:

  Column names from data when `TRUE`

## Value

A sparse matrix. Column names are taken from dataVector (if non-NULL)
and row names are taken from the row names of dummyHierarchy.

## Details

`DataDummyHierarchies` is a user-friendly wrapper for the original
function `DataDummyHierarchy`. When `colNamesFromData` is `FALSE`
(default), this function returns `mapply(DataDummyHierarchy,`
`data[names(dummyHierarchies)],` `dummyHierarchies)`.

## Author

Øyvind Langsrud

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")[1:9, ]
hi <- FindHierarchies(z[, c("geo", "eu", "age", "year")])
dhi <- DummyHierarchies(hi, inputInOutput = TRUE)
DataDummyHierarchies(z, dhi, colNamesFromData = TRUE)
#> $geo
#> 6 x 9 sparse Matrix of class "dgCMatrix"
#>          Spain Iceland Portugal Spain Iceland Portugal Spain Iceland Portugal
#> Iceland      .       1        .     .       1        .     .       1        .
#> Portugal     .       .        1     .       .        1     .       .        1
#> Spain        1       .        .     1       .        .     1       .        .
#> EU           1       .        1     1       .        1     1       .        1
#> nonEU        .       1        .     .       1        .     .       1        .
#> Total        1       1        1     1       1        1     1       1        1
#> 
#> $age
#> 3 x 9 sparse Matrix of class "dgCMatrix"
#>        Y15-29 Y15-29 Y15-29 Y30-64 Y30-64 Y30-64 Y15-29 Y15-29 Y15-29
#> Y15-29      1      1      1      .      .      .      1      1      1
#> Y30-64      .      .      .      1      1      1      .      .      .
#> Total       1      1      1      1      1      1      1      1      1
#> 
#> $year
#> 3 x 9 sparse Matrix of class "dgCMatrix"
#>       2014 2014 2014 2014 2014 2014 2015 2015 2015
#> 2014     1    1    1    1    1    1    .    .    .
#> 2015     .    .    .    .    .    .    1    1    1
#> Total    1    1    1    1    1    1    1    1    1
#> 
```
