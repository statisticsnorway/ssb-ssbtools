# Limit matrix or data frame to selected model terms

For use with output from
[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
or data frames derived from such output. It is a generic function which
means that methods for other input objects can be added.

## Usage

``` r
# Default S3 method
FormulaSelection(x, formula, intercept = NA, logical = FALSE)

FormulaSelection(x, formula, intercept = NA, logical = FALSE)

formula_selection(x, formula, intercept = NA, logical = FALSE)
```

## Arguments

- x:

  Model matrix or a data frame

- formula:

  Formula representing the limitation or character string(s) to be
  converted to a formula (see details)

- intercept:

  Parameter that specifies whether a possible intercept term (overall
  total) should be included in the output. Default is `TRUE` when a
  formula is input. Otherwise, see details.

- logical:

  When `TRUE`, the logical selection vector is returned.

## Value

Limited model matrix or a data frame

## Details

The selection is based on `startCol` or `startRow` attribute in input
`x`.

With **formula as character**:

- **`~`** is included: Input is converted by `as.formula` and default
  intercept is `TRUE`.

- **`~`** is not included: Internally, input data is converted to a
  formula by adding `~` and possibly `+`'s when the length is `>1`.
  Default intercept is `FALSE` unless `"1"` or `"(Intercept)"` (is
  changed internally to `"1"`) is included.

## Note

`formula_selection` and `FormulaSelection` are identical

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"

x <- ModelMatrix(z, formula = ~age * year)

FormulaSelection(x, "age")
#> 18 x 2 sparse Matrix of class "dgCMatrix"
#>       old-Total young-Total
#>  [1,]         .           1
#>  [2,]         .           1
#>  [3,]         .           1
#>  [4,]         1           .
#>  [5,]         1           .
#>  [6,]         1           .
#>  [7,]         .           1
#>  [8,]         .           1
#>  [9,]         .           1
#> [10,]         1           .
#> [11,]         1           .
#> [12,]         1           .
#> [13,]         .           1
#> [14,]         .           1
#> [15,]         .           1
#> [16,]         1           .
#> [17,]         1           .
#> [18,]         1           .
FormulaSelection(x, ~year)
#> 18 x 4 sparse Matrix of class "dgCMatrix"
#>       Total-Total Total-2014 Total-2015 Total-2016
#>  [1,]           1          1          .          .
#>  [2,]           1          1          .          .
#>  [3,]           1          1          .          .
#>  [4,]           1          1          .          .
#>  [5,]           1          1          .          .
#>  [6,]           1          1          .          .
#>  [7,]           1          .          1          .
#>  [8,]           1          .          1          .
#>  [9,]           1          .          1          .
#> [10,]           1          .          1          .
#> [11,]           1          .          1          .
#> [12,]           1          .          1          .
#> [13,]           1          .          .          1
#> [14,]           1          .          .          1
#> [15,]           1          .          .          1
#> [16,]           1          .          .          1
#> [17,]           1          .          .          1
#> [18,]           1          .          .          1
FormulaSelection(x, ~year:age)
#> 18 x 7 sparse Matrix of class "dgCMatrix"
#>       Total-Total old-2014 old-2015 old-2016 young-2014 young-2015 young-2016
#>  [1,]           1        .        .        .          1          .          .
#>  [2,]           1        .        .        .          1          .          .
#>  [3,]           1        .        .        .          1          .          .
#>  [4,]           1        1        .        .          .          .          .
#>  [5,]           1        1        .        .          .          .          .
#>  [6,]           1        1        .        .          .          .          .
#>  [7,]           1        .        .        .          .          1          .
#>  [8,]           1        .        .        .          .          1          .
#>  [9,]           1        .        .        .          .          1          .
#> [10,]           1        .        1        .          .          .          .
#> [11,]           1        .        1        .          .          .          .
#> [12,]           1        .        1        .          .          .          .
#> [13,]           1        .        .        .          .          .          1
#> [14,]           1        .        .        .          .          .          1
#> [15,]           1        .        .        .          .          .          1
#> [16,]           1        .        .        1          .          .          .
#> [17,]           1        .        .        1          .          .          .
#> [18,]           1        .        .        1          .          .          .

# x1, x2, x3, x4 and x4 are identical
x1 <- FormulaSelection(x, ~age)
x2 <- FormulaSelection(x, "~age")
x3 <- FormulaSelection(x, "age", intercept = TRUE)
x4 <- FormulaSelection(x, c("1", "age"))
x5 <- FormulaSelection(x, c("(Intercept)", "age"))


a <- ModelMatrix(z, formula = ~age * geo + year, crossTable = TRUE)
b <- cbind(as.data.frame(a$crossTable), 
           sum = (Matrix::t(a$modelMatrix) %*% z$ths_per)[, 1], 
           max = DummyApply(a$modelMatrix, 
           z$ths_per, max))
rownames(b) <- NULL
attr(b, "startRow") <- attr(a$modelMatrix, "startCol", exact = TRUE)

FormulaSelection(b, ~geo * age)
#>      age      geo  year   sum   max
#> 1  Total    Total Total 680.8 122.1
#> 2    old    Total Total 437.3 122.1
#> 3  young    Total Total 243.5  69.1
#> 4  Total  Iceland Total  10.6   1.9
#> 5  Total Portugal Total 108.8  25.8
#> 6  Total    Spain Total 561.4 122.1
#> 10   old  Iceland Total   5.0   1.9
#> 11   old Portugal Total  70.3  25.8
#> 12   old    Spain Total 362.0 122.1
#> 13 young  Iceland Total   5.6   1.9
#> 14 young Portugal Total  38.5  14.2
#> 15 young    Spain Total 199.4  69.1
FormulaSelection(b, "age:geo")
#>      age      geo  year   sum   max
#> 10   old  Iceland Total   5.0   1.9
#> 11   old Portugal Total  70.3  25.8
#> 12   old    Spain Total 362.0 122.1
#> 13 young  Iceland Total   5.6   1.9
#> 14 young Portugal Total  38.5  14.2
#> 15 young    Spain Total 199.4  69.1
FormulaSelection(b, ~year - 1)
#>     age   geo year   sum   max
#> 7 Total Total 2014 222.3 120.3
#> 8 Total Total 2015 225.0 119.6
#> 9 Total Total 2016 233.5 122.1
FormulaSelection(b, ~geo:age, logical = TRUE)
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
#> [13]  TRUE  TRUE  TRUE
```
