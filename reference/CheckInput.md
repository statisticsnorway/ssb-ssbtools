# Checking function inputs

An input vector (of length one unless `okSeveral` is `TRUE`) is checked.

## Usage

``` r
CheckInput(
  x,
  alt = NULL,
  min = NULL,
  max = NULL,
  type = "character",
  data = NULL,
  okSeveral = FALSE,
  okNULL = FALSE,
  okNA = FALSE,
  okDuplicates = is.null(alt) & !(type %in% c("varName", "varNr", "varNrName"))
)

check_input(
  x,
  alt = NULL,
  min = NULL,
  max = NULL,
  type = "character",
  data = NULL,
  okSeveral = FALSE,
  okNULL = FALSE,
  okNA = FALSE,
  okDuplicates = is.null(alt) & !(type %in% c("varName", "varNr", "varNrName"))
)
```

## Arguments

- x:

  Input vector to be checked

- alt:

  `NULL` or vector of allowed values

- min:

  `NULL` or minimum value (when `type` is numeric or integer)

- max:

  `NULL` or maximum value (when `type` is numeric or integer)

- type:

  One of: `"character"`, `"numeric"`, `"integer"`, `"logical"`,
  `"varName"`, `"varNr"`, `"varNrName"`. numeric/integer is not checked
  against exact class, but whether the value fit into the class. Also
  see data below.

- data:

  A data frame or matrix. When above type is `varNames`, `x` is checked
  against `colnames(data)`. When type is `varNr`, `x` is checked against
  column numbers. When type is `varNrName`, `x` can be either column
  numbers or column names.

- okSeveral:

  When `TRUE`, `length(x)>1` is allowed

- okNULL:

  When `TRUE`, `NULL` is allowed

- okNA:

  When `TRUE`, `NA` is allowed

- okDuplicates:

  When `TRUE`, duplicated values are allowed. Default is `TRUE` if `alt`
  is `NULL` and if `type` does not refer to column(s) of `data`.

## Details

`x` is checked according to the other input parameters. When `x` is
wrong an error is produced with appropriate text.

*The function was originally created in 2016 and has been included in
internal packages at Statistics Norway (SSB). Due to its widespread use,
it was beneficial to include it in this CRAN package.*

## Note

`check_input` and `CheckInput` are identical

## Author

Øyvind Langsrud

## Examples

``` r
a <- c("no", "yes")
b <- c(3.14, 4, 5)
z <- data.frame(A = a, B = b[1:2], C = TRUE)

# Lines causing error are embedded in 'try'

try(CheckInput(a, type = "character"))
#> Error in CheckInput(a, type = "character") : 
#>   a : Input must be of length 1
CheckInput(a, type = "character", alt = c("no", "yes", "dontknow"), okSeveral = TRUE)
#> NULL
try(CheckInput("yesno", type = "character", alt = c("no", "yes", "dontknow")))
#> Error in CheckInput("yesno", type = "character", alt = c("no", "yes",  : 
#>   "yesno" : Input must be in: no, yes, dontknow
CheckInput(a[1], type = "character", alt = c("no", "yes", "dontknow"))
#> NULL

try(CheckInput(b, type = "integer", max = 100, okSeveral = TRUE))
#> Error in CheckInput(b, type = "integer", max = 100, okSeveral = TRUE) : 
#>   b : Input is not in accordance with integer
try(CheckInput(b, type = "numeric", min = 4, okSeveral = TRUE))
#> Error in CheckInput(b, type = "numeric", min = 4, okSeveral = TRUE) : 
#>   b : Input must be within the interval [ 4 , Inf ]
CheckInput(b, type = "numeric", max = 100, okSeveral = TRUE)
#> NULL
try(CheckInput(b, type = "numeric", alt = 1:10, okSeveral = TRUE))
#> Error in CheckInput(b, type = "numeric", alt = 1:10, okSeveral = TRUE) : 
#>   b : Input must be in: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
CheckInput(b[2], type = "numeric", alt = 1:10)
#> NULL

try(CheckInput("TRUE", type = "logical"))
#> Error in CheckInput("TRUE", type = "logical") : 
#>   "TRUE" : Input must be logical
CheckInput(TRUE, type = "logical")
#> NULL

try(CheckInput("A", type = "varName"))
#> Error in CheckInput("A", type = "varName") : "A" : data missing
CheckInput("A", type = "varName", data = z)
#> NULL
CheckInput(c("A", "B"), type = "varNrName", data = z, okSeveral = TRUE)
#> NULL
try(CheckInput("ABC", type = "varNrName", data = z))
#> Error in CheckInput("ABC", type = "varNrName", data = z) : 
#>   "ABC" : Input must be in: A, B, C
try(CheckInput(5, type = "varNrName", data = z))
#> Error in CheckInput(5, type = "varNrName", data = z) : 
#>   5 : Input must be within the interval [ 1 , 3 ]
CheckInput(3, type = "varNr", data = z)
#> NULL
CheckInput(2:3, type = "varNr", data = z, okSeveral = TRUE)
#> NULL
```
