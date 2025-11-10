# Combining columns of a matrix

Combining columns of a matrix

## Usage

``` r
MatrixPaste(x, sep = "_", forceCharacter = FALSE, stringEmpty = " ")

MatrixPaste1(x, stringEmpty = "1")
```

## Arguments

- x:

  Matrix or vector

- sep:

  String used to combine columns

- forceCharacter:

  When FALSE single column input will keep to original class in output.

- stringEmpty:

  String used when input is empty (can be set to NULL)

## Value

Character vector or possibly same vector as input

## Details

Each row in input will be combined to a single string using sep.

## Author

Øyvind Langsrud

## Examples

``` r
if (FALSE) { # \dontrun{
MatrixPaste(matrix(1:12,3,4))
MatrixPaste(1:5)
MatrixPaste(1:5, forceCharacter=TRUE)
MatrixPaste(matrix(integer(0),3,0))
MatrixPaste(NULL)
} # }
```
