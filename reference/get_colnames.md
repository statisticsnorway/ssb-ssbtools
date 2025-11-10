# Get column names from a data.frame, tibble, or data.table

This helper function returns column names based on either column indices
(numeric) or column names (character). It works consistently across
`data.frame`, `tibble`, and `data.table` objects.

## Usage

``` r
get_colnames(data, cols, preserve_duplicates = FALSE, preserve_NULL = FALSE)
```

## Arguments

- data:

  A data frame, tibble, or data.table.

- cols:

  Column selection, either as numeric indices, character names, or
  `NULL`.

- preserve_duplicates:

  Logical, default `FALSE`. If `TRUE`, duplicates and order in `cols`
  are preserved. If `FALSE`, duplicates are removed while preserving
  order of first appearance.

- preserve_NULL:

  Logical, default `FALSE`. If `TRUE`, returns `NULL` when
  `cols = NULL`. If `FALSE`, returns `character(0)` when `cols = NULL`.

## Value

A character vector of column names, or `NULL` if `cols = NULL` and
`preserve_NULL = TRUE`.

## Details

By default, `cols = NULL` returns `character(0)`, matching the behavior
of `names(data[1, NULL, drop = FALSE])`. If `preserve_NULL = TRUE`, the
function instead returns `NULL`.

## Note

This function is written and documented by ChatGPT after some
discussion.

## Examples

``` r
df <- data.frame(a = 1, b = 2, c = 3)

# NULL input handling
get_colnames(df, NULL)
#> character(0)
get_colnames(df, NULL, preserve_NULL = TRUE)
#> NULL

# Numeric input
get_colnames(df, c(2, 2, 1))
#> [1] "b" "a"
get_colnames(df, c(2, 2, 1), preserve_duplicates = TRUE)
#> [1] "b" "b" "a"

# Character input
get_colnames(df, c("c", "a", "c"))
#> [1] "c" "a"
get_colnames(df, c("c", "a", "c"), preserve_duplicates = TRUE)
#> [1] "c" "a" "c"
```
