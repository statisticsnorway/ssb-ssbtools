# Filter a List of Items or Retrieve Names by a Variable

Filters a list of items, retaining only those associated with a specific
variable, or retrieves the names of items associated with the variable.
The association between items and variables is provided via a named
list, where each element contains a vector of variables corresponding to
an item in `items`.

## Usage

``` r
filter_by_variable(variable, items, variable_mapping)

names_by_variable(variable, variable_mapping)
```

## Arguments

- variable:

  A character string. The variable to filter the items by.

- items:

  A named list of elements. These can be any type of objects (e.g.,
  formulas, data, etc.).

- variable_mapping:

  A named list. Each element is a character vector of variables
  associated with the corresponding item in `items`. The names of the
  list in `variable_mapping` should match the names of the list in
  `items`.

## Value

- `filter_by_variable()`: A named list containing a subset of `items`
  where each element is associated with the specified `variable`. If no
  matches are found, an empty list is returned.

- `names_by_variable()`: A character vector of names from
  `variable_mapping` that are associated with the specified `variable`.
  If no matches are found, an empty character vector is returned.

## Details

`filter_by_variable()` returns the filtered list of items, whereas
`names_by_variable()` is a simpler function that just returns the names
of the items.

## Note

This function is written and documented by ChatGPT after some
discussion. The examples have been chosen to be relevant in connection
with the
[`tables_by_formulas`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.md)
function.

## Examples

``` r
items <- list(
  table_1 = ~region * sector2, 
  table_2 = ~region1:sector4 - 1, 
  table_3 = ~region + sector4 - 1
)

variable_mapping <- list(
  table_3 = c("z", "y"), 
  table_1 = c("value", "x"), 
  table_2 = c("value", "x", "y")
)

filter_by_variable("value", items, variable_mapping)
#> $table_1
#> ~region * sector2
#> <environment: 0x55c2d4a0ff60>
#> 
#> $table_2
#> ~region1:sector4 - 1
#> <environment: 0x55c2d4a0ff60>
#> 
filter_by_variable("y", items, variable_mapping)
#> $table_2
#> ~region1:sector4 - 1
#> <environment: 0x55c2d4a0ff60>
#> 
#> $table_3
#> ~region + sector4 - 1
#> <environment: 0x55c2d4a0ff60>
#> 
filter_by_variable("nonexistent", items, variable_mapping)
#> named list()

names_by_variable("value", variable_mapping)
#> [1] "table_1" "table_2"
names_by_variable("y", variable_mapping)
#> [1] "table_3" "table_2"
names_by_variable("nonexistent", variable_mapping)
#> character(0)
```
