# Find Major Contributions to Aggregates and Count Contributors

These functions analyze contributions to aggregates, assuming that the
aggregates are calculated using a dummy matrix with the formula:
`z = t(x) %*% y`.

## Usage

``` r
max_contribution(
  x,
  y,
  n = 1,
  id = NULL,
  output = "y",
  drop = TRUE,
  decreasing = TRUE,
  remove_fraction = NULL,
  do_abs = TRUE
)

n_contributors(x, y = rep(1L, nrow(x)), id = NULL, output = "n_contr", ...)
```

## Arguments

- x:

  A (sparse) dummy matrix

- y:

  A numeric vector of input values (contributions).

- n:

  Integer. The number of largest contributors to identify for each
  aggregate. Default is 1.

- id:

  An optional vector for grouping. When non-NULL, major contributions
  are found after aggregation within each group specified by `id`.
  Aggregates with missing `id` values are excluded.

- output:

  A character vector specifying the desired output. Possible values:

  - `"y"`: A matrix with the largest contributions in the first column,
    the second largest in the second column, and so on.

  - `"id"`: A matrix of IDs associated with the largest contributions.
    If an `id` vector is provided, it returns these IDs; otherwise, it
    returns indices.

  - `"n_contr"`: An integer vector indicating the number of contributors
    to each aggregate.

  - `"n_0_contr"`: An integer vector indicating the number of
    contributors that contribute a value of 0 to each aggregate.

  - `"n_non0_contr"`: An integer vector indicating the number of
    contributors that contribute a nonzero value to each aggregate.

  - `"sums"`: A numeric vector containing the aggregate sums of `y`.

  - `"n_contr_all"`, `"n_0_contr_all"`, `"n_non0_contr_all"`,
    `"sums_all"`: Same as the corresponding outputs above, but without
    applying the `remove_fraction` parameter.

- drop:

  Logical. If TRUE (default) and `output` has length 1, the function
  returns the single list element directly instead of a list containing
  one element.

- decreasing:

  Logical. If TRUE (default), finds the largest contributors. If FALSE,
  finds the smallest contributors.

- remove_fraction:

  A numeric vector containing values in the interval `[0, 1]`,
  specifying contributors to be removed when identifying the largest
  contributions.

  - If an `id` vector is provided, `remove_fraction` must be named
    according to the IDs of the contributors to be removed.

  - If no `id` vector is provided, the length of `remove_fraction` must
    match the length of `y`. In this case, contributors not to be
    removed should have a value of `NA` in `remove_fraction`.

  - The actual values in `remove_fraction` are used for calculating
    `"sums"` (see description above).

- do_abs:

  Logical. If TRUE (default), uses the absolute values of the summed
  contributions. The summation is performed for all contributions from
  the same contributor, within each aggregate being computed.

- ...:

  Further arguments to `max_contribution` (used by `n_contributors`).

## Value

A list or a single element, depending on the values of the `output` and
`drop` parameters.

## Details

The `max_contribution` function identifies the largest contributions to
these aggregates, while the wrapper function `n_contributors` is
designed specifically to count the number of contributors for each
aggregate.

## Examples

``` r
z <- SSBtoolsData("magnitude1")
a <- ModelMatrix(z, formula = ~sector4 + geo, crossTable = TRUE)

cbind(a$crossTable, 
      y =  max_contribution(x = a$modelMatrix, y = z$value, n = 2), 
      id = max_contribution(x = a$modelMatrix, y = z$value, n = 2, output = "id"),
      n =  n_contributors(  x = a$modelMatrix, y = z$value, n = 2))
#>         sector4      geo  y.1  y.2 id.1 id.2  n
#> 1         Total    Total 96.6 77.4    3    8 20
#> 2   Agriculture    Total 96.6 75.9    3    1  4
#> 3 Entertainment    Total 77.4 16.8    8    5  6
#> 4  Governmental    Total 21.6  6.5   11   13  4
#> 5      Industry    Total 25.7  9.6   18   15  6
#> 6         Total  Iceland 16.8  9.6    5   15  4
#> 7         Total Portugal 75.9 25.7    1   18  8
#> 8         Total    Spain 96.6 77.4    3    8  8

cbind(a$crossTable, 
      y = max_contribution(x = a$modelMatrix, y = z$value, n = 3, id = z$company), 
      id = max_contribution(a$modelMatrix, z$value, 3, id = z$company, output = "id"))
#>         sector4      geo   y.1   y.2  y.3 id.1 id.2 id.3
#> 1         Total    Total 249.9 160.0 40.1    A    B    C
#> 2   Agriculture    Total 172.5  67.7   NA    A    B <NA>
#> 3 Entertainment    Total  77.4  35.4 16.4    A    B    C
#> 4  Governmental    Total  21.6   6.5  4.7    B    C    D
#> 5      Industry    Total  35.3  17.2  5.3    B    C    D
#> 6         Total  Iceland  26.4   8.8  1.9    B    C    D
#> 7         Total Portugal  78.9  75.9  7.7    B    A    D
#> 8         Total    Spain 174.0  54.7 31.3    A    B    C

max_contribution(x = a$modelMatrix, 
                 y = z$value, 
                 n = 3, 
                 id = z$company, 
                 output = c("y", "id", "n_contr", "sums"))
#> $y
#>       [,1]  [,2] [,3]
#> [1,] 249.9 160.0 40.1
#> [2,] 172.5  67.7   NA
#> [3,]  77.4  35.4 16.4
#> [4,]  21.6   6.5  4.7
#> [5,]  35.3  17.2  5.3
#> [6,]  26.4   8.8  1.9
#> [7,]  78.9  75.9  7.7
#> [8,] 174.0  54.7 31.3
#> 
#> $id
#>      [,1] [,2] [,3]
#> [1,] "A"  "B"  "C" 
#> [2,] "A"  "B"  NA  
#> [3,] "A"  "B"  "C" 
#> [4,] "B"  "C"  "D" 
#> [5,] "B"  "C"  "D" 
#> [6,] "B"  "C"  "D" 
#> [7,] "B"  "A"  "D" 
#> [8,] "A"  "B"  "C" 
#> 
#> $n_contr
#> [1] 4 2 4 3 3 3 3 4
#> 
#> $sums
#>         Total-Total   Agriculture-Total Entertainment-Total  Governmental-Total 
#>               462.3               240.2               131.5                32.8 
#>      Industry-Total       Total-Iceland      Total-Portugal         Total-Spain 
#>                57.8                37.1               162.5               262.7 
#> 

as.data.frame(
  max_contribution(x = a$modelMatrix, 
                   y = z$value, 
                   n = 3, 
                   id = z$company, 
                   output = c("y", "id", "n_contr", "sums", "n_contr_all", "sums_all"), 
                   remove_fraction = c(B = 1)))
#>                       y.1  y.2  y.3 id.1 id.2 id.3 n_contr  sums n_contr_all
#> Total-Total         249.9 40.1 12.3    A    C    D       3 302.3           4
#> Agriculture-Total   172.5   NA   NA    A <NA> <NA>       1 172.5           2
#> Entertainment-Total  77.4 16.4  2.3    A    C    D       3  96.1           4
#> Governmental-Total    6.5  4.7   NA    C    D <NA>       2  11.2           3
#> Industry-Total       17.2  5.3   NA    C    D <NA>       2  22.5           3
#> Total-Iceland         8.8  1.9   NA    C    D <NA>       2  10.7           3
#> Total-Portugal       75.9  7.7   NA    A    D <NA>       2  83.6           3
#> Total-Spain         174.0 31.3  2.7    A    C    D       3 208.0           4
#>                     sums_all
#> Total-Total            462.3
#> Agriculture-Total      240.2
#> Entertainment-Total    131.5
#> Governmental-Total      32.8
#> Industry-Total          57.8
#> Total-Iceland           37.1
#> Total-Portugal         162.5
#> Total-Spain            262.7
```
