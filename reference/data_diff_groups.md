# Add diff_groups results as columns in a data frame

`data_diff_groups()` is a wrapper around
[`diff_groups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/diff_groups.md)
that runs the same analysis on two variables in a data frame and adds
selected results back as new columns.

## Usage

``` r
data_diff_groups(
  data,
  input_vars,
  output_vars = c(is_common = "is_common", diff_1_2 = "diff_1_2", diff_2_1 = "diff_2_1",
    sum_1_2 = "sum_1_2", sum_2_1 = "sum_2_1"),
  ...
)
```

## Arguments

- data:

  A data frame containing the variables to be compared.

- input_vars:

  Character vector of length two specifying the names of the two
  variables in `data` to be compared.

- output_vars:

  Named character vector defining which variables from the group results
  are added to `data`, and what their names will be in the output.

- ...:

  Additional arguments passed to
  [`diff_groups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/diff_groups.md).

## Value

A data frame identical to `data`, but with additional variables
describing relationships between the two specified columns.

## Examples

``` r
df <- cbind(v1 = 1, SSBtoolsData("code_pairs"), v4 = 4)

data_diff_groups(df, input_vars = c("code_1", "code_2"))
#>    v1 code_1 code_2 v4 is_common  diff_1_2 diff_2_1   sum_1_2       sum_2_1
#> 1   1      d      O  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 2   1      a      N  4     FALSE      <NA>    N_-_c      <NA>          <NA>
#> 3   1      f      S  4     FALSE f_-_Q_-_R    S_-_b      <NA>          <NA>
#> 4   1      b      S  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 5   1      d      N  4     FALSE     d_-_O    N_-_c      <NA>          <NA>
#> 6   1      j      V  4     FALSE      <NA>     <NA>      <NA> V_=_h_+_i_+_j
#> 7   1      g      U  4     FALSE      <NA>     <NA> g_=_T_+_U          <NA>
#> 8   1      h      V  4     FALSE      <NA>     <NA>      <NA> V_=_h_+_i_+_j
#> 9   1      d      O  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 10  1      d      N  4     FALSE     d_-_O    N_-_c      <NA>          <NA>
#> 11  1      g      T  4     FALSE      <NA>     <NA> g_=_T_+_U          <NA>
#> 12  1      f      R  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 13  1      a      S  4     FALSE      <NA>    S_-_b      <NA>          <NA>
#> 14  1      c      N  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 15  1      e      P  4      TRUE      <NA>     <NA>      <NA>          <NA>
#> 16  1      a      N  4     FALSE      <NA>    N_-_c      <NA>          <NA>
#> 17  1      e      P  4      TRUE      <NA>     <NA>      <NA>          <NA>
#> 18  1      d      N  4     FALSE     d_-_O    N_-_c      <NA>          <NA>
#> 19  1      f      S  4     FALSE f_-_Q_-_R    S_-_b      <NA>          <NA>
#> 20  1      f      Q  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 21  1      i      V  4     FALSE      <NA>     <NA>      <NA> V_=_h_+_i_+_j
#> 22  1      i      V  4     FALSE      <NA>     <NA>      <NA> V_=_h_+_i_+_j
#> 23  1      c      N  4     FALSE      <NA>     <NA>      <NA>          <NA>
#> 24  1      a      S  4     FALSE      <NA>    S_-_b      <NA>          <NA>
#> 25  1      a      N  4     FALSE      <NA>    N_-_c      <NA>          <NA>
#> 26  1      a      S  4     FALSE      <NA>    S_-_b      <NA>          <NA>
```
