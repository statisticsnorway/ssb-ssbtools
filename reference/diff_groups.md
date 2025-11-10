# Difference and Sum Groups

This function is a wrapper around
[`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md)
for the specific case where the input contains two columns. It calls
[`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md)
with `returnGroups = TRUE`, and extends the resulting data frame of
unique code combinations with additional information about common
groups, difference groups, and sum groups.

## Usage

``` r
diff_groups(
  x,
  ...,
  hiddenNA = TRUE,
  sep_common = "_=_",
  sep_diff = "_-_",
  sep_sum = c("_=_", "_+_"),
  outputNA = "NA",
  diff_extra = FALSE
)
```

## Arguments

- x:

  A data frame with exactly two columns.

- ...:

  Additional arguments passed to
  [`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md).

- hiddenNA:

  Logical. When `TRUE` (default), missing codes (`NA`) are treated as
  hidden categories — they are not available for computing difference
  and sum groups. See *Note* for details on how this differs from the
  `NAomit` parameter in
  [`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md).

- sep_common:

  A character string used in the `common` column to separate codes that
  are identical across the two input columns.

- sep_diff:

  A character string used in the `diff_1_2` and `diff_2_1` columns to
  indicate difference groups. The first column contains the parent code,
  and one or more child codes from the other column are subtracted.

- sep_sum:

  A character vector of one or two elements used in the `sum_1_2` and
  `sum_2_1` columns to describe relationships where a code in one column
  represents the sum of several codes in the other. The first element
  (`sep_sum[1]`) acts as an equality sign, and the second element
  (`sep_sum[2]`) acts as a plus sign. If `sep_sum` has length 1, the
  same value is used for both positions.

- outputNA:

  Character string used to represent `NA` values within the newly
  constructed text strings in the additional output columns. Only
  relevant when `hiddenNA = FALSE`.

- diff_extra:

  Logical. When TRUE, additional difference-group variables are returned
  when found.

## Value

A list (as returned by
[`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md)),
where the `groups` data frame is extended with additional descriptive
columns indicating common, difference, and sum relationships between the
two code columns.

## Details

The returned list contains the same elements as from
[`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md),
but with an extended `groups` data frame. The columns describe
relationships between the two input columns as follows:

- **is_common** — `TRUE` when the two codes on the row are identical.

- **is_child_1**, **is_child_2** — `TRUE` when the code in the column is
  a subset or subgroup of a code in the other column.

- **common** — identical code pairs, formatted using `sep_common`.

- **diff_1_2**, **diff_2_1** — difference groups. The first element is
  the parent from the source column, followed by one or more child codes
  from the opposite column, joined using `sep_diff`.

- **sum_1_2**, **sum_2_1** — sum groups where a parent code in one
  column equals the sum of several codes in the other.

## Note

The parameter `NAomit` from
[`RowGroups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.md)
can still be set via `...`, but using it will remove rows containing
`NA` before processing. The relationships found will then reflect the
reduced data, which is usually not the intended behaviour when
identifying relationships between code sets.

## See also

[`data_diff_groups()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/data_diff_groups.md)
for adding the results back as new columns in the data frame.

## Examples

``` r
df <- SSBtoolsData("code_pairs")

df
#>    code_1 code_2
#> 1       d      O
#> 2       a      N
#> 3       f      S
#> 4       b      S
#> 5       d      N
#> 6       j      V
#> 7       g      U
#> 8       h      V
#> 9       d      O
#> 10      d      N
#> 11      g      T
#> 12      f      R
#> 13      a      S
#> 14      c      N
#> 15      e      P
#> 16      a      N
#> 17      e      P
#> 18      d      N
#> 19      f      S
#> 20      f      Q
#> 21      i      V
#> 22      i      V
#> 23      c      N
#> 24      a      S
#> 25      a      N
#> 26      a      S

diff_groups(df)
#> $idx
#>  [1]  6  1 10  3  5 15 12 13  6  5 11  9  2  4  7  1  7  5 10  8 14 14  4  2  1
#> [26]  2
#> 
#> $groups
#>    code_1 code_2 is_common is_child_1 is_child_2 common  diff_1_2 diff_2_1
#> 1       a      N     FALSE      FALSE      FALSE   <NA>      <NA>    N_-_c
#> 2       a      S     FALSE      FALSE      FALSE   <NA>      <NA>    S_-_b
#> 3       b      S     FALSE       TRUE      FALSE   <NA>      <NA>     <NA>
#> 4       c      N     FALSE       TRUE      FALSE   <NA>      <NA>     <NA>
#> 5       d      N     FALSE      FALSE      FALSE   <NA>     d_-_O    N_-_c
#> 6       d      O     FALSE      FALSE       TRUE   <NA>      <NA>     <NA>
#> 7       e      P      TRUE      FALSE      FALSE  e_=_P      <NA>     <NA>
#> 8       f      Q     FALSE      FALSE       TRUE   <NA>      <NA>     <NA>
#> 9       f      R     FALSE      FALSE       TRUE   <NA>      <NA>     <NA>
#> 10      f      S     FALSE      FALSE      FALSE   <NA> f_-_Q_-_R    S_-_b
#> 11      g      T     FALSE      FALSE       TRUE   <NA>      <NA>     <NA>
#> 12      g      U     FALSE      FALSE       TRUE   <NA>      <NA>     <NA>
#> 13      h      V     FALSE       TRUE      FALSE   <NA>      <NA>     <NA>
#> 14      i      V     FALSE       TRUE      FALSE   <NA>      <NA>     <NA>
#> 15      j      V     FALSE       TRUE      FALSE   <NA>      <NA>     <NA>
#>      sum_1_2       sum_2_1
#> 1       <NA>          <NA>
#> 2       <NA>          <NA>
#> 3       <NA>          <NA>
#> 4       <NA>          <NA>
#> 5       <NA>          <NA>
#> 6       <NA>          <NA>
#> 7       <NA>          <NA>
#> 8       <NA>          <NA>
#> 9       <NA>          <NA>
#> 10      <NA>          <NA>
#> 11 g_=_T_+_U          <NA>
#> 12 g_=_T_+_U          <NA>
#> 13      <NA> V_=_h_+_i_+_j
#> 14      <NA> V_=_h_+_i_+_j
#> 15      <NA> V_=_h_+_i_+_j
#> 


d2 <- SSBtoolsData("d2")
diff_groups(d2[1:2])$groups
#>    region county is_common is_child_1 is_child_2 common diff_1_2 diff_2_1
#> 1       A      1     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 2       B      4      TRUE      FALSE      FALSE  B_=_4     <NA>       NA
#> 3       C      5     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 4       D      5     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 5       E      6     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 6       F      6     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 7       G      8     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 8       H      8     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 9       I      1     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 10      J     10     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#> 11      K     10     FALSE       TRUE      FALSE   <NA>     <NA>       NA
#>    sum_1_2    sum_2_1
#> 1     <NA>  1_=_A_+_I
#> 2     <NA>       <NA>
#> 3     <NA>  5_=_C_+_D
#> 4     <NA>  5_=_C_+_D
#> 5     <NA>  6_=_E_+_F
#> 6     <NA>  6_=_E_+_F
#> 7     <NA>  8_=_G_+_H
#> 8     <NA>  8_=_G_+_H
#> 9     <NA>  1_=_A_+_I
#> 10    <NA> 10_=_J_+_K
#> 11    <NA> 10_=_J_+_K
diff_groups(d2[2:3])$groups
#>   county k_group is_common is_child_1 is_child_2 common diff_1_2
#> 1      1     300     FALSE      FALSE      FALSE   <NA>       NA
#> 2      1     400     FALSE      FALSE      FALSE   <NA>       NA
#> 3      4     300     FALSE       TRUE      FALSE   <NA>       NA
#> 4      5     300     FALSE       TRUE      FALSE   <NA>       NA
#> 5      6     300     FALSE       TRUE      FALSE   <NA>       NA
#> 6      8     300     FALSE       TRUE      FALSE   <NA>       NA
#> 7     10     400     FALSE       TRUE      FALSE   <NA>       NA
#>              diff_2_1 sum_1_2 sum_2_1
#> 1 300_-_4_-_5_-_6_-_8    <NA>    <NA>
#> 2            400_-_10    <NA>    <NA>
#> 3                <NA>    <NA>    <NA>
#> 4                <NA>    <NA>    <NA>
#> 5                <NA>    <NA>    <NA>
#> 6                <NA>    <NA>    <NA>
#> 7                <NA>    <NA>    <NA>
                           
```
