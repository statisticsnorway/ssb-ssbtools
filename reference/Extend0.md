# Add zero frequency rows

Microdata or tabular frequency data is extended to contain all
combinations of unique rows of (hierarchical) groups of dimensional
variables. Extra variables are extended by `NA`'s or `0`'s.

## Usage

``` r
Extend0(
  data,
  freqName = "freq",
  hierarchical = TRUE,
  varGroups = NULL,
  dimVar = NULL,
  extraVar = TRUE
)
```

## Arguments

- data:

  data frame

- freqName:

  Name of (existing) frequency variable

- hierarchical:

  Hierarchical variables treated atomatically when `TRUE`

- varGroups:

  List of variable groups, possibly with data (see details and
  examples).

- dimVar:

  The dimensional variables

- extraVar:

  Extra variables as variable names, TRUE (all remaining) or FALSE
  (none).

## Value

Extended data frame

## Details

With no frequency variable in input (microdata), the frequency variable
in output consists of ones and zeros. By default, all variables, except
the frequencies, are considered as dimensional variables. By default,
the grouping of dimensional variables is based on hierarchical
relationships (`hierarchical = TRUE`). With `varGroups = NULL` and
`hierarchical = FALSE`, each dimensional variable forms a separate group
(as `as.list(dimVar)`). Parameter `extraVar` can be specified as
variable names. `TRUE` means all remaining variables and `FALSE` no
variables.

When the contents of `varGroups[[i]]` is variable names, the data frame
`unique(data[varGroups[[i]]])` will be made as a building block within
the function. A possibility is to supply such a data frame instead of
variable names. Then, the building block will be
`unique(varGroups[[i]])`. Names and data frames can be mixed.

## See also

Advanced possibilities by varGroups-attribute. See
[`Extend0rnd1`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0rnd1.md).

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")[c(1, 4:6, 8, 11:15), ]
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"

Extend0(z[, -4])
#>      age      geo year    eu freq
#> 1  young    Spain 2014    EU    1
#> 2    old    Spain 2014    EU    1
#> 3    old  Iceland 2014 nonEU    1
#> 4    old Portugal 2014    EU    1
#> 5  young  Iceland 2015 nonEU    1
#> 6    old  Iceland 2015 nonEU    1
#> 7    old Portugal 2015    EU    1
#> 8  young    Spain 2016    EU    1
#> 9  young  Iceland 2016 nonEU    1
#> 10 young Portugal 2016    EU    1
#> 11 young  Iceland 2014 nonEU    0
#> 12 young Portugal 2014    EU    0
#> 13 young    Spain 2015    EU    0
#> 14   old    Spain 2015    EU    0
#> 15 young Portugal 2015    EU    0
#> 16   old    Spain 2016    EU    0
#> 17   old  Iceland 2016 nonEU    0
#> 18   old Portugal 2016    EU    0
Extend0(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"))
#>      age      geo year ths_per    eu freq
#> 1  young    Spain 2014    66.9    EU    1
#> 2    old    Spain 2014   120.3    EU    1
#> 3    old  Iceland 2014     1.5 nonEU    1
#> 4    old Portugal 2014    20.2    EU    1
#> 5  young  Iceland 2015     1.9 nonEU    1
#> 6    old  Iceland 2015     1.6 nonEU    1
#> 7    old Portugal 2015    24.3    EU    1
#> 8  young    Spain 2016    69.1    EU    1
#> 9  young  Iceland 2016     1.9 nonEU    1
#> 10 young Portugal 2016    12.7    EU    1
#> 11 young  Iceland <NA>     0.0    EU    0
#> 12   old  Iceland <NA>     0.0    EU    0
#> 13 young    Spain <NA>     0.0 nonEU    0
#> 14   old    Spain <NA>     0.0 nonEU    0
#> 15 young Portugal <NA>     0.0 nonEU    0
#> 16   old Portugal <NA>     0.0 nonEU    0
Extend0(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"), extraVar = "year")
#>      age      geo year    eu freq
#> 1  young    Spain 2014    EU    1
#> 2    old    Spain 2014    EU    1
#> 3    old  Iceland 2014 nonEU    1
#> 4    old Portugal 2014    EU    1
#> 5  young  Iceland 2015 nonEU    1
#> 6    old  Iceland 2015 nonEU    1
#> 7    old Portugal 2015    EU    1
#> 8  young    Spain 2016    EU    1
#> 9  young  Iceland 2016 nonEU    1
#> 10 young Portugal 2016    EU    1
#> 11 young  Iceland <NA>    EU    0
#> 12   old  Iceland <NA>    EU    0
#> 13 young    Spain <NA> nonEU    0
#> 14   old    Spain <NA> nonEU    0
#> 15 young Portugal <NA> nonEU    0
#> 16   old Portugal <NA> nonEU    0
Extend0(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"), extraVar = FALSE)
#>      age      geo    eu freq
#> 1  young    Spain    EU    1
#> 2    old    Spain    EU    1
#> 3    old  Iceland nonEU    1
#> 4    old Portugal    EU    1
#> 5  young  Iceland nonEU    1
#> 6    old  Iceland nonEU    1
#> 7    old Portugal    EU    1
#> 8  young    Spain    EU    1
#> 9  young  Iceland nonEU    1
#> 10 young Portugal    EU    1
#> 11 young  Iceland    EU    0
#> 12   old  Iceland    EU    0
#> 13 young    Spain nonEU    0
#> 14   old    Spain nonEU    0
#> 15 young Portugal nonEU    0
#> 16   old Portugal nonEU    0
Extend0(z, varGroups = list(c("age", "geo", "year"), "eu"))
#>      age      geo year ths_per    eu freq
#> 1  young    Spain 2014    66.9    EU    1
#> 2    old    Spain 2014   120.3    EU    1
#> 3    old  Iceland 2014     1.5 nonEU    1
#> 4    old Portugal 2014    20.2    EU    1
#> 5  young  Iceland 2015     1.9 nonEU    1
#> 6    old  Iceland 2015     1.6 nonEU    1
#> 7    old Portugal 2015    24.3    EU    1
#> 8  young    Spain 2016    69.1    EU    1
#> 9  young  Iceland 2016     1.9 nonEU    1
#> 10 young Portugal 2016    12.7    EU    1
#> 11   old  Iceland 2014     0.0    EU    0
#> 12 young  Iceland 2015     0.0    EU    0
#> 13   old  Iceland 2015     0.0    EU    0
#> 14 young  Iceland 2016     0.0    EU    0
#> 15 young    Spain 2014     0.0 nonEU    0
#> 16   old    Spain 2014     0.0 nonEU    0
#> 17   old Portugal 2014     0.0 nonEU    0
#> 18   old Portugal 2015     0.0 nonEU    0
#> 19 young    Spain 2016     0.0 nonEU    0
#> 20 young Portugal 2016     0.0 nonEU    0
Extend0(MakeFreq(z[c(1, 1, 1, 2, 2, 3:10), -4]))
#>      age      geo year    eu freq
#> 1    old  Iceland 2014 nonEU    1
#> 2    old  Iceland 2015 nonEU    1
#> 3    old Portugal 2014    EU    1
#> 4    old Portugal 2015    EU    1
#> 5    old    Spain 2014    EU    2
#> 6  young  Iceland 2015 nonEU    1
#> 7  young  Iceland 2016 nonEU    1
#> 8  young Portugal 2016    EU    1
#> 9  young    Spain 2014    EU    3
#> 10 young    Spain 2016    EU    1
#> 11 young  Iceland 2014 nonEU    0
#> 12 young Portugal 2014    EU    0
#> 13 young Portugal 2015    EU    0
#> 14   old    Spain 2015    EU    0
#> 15 young    Spain 2015    EU    0
#> 16   old  Iceland 2016 nonEU    0
#> 17   old Portugal 2016    EU    0
#> 18   old    Spain 2016    EU    0
Extend0(z, "ths_per")
#>      age      geo year ths_per    eu
#> 1  young    Spain 2014    66.9    EU
#> 2    old    Spain 2014   120.3    EU
#> 3    old  Iceland 2014     1.5 nonEU
#> 4    old Portugal 2014    20.2    EU
#> 5  young  Iceland 2015     1.9 nonEU
#> 6    old  Iceland 2015     1.6 nonEU
#> 7    old Portugal 2015    24.3    EU
#> 8  young    Spain 2016    69.1    EU
#> 9  young  Iceland 2016     1.9 nonEU
#> 10 young Portugal 2016    12.7    EU
#> 11 young  Iceland 2014     0.0 nonEU
#> 12 young Portugal 2014     0.0    EU
#> 13 young    Spain 2015     0.0    EU
#> 14   old    Spain 2015     0.0    EU
#> 15 young Portugal 2015     0.0    EU
#> 16   old    Spain 2016     0.0    EU
#> 17   old  Iceland 2016     0.0 nonEU
#> 18   old Portugal 2016     0.0    EU

# varGroups with data frames (same result as with names above)
Extend0(z, varGroups = list(z[c("age", "geo", "year")], z["eu"]))
#>      age      geo year ths_per    eu freq
#> 1  young    Spain 2014    66.9    EU    1
#> 2    old    Spain 2014   120.3    EU    1
#> 3    old  Iceland 2014     1.5 nonEU    1
#> 4    old Portugal 2014    20.2    EU    1
#> 5  young  Iceland 2015     1.9 nonEU    1
#> 6    old  Iceland 2015     1.6 nonEU    1
#> 7    old Portugal 2015    24.3    EU    1
#> 8  young    Spain 2016    69.1    EU    1
#> 9  young  Iceland 2016     1.9 nonEU    1
#> 10 young Portugal 2016    12.7    EU    1
#> 11   old  Iceland 2014     0.0    EU    0
#> 12 young  Iceland 2015     0.0    EU    0
#> 13   old  Iceland 2015     0.0    EU    0
#> 14 young  Iceland 2016     0.0    EU    0
#> 15 young    Spain 2014     0.0 nonEU    0
#> 16   old    Spain 2014     0.0 nonEU    0
#> 17   old Portugal 2014     0.0 nonEU    0
#> 18   old Portugal 2015     0.0 nonEU    0
#> 19 young    Spain 2016     0.0 nonEU    0
#> 20 young Portugal 2016     0.0 nonEU    0

# varGroups with both names and data frame
Extend0(z, varGroups = list(c("year", "geo", "eu"), data.frame(age = c("middle", "old"))))
#>       age      geo year ths_per    eu freq
#> 1   young    Spain 2014    66.9    EU    1
#> 2     old    Spain 2014   120.3    EU    1
#> 3     old  Iceland 2014     1.5 nonEU    1
#> 4     old Portugal 2014    20.2    EU    1
#> 5   young  Iceland 2015     1.9 nonEU    1
#> 6     old  Iceland 2015     1.6 nonEU    1
#> 7     old Portugal 2015    24.3    EU    1
#> 8   young    Spain 2016    69.1    EU    1
#> 9   young  Iceland 2016     1.9 nonEU    1
#> 10  young Portugal 2016    12.7    EU    1
#> 11 middle    Spain 2014     0.0    EU    0
#> 12 middle  Iceland 2014     0.0 nonEU    0
#> 13 middle Portugal 2014     0.0    EU    0
#> 14 middle  Iceland 2015     0.0 nonEU    0
#> 15 middle Portugal 2015     0.0    EU    0
#> 16 middle    Spain 2016     0.0    EU    0
#> 17 middle  Iceland 2016     0.0 nonEU    0
#> 18 middle Portugal 2016     0.0    EU    0
#> 19    old    Spain 2016     0.0    EU    0
#> 20    old  Iceland 2016     0.0 nonEU    0
#> 21    old Portugal 2016     0.0    EU    0
```
