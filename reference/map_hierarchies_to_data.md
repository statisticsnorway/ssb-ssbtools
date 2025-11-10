# Add variables to dataset based on hierarchies

Uses
[`hierarchies_as_vars`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md)
to transform hierarchies, followed by mapping to the dataset.

## Usage

``` r
map_hierarchies_to_data(
  data,
  hierarchies,
  when_overwritten = warning,
  add_comment = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame containing variables with names matching the names of the
  hierarchies.

- hierarchies:

  List of hierarchies in the same format as input to
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)

- when_overwritten:

  A function to be called when existing column(s) are overwritten.
  Supply `stop` to invoke an error, `warning` for a warning (default),
  `message` to display an informational message, or `NULL` to do
  nothing.

- add_comment:

  Logical. When `TRUE` (default), a comment attribute will be added to
  the output data frame, containing the names of the variables that were
  added.

- ...:

  Further parameters sent to
  [`hierarchies_as_vars`](https://statisticsnorway.github.io/ssb-ssbtools/reference/hierarchies_as_vars.md)

## Value

Input `data` with extra Variables

## Examples

``` r
# Examples similar those from hierarchies_as_vars

z <- SSBtoolsData("sprt_emp_withEU")
year_formula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
geo_dim_list <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
age_hierarchy <- SSBtoolsData("sprt_emp_ageHier")

map_hierarchies_to_data(z, list(age = age_hierarchy, geo = geo_dim_list, 
                                year = year_formula))
#>       age      geo year ths_per    eu age_level_1 geo_level_1 geo_level_2
#> 1  Y15-29    Spain 2014    66.9    EU      Y15-64          EU      Europe
#> 2  Y15-29  Iceland 2014     1.8 nonEU      Y15-64       nonEU      Europe
#> 3  Y15-29 Portugal 2014    11.6    EU      Y15-64          EU      Europe
#> 4  Y30-64    Spain 2014   120.3    EU      Y15-64          EU      Europe
#> 5  Y30-64  Iceland 2014     1.5 nonEU      Y15-64       nonEU      Europe
#> 6  Y30-64 Portugal 2014    20.2    EU      Y15-64          EU      Europe
#> 7  Y15-29    Spain 2015    63.4    EU      Y15-64          EU      Europe
#> 8  Y15-29  Iceland 2015     1.9 nonEU      Y15-64       nonEU      Europe
#> 9  Y15-29 Portugal 2015    14.2    EU      Y15-64          EU      Europe
#> 10 Y30-64    Spain 2015   119.6    EU      Y15-64          EU      Europe
#> 11 Y30-64  Iceland 2015     1.6 nonEU      Y15-64       nonEU      Europe
#> 12 Y30-64 Portugal 2015    24.3    EU      Y15-64          EU      Europe
#> 13 Y15-29    Spain 2016    69.1    EU      Y15-64          EU      Europe
#> 14 Y15-29  Iceland 2016     1.9 nonEU      Y15-64       nonEU      Europe
#> 15 Y15-29 Portugal 2016    12.7    EU      Y15-64          EU      Europe
#> 16 Y30-64    Spain 2016   122.1    EU      Y15-64          EU      Europe
#> 17 Y30-64  Iceland 2016     1.9 nonEU      Y15-64       nonEU      Europe
#> 18 Y30-64 Portugal 2016    25.8    EU      Y15-64          EU      Europe
#>    year_level_1 year_level_2
#> 1          y_14        y_all
#> 2          y_14        y_all
#> 3          y_14        y_all
#> 4          y_14        y_all
#> 5          y_14        y_all
#> 6          y_14        y_all
#> 7       y_15_16        y_all
#> 8       y_15_16        y_all
#> 9       y_15_16        y_all
#> 10      y_15_16        y_all
#> 11      y_15_16        y_all
#> 12      y_15_16        y_all
#> 13      y_15_16        y_all
#> 14      y_15_16        y_all
#> 15      y_15_16        y_all
#> 16      y_15_16        y_all
#> 17      y_15_16        y_all
#> 18      y_15_16        y_all

map_hierarchies_to_data(data.frame(f = c("A", "B", "C", "D", "E", "A")), list(f = 
       c("AB = A + B", "AC = A + C", "CD = C + D", "ABCD = AB + CD")))
#>   f f_level_1 f_level_2 f_level_3
#> 1 A        AB        AC      ABCD
#> 2 B        AB      <NA>      ABCD
#> 3 C        CD        AC      ABCD
#> 4 D        CD      <NA>      ABCD
#> 5 E      <NA>      <NA>      <NA>
#> 6 A        AB        AC      ABCD
       
       
# Examples demonstrating when_overwritten and add_comment        
       
a <- map_hierarchies_to_data(z, list(age = age_hierarchy, geo = geo_dim_list))
comment(a)
#> [1] "age_level_1" "geo_level_1" "geo_level_2"

b <- map_hierarchies_to_data(a[-7], list(age = age_hierarchy, geo = geo_dim_list), 
                             when_overwritten = message, add_comment = FALSE)
#> Overwritten columns: age_level_1, geo_level_2
comment(b)
#> NULL
```
