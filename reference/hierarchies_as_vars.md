# Hierarchies coded as variables

The hierarchical relations are stored as minimal datasets

## Usage

``` r
hierarchies_as_vars(
  hierarchies,
  name_function = function(name, level) paste0(name, "_level_", level),
  single_vars = FALSE,
  from_dummy = NA,
  dummy_reorder = TRUE,
  combine_vars = TRUE,
  drop_codes = NULL,
  include_codes = NULL,
  ...
)
```

## Arguments

- hierarchies:

  List of hierarchies in the same format as input to
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)

- name_function:

  A function defining how to name all columns except the first. The
  input consists of the hierarchy name (identical to the first column’s
  name, `name`) and the column number minus 1 (`level`).

- single_vars:

  When `TRUE`, a single variable is created for all codes except the
  input codes.

- from_dummy:

  Logical value indicating the method for handling hierarchies.

  - When `TRUE`, the algorithm uses dummy-coded hierarchies.

  - When `FALSE`, the algorithm works directly on hierarchies
    standardized by `AutoHierarchies`, often resulting in
    well-structured output variables.

  - When `NA` (default), the algorithm first attempts the `FALSE`
    method; if not feasible, it falls back to the `TRUE` method.

- dummy_reorder:

  When `TRUE`, dummy-coded hierarchies are reordered to potentially
  improve the structure of output variables.

- combine_vars:

  When `TRUE`, an algorithm is applied to potentially reduce the number
  of output variables needed.

- drop_codes:

  A named list of codes (except the input codes) to be dropped from the
  output. The list must have the same names as the hierarchies, but not
  all names/elements need to be included.

- include_codes:

  Similar to drop_codes, but specifies the codes to be included instead.

- ...:

  Additional parameters passed to
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.md)

## Value

Named list of data frames

## See also

[`vars_to_hierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/vars_to_hierarchies.md)

## Examples

``` r
# Examples based on those from AutoHierarchies
# You may also try converting other examples from AutoHierarchies

z <- SSBtoolsData("sprt_emp_withEU")
year_formula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
geo_dim_list <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
age_hierarchy <- SSBtoolsData("sprt_emp_ageHier")

hierarchies_as_vars(list(age = age_hierarchy, geo = geo_dim_list, year = year_formula))
#> $age
#>      age age_level_1
#> 1 Y15-29      Y15-64
#> 2 Y30-64      Y15-64
#> 
#> $geo
#>        geo geo_level_1 geo_level_2
#> 1  Iceland       nonEU      Europe
#> 2 Portugal          EU      Europe
#> 3    Spain          EU      Europe
#> 
#> $year
#>   year year_level_1 year_level_2
#> 1 2014         y_14        y_all
#> 2 2015      y_15_16        y_all
#> 3 2016      y_15_16        y_all
#> 
hierarchies_as_vars(list(age = age_hierarchy, geo = geo_dim_list, year = year_formula), 
                    singleVars = TRUE)
#> $age
#>      age age_level_1
#> 1 Y15-29      Y15-64
#> 2 Y30-64      Y15-64
#> 
#> $geo
#>        geo geo_level_1 geo_level_2
#> 1  Iceland       nonEU      Europe
#> 2 Portugal          EU      Europe
#> 3    Spain          EU      Europe
#> 
#> $year
#>   year year_level_1 year_level_2
#> 1 2014         y_14        y_all
#> 2 2015      y_15_16        y_all
#> 3 2016      y_15_16        y_all
#> 
                    
# NAs are included in data when necessary
hierarchies_as_vars(list(f = c("AB = A + B", "AC = A + C", "CD = C + D", "ABCD = AB + CD")))                     
#> $f
#>   f f_level_1 f_level_2 f_level_3
#> 1 A        AB        AC      ABCD
#> 2 B        AB      <NA>      ABCD
#> 3 C        CD        AC      ABCD
#> 4 D        CD      <NA>      ABCD
#> 

# drop_codes and include_codes  
hierarchies_as_vars(list(age = age_hierarchy, geo = geo_dim_list, year = year_formula), 
                    drop_codes = list(geo = "nonEU", year = c("y_14", "y_all")))  
#> $age
#>      age age_level_1
#> 1 Y15-29      Y15-64
#> 2 Y30-64      Y15-64
#> 
#> $geo
#>        geo geo_level_1 geo_level_2
#> 1  Iceland        <NA>      Europe
#> 2 Portugal          EU      Europe
#> 3    Spain          EU      Europe
#> 
#> $year
#>   year year_level_1
#> 1 2014         <NA>
#> 2 2015      y_15_16
#> 3 2016      y_15_16
#> 
hierarchies_as_vars(list(age = age_hierarchy, geo = geo_dim_list, year = year_formula), 
                    include_codes = list(year = c("y_14", "y_all")))      
#> $age
#>      age age_level_1
#> 1 Y15-29      Y15-64
#> 2 Y30-64      Y15-64
#> 
#> $geo
#>        geo geo_level_1 geo_level_2
#> 1  Iceland       nonEU      Europe
#> 2 Portugal          EU      Europe
#> 3    Spain          EU      Europe
#> 
#> $year
#>   year year_level_1 year_level_2
#> 1 2014         y_14        y_all
#> 2 2015         <NA>        y_all
#> 3 2016         <NA>        y_all
#> 
               
```
