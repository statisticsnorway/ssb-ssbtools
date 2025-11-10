# Collapse variables to single representation

Simplify a data frame by collapsing specified variables, according to
the location of total codes, into a single vector or by consolidating
groups of variables into new columns.

## Usage

``` r
total_collapse(data, variables, total = "Total", include_names = NULL)
```

## Arguments

- data:

  A data frame containing the variables to be collapsed.

- variables:

  A vector of variable names or a named list of variable names.

  - If `variables` is a vector, the specified variables in `data` are
    collapsed into a single character vector.

  - If `variables` is a named list, each element in the list defines a
    group of variables to consolidate into a new column. Each list name
    will be used as the new column name in the resulting data frame.

- total:

  A total code or vector of total codes to use in the result.

  - If `variables` is a vector, `total` specifies the code to represent
    collapsed values.

  - If `variables` is a named list, `total` may contain one code per
    group.

- include_names:

  A character string or `NULL` (default).

  - If `variables` is a vector, whether the resulting output vector is
    named depends on whether `include_names` is `NULL` or not. The
    actual value of `include_names` is ignored in this case.

  - If `variables` is a named list, `include_names` specifies a suffix
    to append to each group name, creating one additional column per
    group. If `NULL`, no additional columns with variable names are
    included in the result.

## Value

A character vector (if `variables` is a vector) or a modified data frame
(if `variables` is a named list).

## Examples

``` r
# Creates data that can act as input
magnitude1 <- SSBtoolsData("magnitude1")
a <- model_aggregate(magnitude1, 
                     formula = ~geo + eu + sector2 + sector4, 
                     sum_vars = "value", 
                     avoid_hierarchical = TRUE)
#> [pre_aggregate 20*6->10*5] [ModelMatrix] [crossprod] [cbind]
a
#>         geo    eu sector2       sector4 value
#> 1     Total Total   Total         Total 462.3
#> 2   Iceland Total   Total         Total  37.1
#> 3  Portugal Total   Total         Total 162.5
#> 4     Spain Total   Total         Total 262.7
#> 5     Total    EU   Total         Total 425.2
#> 6     Total nonEU   Total         Total  37.1
#> 7     Total Total private         Total 429.5
#> 8     Total Total  public         Total  32.8
#> 9     Total Total   Total   Agriculture 240.2
#> 10    Total Total   Total Entertainment 131.5
#> 11    Total Total   Total  Governmental  32.8
#> 12    Total Total   Total      Industry  57.8

b <- total_collapse(a, list(GEO = c("geo", "eu"), SECTOR = c("sector2", "sector4")))
b
#>         GEO        SECTOR value
#> 1     Total         Total 462.3
#> 2   Iceland         Total  37.1
#> 3  Portugal         Total 162.5
#> 4     Spain         Total 262.7
#> 5        EU         Total 425.2
#> 6     nonEU         Total  37.1
#> 7     Total       private 429.5
#> 8     Total        public  32.8
#> 9     Total   Agriculture 240.2
#> 10    Total Entertainment 131.5
#> 11    Total  Governmental  32.8
#> 12    Total      Industry  57.8

total_collapse(a, c("geo", "eu"))
#>  [1] "Total"    "Iceland"  "Portugal" "Spain"    "EU"       "nonEU"   
#>  [7] "Total"    "Total"    "Total"    "Total"    "Total"    "Total"   
total_collapse(a, c("sector2", "sector4"))                                 
#>  [1] "Total"         "Total"         "Total"         "Total"        
#>  [5] "Total"         "Total"         "private"       "public"       
#>  [9] "Agriculture"   "Entertainment" "Governmental"  "Industry"     


# Similar examples with both `total` and `include_names` parameters
aa <- a
aa[1:2][aa[1:2] == "Total"] <- "Europe"
aa[3:4][aa[3:4] == "Total"] <- ""
aa
#>         geo     eu sector2       sector4 value
#> 1    Europe Europe                       462.3
#> 2   Iceland Europe                        37.1
#> 3  Portugal Europe                       162.5
#> 4     Spain Europe                       262.7
#> 5    Europe     EU                       425.2
#> 6    Europe  nonEU                        37.1
#> 7    Europe Europe private               429.5
#> 8    Europe Europe  public                32.8
#> 9    Europe Europe           Agriculture 240.2
#> 10   Europe Europe         Entertainment 131.5
#> 11   Europe Europe          Governmental  32.8
#> 12   Europe Europe              Industry  57.8

bb <- total_collapse(data = aa, 
                     variables = list(GEO = c("geo", "eu"), 
                                      SECTOR = c("sector2", "sector4")), 
                     total = c("Europe", ""),
                     include_names = "_Vars")
bb
#>         GEO GEO_Vars        SECTOR SECTOR_Vars value
#> 1    Europe      geo                   sector2 462.3
#> 2   Iceland      geo                   sector2  37.1
#> 3  Portugal      geo                   sector2 162.5
#> 4     Spain      geo                   sector2 262.7
#> 5        EU       eu                   sector2 425.2
#> 6     nonEU       eu                   sector2  37.1
#> 7    Europe      geo       private     sector2 429.5
#> 8    Europe      geo        public     sector2  32.8
#> 9    Europe      geo   Agriculture     sector4 240.2
#> 10   Europe      geo Entertainment     sector4 131.5
#> 11   Europe      geo  Governmental     sector4  32.8
#> 12   Europe      geo      Industry     sector4  57.8

total_collapse(aa, c("geo", "eu"), total = "Europe", include_names = "_Vars")
#>        geo        geo        geo        geo         eu         eu        geo 
#>   "Europe"  "Iceland" "Portugal"    "Spain"       "EU"    "nonEU"   "Europe" 
#>        geo        geo        geo        geo        geo 
#>   "Europe"   "Europe"   "Europe"   "Europe"   "Europe" 
total_collapse(aa, c("sector2", "sector4"), total = "", include_names = "_Vars") 
#>         sector2         sector2         sector2         sector2         sector2 
#>              ""              ""              ""              ""              "" 
#>         sector2         sector2         sector2         sector4         sector4 
#>              ""       "private"        "public"   "Agriculture" "Entertainment" 
#>         sector4         sector4 
#>  "Governmental"      "Industry" 


# All four variables can be collapsed
total_collapse(a, 
               list(ALL = c("geo", "eu", "sector2", "sector4")), 
               include_names = "_Vars")
#>              ALL ALL_Vars value
#> 1          Total      geo 462.3
#> 2        Iceland      geo  37.1
#> 3       Portugal      geo 162.5
#> 4          Spain      geo 262.7
#> 5             EU       eu 425.2
#> 6          nonEU       eu  37.1
#> 7        private  sector2 429.5
#> 8         public  sector2  32.8
#> 9    Agriculture  sector4 240.2
#> 10 Entertainment  sector4 131.5
#> 11  Governmental  sector4  32.8
#> 12      Industry  sector4  57.8
```
