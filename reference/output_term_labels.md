# Extract vector of term labels from a data.frame

The data.frame is assumed to be have been constructed with
[`ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.md)
functionality using the `formula` parameter.

## Usage

``` r
output_term_labels(x)
```

## Arguments

- x:

  data.frame

## Value

vector of term labels

## See also

[`formula_term_labels()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/formula_term_labels.md),
[`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.md)

## Examples

``` r
out <- model_aggregate(SSBtoolsData("magnitude1"), 
                       formula = ~eu:sector4 + geo * sector2, 
                       sum_vars = "value",
                       avoid_hierarchical = TRUE)
#> [pre_aggregate 20*6->10*5] [ModelMatrix] [crossprod] [cbind]
out
#>       eu       sector4      geo sector2 value
#> 1  Total         Total    Total   Total 462.3
#> 2  Total         Total  Iceland   Total  37.1
#> 3  Total         Total Portugal   Total 162.5
#> 4  Total         Total    Spain   Total 262.7
#> 5  Total         Total    Total private 429.5
#> 6  Total         Total    Total  public  32.8
#> 7     EU   Agriculture    Total   Total 240.2
#> 8     EU Entertainment    Total   Total 114.7
#> 9     EU  Governmental    Total   Total  32.8
#> 10    EU      Industry    Total   Total  37.5
#> 11 nonEU Entertainment    Total   Total  16.8
#> 12 nonEU      Industry    Total   Total  20.3
#> 13 Total         Total  Iceland private  37.1
#> 14 Total         Total Portugal private 138.9
#> 15 Total         Total Portugal  public  23.6
#> 16 Total         Total    Spain private 253.5
#> 17 Total         Total    Spain  public   9.2
term_labels <- output_term_labels(out)
term_labels
#>  [1] "(Intercept)" "geo"         "geo"         "geo"         "sector2"    
#>  [6] "sector2"     "eu:sector4"  "eu:sector4"  "eu:sector4"  "eu:sector4" 
#> [11] "eu:sector4"  "eu:sector4"  "geo:sector2" "geo:sector2" "geo:sector2"
#> [16] "geo:sector2" "geo:sector2"
cbind(term_labels, out)
#>    term_labels    eu       sector4      geo sector2 value
#> 1  (Intercept) Total         Total    Total   Total 462.3
#> 2          geo Total         Total  Iceland   Total  37.1
#> 3          geo Total         Total Portugal   Total 162.5
#> 4          geo Total         Total    Spain   Total 262.7
#> 5      sector2 Total         Total    Total private 429.5
#> 6      sector2 Total         Total    Total  public  32.8
#> 7   eu:sector4    EU   Agriculture    Total   Total 240.2
#> 8   eu:sector4    EU Entertainment    Total   Total 114.7
#> 9   eu:sector4    EU  Governmental    Total   Total  32.8
#> 10  eu:sector4    EU      Industry    Total   Total  37.5
#> 11  eu:sector4 nonEU Entertainment    Total   Total  16.8
#> 12  eu:sector4 nonEU      Industry    Total   Total  20.3
#> 13 geo:sector2 Total         Total  Iceland private  37.1
#> 14 geo:sector2 Total         Total Portugal private 138.9
#> 15 geo:sector2 Total         Total Portugal  public  23.6
#> 16 geo:sector2 Total         Total    Spain private 253.5
#> 17 geo:sector2 Total         Total    Spain  public   9.2
```
