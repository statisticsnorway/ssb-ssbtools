# Selection of elements by wildcard/globbing

Selection of elements by wildcard/globbing

## Usage

``` r
WildcardGlobbingVector(x, wg, negSign = "-", invert = "!")
```

## Arguments

- x:

  Character vector

- wg:

  Character vector with wildcard/globbing

- negSign:

  Character representing selection to be removed

- invert:

  Character to invert each single selection.

## Value

vector with selected elements of x

## Author

Øyvind Langsrud

## Examples

``` r
data(precip)
x <- names(precip)

# Select the cities starting with B, C and Sa.
WildcardGlobbingVector(x, c("B*", "C*", "Sa*"))
#>  [1] "Sacramento"       "San Francisco"    "Boise"            "Chicago"         
#>  [5] "Baltimore"        "Boston"           "Sault Ste. Marie" "Concord"         
#>  [9] "Buffalo"          "Charlotte"        "Bismark"          "Cincinnati"      
#> [13] "Cleveland"        "Columbus"         "Columbia"         "Salt Lake City"  
#> [17] "Burlington"       "Charleston"       "Cheyenne"         "San Juan"        

# Remove from the selection cities with o and t in position 2 and 4, respectively.
WildcardGlobbingVector(x, c("B*", "C*", "Sa*", "-?o*", "-???t*"))
#>  [1] "Sacramento"       "San Francisco"    "Chicago"          "Sault Ste. Marie"
#>  [5] "Buffalo"          "Charlotte"        "Bismark"          "Cincinnati"      
#>  [9] "Cleveland"        "Burlington"       "Charleston"       "Cheyenne"        
#> [13] "San Juan"        

# Add to the selection cities not having six or more letters.
WildcardGlobbingVector(x, c("B*", "C*", "Sa*", "-?o*", "-???t*", "!??????*"))
#>  [1] "Sacramento"       "San Francisco"    "Miami"            "Chicago"         
#>  [5] "Sault Ste. Marie" "Omaha"            "Reno"             "Buffalo"         
#>  [9] "Charlotte"        "Bismark"          "Cincinnati"       "Cleveland"       
#> [13] "Burlington"       "Charleston"       "Cheyenne"         "San Juan"        
```
