# Row selection by wildcard/globbing

The selected rows match combined requirements for all variables.

## Usage

``` r
WildcardGlobbing(x, wg, sign = TRUE, invert = "!")
```

## Arguments

- x:

  data.frame with character data

- wg:

  data.frame with wildcard/globbing

- sign:

  When FALSE, the result is inverted.

- invert:

  Character to invert each single selection.

## Value

Logical vector defining subset of rows.

## Details

This function is used by
[`HierarchicalWildcardGlobbing`](https://statisticsnorway.github.io/ssb-ssbtools/reference/HierarchicalWildcardGlobbing.md)
and
[`WildcardGlobbingVector`](https://statisticsnorway.github.io/ssb-ssbtools/reference/WildcardGlobbingVector.md)
and make use of [`grepl`](https://rdrr.io/r/base/grep.html) and
[`glob2rx`](https://rdrr.io/r/utils/glob2rx.html).

## Author

Øyvind Langsrud

## Examples

``` r
# Create data input
data(precip)
data(mtcars)
x <- data.frame(car = rownames(mtcars)[rep(1:NROW(mtcars), each = 35)], city = names(precip), 
                stringsAsFactors = FALSE)

# Create globbing/wildcards input
wg <- data.frame(rbind(c("Merc*", "C*"), c("F*", "??????"), c("!?????????*", "!???????*")), 
                 stringsAsFactors = FALSE)
names(wg) <- names(x)

# Select the following combinations:
# - Cars starting with Merc and cities starting with C
# - Cars starting with F and six-letter cities 
# - Cars with less than nine letters and cities with less than seven letters
x[WildcardGlobbing(x, wg), ]
#>                 car       city
#> 176         Valiant       Reno
#> 180         Valiant     Albany
#> 198         Valiant     Dallas
#> 247       Merc 240D    Concord
#> 253       Merc 240D  Charlotte
#> 256       Merc 240D Cincinnati
#> 257       Merc 240D  Cleveland
#> 258       Merc 240D   Columbus
#> 264       Merc 240D   Columbia
#> 277       Merc 240D Charleston
#> 279       Merc 240D   Cheyenne
#> 281        Merc 230     Mobile
#> 282        Merc 230     Juneau
#> 288        Merc 230     Denver
#> 293        Merc 230      Miami
#> 296        Merc 230      Boise
#> 297        Merc 230    Chicago
#> 298        Merc 230     Peoria
#> 306        Merc 230     Boston
#> 309        Merc 230     Duluth
#> 315        Merc 230      Omaha
#> 316        Merc 280       Reno
#> 317        Merc 280    Concord
#> 320        Merc 280     Albany
#> 323        Merc 280  Charlotte
#> 326        Merc 280 Cincinnati
#> 327        Merc 280  Cleveland
#> 328        Merc 280   Columbus
#> 334        Merc 280   Columbia
#> 338        Merc 280     Dallas
#> 347        Merc 280 Charleston
#> 349        Merc 280   Cheyenne
#> 367       Merc 280C    Chicago
#> 387      Merc 450SE    Concord
#> 393      Merc 450SE  Charlotte
#> 396      Merc 450SE Cincinnati
#> 397      Merc 450SE  Cleveland
#> 398      Merc 450SE   Columbus
#> 404      Merc 450SE   Columbia
#> 417      Merc 450SE Charleston
#> 419      Merc 450SE   Cheyenne
#> 437      Merc 450SL    Chicago
#> 457     Merc 450SLC    Concord
#> 463     Merc 450SLC  Charlotte
#> 466     Merc 450SLC Cincinnati
#> 467     Merc 450SLC  Cleveland
#> 468     Merc 450SLC   Columbus
#> 474     Merc 450SLC   Columbia
#> 487     Merc 450SLC Charleston
#> 489     Merc 450SLC   Cheyenne
#> 596        Fiat 128       Reno
#> 600        Fiat 128     Albany
#> 618        Fiat 128     Dallas
#> 880       Fiat X1-9     Albany
#> 898       Fiat X1-9     Dallas
#> 981  Ford Pantera L     Mobile
#> 982  Ford Pantera L     Juneau
#> 988  Ford Pantera L     Denver
#> 998  Ford Pantera L     Peoria
#> 1006 Ford Pantera L     Boston
#> 1009 Ford Pantera L     Duluth
#> 1020   Ferrari Dino     Albany
#> 1038   Ferrari Dino     Dallas
```
