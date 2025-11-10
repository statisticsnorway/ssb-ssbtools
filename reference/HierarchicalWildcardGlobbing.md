# Find variable combinations by advanced wildcard/globbing specifications.

Find combinations present in an input data frame or, when input is a
list, find all possible combinations that meet the requirements.

## Usage

``` r
HierarchicalWildcardGlobbing(
  z,
  wg,
  useUnique = NULL,
  useFactor = FALSE,
  makeWarning = TRUE,
  printInfo = FALSE,
  useMatrixToDataFrame = TRUE,
  invert = "!"
)
```

## Arguments

- z:

  list or data.frame

- wg:

  data.frame with data globbing and wildcards

- useUnique:

  Logical variable about recoding within the algorithm. By default
  (NULL) an automatic decision is made.

- useFactor:

  When TRUE, internal factor recoding is used.

- makeWarning:

  When TRUE, warning is made in cases of unused variables. Only
  variables common to z and wg are used.

- printInfo:

  When TRUE, information is printed during the process.

- useMatrixToDataFrame:

  When TRUE, special functions (DataFrameToMatrix/MatrixToDataFrame) for
  improving speed and memory is utilized.

- invert:

  Character to invert each single selection.

## Value

data.frame

## Details

The final variable combinations must meet the requirements in each
positive sign group and must not match the requirements in the negative
sign groups.The function is implemented by calling
[`WildcardGlobbing`](https://statisticsnorway.github.io/ssb-ssbtools/reference/WildcardGlobbing.md)
several times within an algorithm that uses hierarchical clustering
([`hclust`](https://rdrr.io/r/stats/hclust.html)).

## Author

Øyvind Langsrud

## Examples

``` r
                  
# useUnique=NULL betyr valg ut fra antall rader i kombinasjonsfil
data(precip)
data(mtcars)
codes <- as.character(c(100, 200, 300, 600, 700, 101, 102, 103, 104, 134, 647, 783, 
                        13401, 13402, 64701, 64702))


# Create list input
zList <- list(car = rownames(mtcars), wt = as.character(1000 * mtcars$wt), 
              city = names(precip), code = codes)

# Create data.frame input
m <- cbind(car = rownames(mtcars), wt = as.character(1000 * mtcars$wt))
zFrame <- data.frame(m[rep(1:NROW(m), each = 35), ], 
                     city = names(precip), code = codes, stringsAsFactors = FALSE)

# Create globbing/wildcards input
wg <- data.frame(rbind(c("Merc*", ""    , ""    , "?00"  ), 
                       c("F*"   , ""    , ""    , "?????"), 
                       c(""     , "???0", "C*"  , ""     ), 
                       c(""     , ""    , "!Co*", ""     ), 
                       c(""     , ""    , "?i*" , "????2"), 
                       c(""     , ""    , "?h*" , "????1")), 
           sign = c("+", "+", "+", "+", "-", "-"), stringsAsFactors = FALSE)
names(wg)[1:4] <- names(zList)



# =================================================================== 
#   Finding unique combinations present in the input data frame
# ===================================================================


# Using first row of wg. Combinations of car starting with Merc 
# and three-digit code ending with 00
HierarchicalWildcardGlobbing(zFrame[, c(1, 4)], wg[1, c(1, 4, 5)])
#>            car code
#> 1    Merc 240D  100
#> 2     Merc 230  100
#> 3     Merc 280  100
#> 4    Merc 280C  100
#> 5   Merc 450SE  100
#> 6   Merc 450SL  100
#> 7  Merc 450SLC  100
#> 8    Merc 240D  200
#> 9     Merc 230  200
#> 10    Merc 280  200
#> 11   Merc 280C  200
#> 12  Merc 450SE  200
#> 13  Merc 450SL  200
#> 14 Merc 450SLC  200
#> 15   Merc 240D  300
#> 16    Merc 230  300
#> 17    Merc 280  300
#> 18   Merc 280C  300
#> 19  Merc 450SE  300
#> 20  Merc 450SL  300
#> 21 Merc 450SLC  300
#> 22   Merc 240D  600
#> 23    Merc 230  600
#> 24    Merc 280  600
#> 25   Merc 280C  600
#> 26  Merc 450SE  600
#> 27  Merc 450SL  600
#> 28 Merc 450SLC  600
#> 29   Merc 240D  700
#> 30    Merc 230  700
#> 31    Merc 280  700
#> 32   Merc 280C  700
#> 33  Merc 450SE  700
#> 34  Merc 450SL  700
#> 35 Merc 450SLC  700

# Using first row of wg. Combinations of all four variables
HierarchicalWildcardGlobbing(zFrame, wg[1, ])
#>            car code   wt                city
#> 1   Merc 450SL  700 3730              Mobile
#> 2    Merc 280C  100 3440             Phoenix
#> 3    Merc 280C  200 3440         Little Rock
#> 4    Merc 280C  300 3440         Los Angeles
#> 5    Merc 280C  600 3440          Sacramento
#> 6    Merc 280C  700 3440       San Francisco
#> 7     Merc 230  100 3150            Hartford
#> 8     Merc 230  200 3150          Wilmington
#> 9     Merc 230  300 3150          Washington
#> 10    Merc 230  600 3150        Jacksonville
#> 11    Merc 230  700 3150               Miami
#> 12  Merc 450SL  100 3730               Miami
#> 13  Merc 450SL  200 3730             Atlanta
#> 14  Merc 450SL  300 3730            Honolulu
#> 15  Merc 450SL  600 3730               Boise
#> 16  Merc 450SL  700 3730             Chicago
#> 17   Merc 280C  100 3440        Indianapolis
#> 18   Merc 280C  200 3440          Des Moines
#> 19   Merc 280C  300 3440             Wichita
#> 20   Merc 280C  600 3440          Louisville
#> 21   Merc 280C  700 3440         New Orleans
#> 22   Merc 240D  600 3190            Portland
#> 23    Merc 230  100 3150           Baltimore
#> 24    Merc 230  200 3150              Boston
#> 25    Merc 230  300 3150             Detroit
#> 26    Merc 230  600 3150    Sault Ste. Marie
#> 27    Merc 230  700 3150              Duluth
#> 28  Merc 450SL  100 3730              Duluth
#> 29  Merc 450SL  200 3730 Minneapolis/St Paul
#> 30  Merc 450SL  300 3730             Jackson
#> 31  Merc 450SL  600 3730         Kansas City
#> 32  Merc 450SL  700 3730            St Louis
#> 33   Merc 280C  100 3440               Omaha
#> 34  Merc 450SE  200 4070                Reno
#> 35  Merc 450SE  300 4070             Concord
#> 36  Merc 450SE  600 4070       Atlantic City
#> 37  Merc 450SE  700 4070         Albuquerque
#> 38    Merc 280  100 3440             Buffalo
#> 39    Merc 280  200 3440            New York
#> 40    Merc 280  300 3440           Charlotte
#> 41    Merc 280  600 3440             Raleigh
#> 42    Merc 280  700 3440             Bismark
#> 43 Merc 450SLC  100 3780             Bismark
#> 44 Merc 450SLC  200 3780          Cincinnati
#> 45   Merc 240D  100 3190           Cleveland
#> 46 Merc 450SLC  300 3780           Cleveland
#> 47   Merc 240D  200 3190            Columbus
#> 48 Merc 450SLC  600 3780            Columbus
#> 49   Merc 240D  300 3190       Oklahoma City
#> 50 Merc 450SLC  700 3780       Oklahoma City
#> 51   Merc 240D  700 3190        Philadelphia
#> 52  Merc 450SE  100 4070        Philadelphia
#> 53  Merc 450SE  200 4070           Pittsburg
#> 54  Merc 450SE  300 4070          Providence
#> 55  Merc 450SE  600 4070            Columbia
#> 56  Merc 450SE  700 4070         Sioux Falls
#> 57    Merc 280  100 3440           Nashville
#> 58    Merc 280  200 3440              Dallas
#> 59    Merc 280  300 3440             El Paso
#> 60    Merc 280  600 3440             Houston
#> 61    Merc 280  700 3440      Salt Lake City
#> 62 Merc 450SLC  100 3780      Salt Lake City
#> 63 Merc 450SLC  200 3780          Burlington
#> 64   Merc 240D  100 3190             Norfolk
#> 65 Merc 450SLC  300 3780             Norfolk
#> 66   Merc 240D  200 3190            Richmond
#> 67 Merc 450SLC  600 3780            Richmond
#> 68   Merc 240D  300 3190      Seattle Tacoma
#> 69 Merc 450SLC  700 3780      Seattle Tacoma
#> 70   Merc 240D  600 3190             Spokane
#> 71   Merc 240D  700 3190          Charleston
#> 72  Merc 450SE  100 4070          Charleston
#> 73  Merc 450SE  200 4070           Milwaukee
#> 74  Merc 450SE  300 4070            Cheyenne
#> 75  Merc 450SE  600 4070            San Juan

# More combinations when using second row also
HierarchicalWildcardGlobbing(zFrame, wg[1:2, ])
#>                car  code   wt                city
#> 1       Merc 450SL   700 3730              Mobile
#> 2        Merc 280C   100 3440             Phoenix
#> 3        Merc 280C   200 3440         Little Rock
#> 4        Merc 280C   300 3440         Los Angeles
#> 5        Merc 280C   600 3440          Sacramento
#> 6        Merc 280C   700 3440       San Francisco
#> 7         Merc 230   100 3150            Hartford
#> 8   Ford Pantera L 13401 3170            Hartford
#> 9         Merc 230   200 3150          Wilmington
#> 10  Ford Pantera L 13402 3170          Wilmington
#> 11        Merc 230   300 3150          Washington
#> 12  Ford Pantera L 64701 3170          Washington
#> 13        Merc 230   600 3150        Jacksonville
#> 14  Ford Pantera L 64702 3170        Jacksonville
#> 15        Merc 230   700 3150               Miami
#> 16      Merc 450SL   100 3730               Miami
#> 17      Merc 450SL   200 3730             Atlanta
#> 18      Merc 450SL   300 3730            Honolulu
#> 19      Merc 450SL   600 3730               Boise
#> 20      Merc 450SL   700 3730             Chicago
#> 21       Merc 280C   100 3440        Indianapolis
#> 22       Merc 280C   200 3440          Des Moines
#> 23       Merc 280C   300 3440             Wichita
#> 24       Merc 280C   600 3440          Louisville
#> 25       Merc 280C   700 3440         New Orleans
#> 26       Merc 240D   600 3190            Portland
#> 27        Merc 230   100 3150           Baltimore
#> 28  Ford Pantera L 13401 3170           Baltimore
#> 29        Merc 230   200 3150              Boston
#> 30  Ford Pantera L 13402 3170              Boston
#> 31        Merc 230   300 3150             Detroit
#> 32  Ford Pantera L 64701 3170             Detroit
#> 33        Merc 230   600 3150    Sault Ste. Marie
#> 34  Ford Pantera L 64702 3170    Sault Ste. Marie
#> 35        Merc 230   700 3150              Duluth
#> 36      Merc 450SL   100 3730              Duluth
#> 37      Merc 450SL   200 3730 Minneapolis/St Paul
#> 38      Merc 450SL   300 3730             Jackson
#> 39      Merc 450SL   600 3730         Kansas City
#> 40      Merc 450SL   700 3730            St Louis
#> 41       Merc 280C   100 3440               Omaha
#> 42      Merc 450SE   200 4070                Reno
#> 43      Merc 450SE   300 4070             Concord
#> 44       Fiat X1-9 13401 1935             Concord
#> 45      Merc 450SE   600 4070       Atlantic City
#> 46       Fiat X1-9 13402 1935       Atlantic City
#> 47      Merc 450SE   700 4070         Albuquerque
#> 48       Fiat X1-9 64701 1935         Albuquerque
#> 49       Fiat X1-9 64702 1935              Albany
#> 50        Merc 280   100 3440             Buffalo
#> 51    Ferrari Dino 13401 2770             Buffalo
#> 52        Merc 280   200 3440            New York
#> 53    Ferrari Dino 13402 2770            New York
#> 54        Merc 280   300 3440           Charlotte
#> 55    Ferrari Dino 64701 2770           Charlotte
#> 56        Merc 280   600 3440             Raleigh
#> 57    Ferrari Dino 64702 2770             Raleigh
#> 58        Merc 280   700 3440             Bismark
#> 59     Merc 450SLC   100 3780             Bismark
#> 60        Fiat 128 13401 2200             Bismark
#> 61     Merc 450SLC   200 3780          Cincinnati
#> 62        Fiat 128 13402 2200          Cincinnati
#> 63       Merc 240D   100 3190           Cleveland
#> 64     Merc 450SLC   300 3780           Cleveland
#> 65        Fiat 128 64701 2200           Cleveland
#> 66       Merc 240D   200 3190            Columbus
#> 67     Merc 450SLC   600 3780            Columbus
#> 68        Fiat 128 64702 2200            Columbus
#> 69       Merc 240D   300 3190       Oklahoma City
#> 70     Merc 450SLC   700 3780       Oklahoma City
#> 71       Merc 240D   700 3190        Philadelphia
#> 72      Merc 450SE   100 4070        Philadelphia
#> 73      Merc 450SE   200 4070           Pittsburg
#> 74      Merc 450SE   300 4070          Providence
#> 75       Fiat X1-9 13401 1935          Providence
#> 76      Merc 450SE   600 4070            Columbia
#> 77       Fiat X1-9 13402 1935            Columbia
#> 78      Merc 450SE   700 4070         Sioux Falls
#> 79       Fiat X1-9 64701 1935         Sioux Falls
#> 80       Fiat X1-9 64702 1935             Memphis
#> 81        Merc 280   100 3440           Nashville
#> 82    Ferrari Dino 13401 2770           Nashville
#> 83        Merc 280   200 3440              Dallas
#> 84    Ferrari Dino 13402 2770              Dallas
#> 85        Merc 280   300 3440             El Paso
#> 86    Ferrari Dino 64701 2770             El Paso
#> 87        Merc 280   600 3440             Houston
#> 88    Ferrari Dino 64702 2770             Houston
#> 89        Merc 280   700 3440      Salt Lake City
#> 90     Merc 450SLC   100 3780      Salt Lake City
#> 91        Fiat 128 13401 2200      Salt Lake City
#> 92     Merc 450SLC   200 3780          Burlington
#> 93        Fiat 128 13402 2200          Burlington
#> 94       Merc 240D   100 3190             Norfolk
#> 95     Merc 450SLC   300 3780             Norfolk
#> 96        Fiat 128 64701 2200             Norfolk
#> 97       Merc 240D   200 3190            Richmond
#> 98     Merc 450SLC   600 3780            Richmond
#> 99        Fiat 128 64702 2200            Richmond
#> 100      Merc 240D   300 3190      Seattle Tacoma
#> 101    Merc 450SLC   700 3780      Seattle Tacoma
#> 102      Merc 240D   600 3190             Spokane
#> 103      Merc 240D   700 3190          Charleston
#> 104     Merc 450SE   100 4070          Charleston
#> 105     Merc 450SE   200 4070           Milwaukee
#> 106     Merc 450SE   300 4070            Cheyenne
#> 107      Fiat X1-9 13401 1935            Cheyenne
#> 108     Merc 450SE   600 4070            San Juan
#> 109      Fiat X1-9 13402 1935            San Juan

# Less combinations when using third row also 
# since last digit of wt must be 0 and only cities starting with C
HierarchicalWildcardGlobbing(zFrame, wg[1:3, ])
#>             car  code   wt       city
#> 1    Merc 450SL   700 3730    Chicago
#> 2    Merc 450SE   300 4070    Concord
#> 3      Merc 280   300 3440  Charlotte
#> 4  Ferrari Dino 64701 2770  Charlotte
#> 5   Merc 450SLC   200 3780 Cincinnati
#> 6      Fiat 128 13402 2200 Cincinnati
#> 7     Merc 240D   100 3190  Cleveland
#> 8   Merc 450SLC   300 3780  Cleveland
#> 9      Fiat 128 64701 2200  Cleveland
#> 10    Merc 240D   200 3190   Columbus
#> 11  Merc 450SLC   600 3780   Columbus
#> 12     Fiat 128 64702 2200   Columbus
#> 13   Merc 450SE   600 4070   Columbia
#> 14    Merc 240D   700 3190 Charleston
#> 15   Merc 450SE   100 4070 Charleston
#> 16   Merc 450SE   300 4070   Cheyenne


# Less combinations when using fourth row also since city cannot start with Co
HierarchicalWildcardGlobbing(zFrame, wg[1:4, ])
#>             car  code   wt       city
#> 1    Merc 450SL   700 3730    Chicago
#> 2      Merc 280   300 3440  Charlotte
#> 3  Ferrari Dino 64701 2770  Charlotte
#> 4   Merc 450SLC   200 3780 Cincinnati
#> 5      Fiat 128 13402 2200 Cincinnati
#> 6     Merc 240D   100 3190  Cleveland
#> 7   Merc 450SLC   300 3780  Cleveland
#> 8      Fiat 128 64701 2200  Cleveland
#> 9     Merc 240D   700 3190 Charleston
#> 10   Merc 450SE   100 4070 Charleston
#> 11   Merc 450SE   300 4070   Cheyenne

# Less combinations when using fourth row also 
# since specific combinations of city and code are removed
HierarchicalWildcardGlobbing(zFrame, wg)
#>           car  code   wt       city
#> 1  Merc 450SL   700 3730    Chicago
#> 2    Merc 280   300 3440  Charlotte
#> 3 Merc 450SLC   200 3780 Cincinnati
#> 4   Merc 240D   100 3190  Cleveland
#> 5 Merc 450SLC   300 3780  Cleveland
#> 6    Fiat 128 64701 2200  Cleveland
#> 7   Merc 240D   700 3190 Charleston
#> 8  Merc 450SE   100 4070 Charleston
#> 9  Merc 450SE   300 4070   Cheyenne


# =================================================================== 
#  Using list input to create all possible combinations
# ===================================================================

dim(HierarchicalWildcardGlobbing(zList, wg))
#> [1] 4788    4

# same result with as.list since same unique values of each variable
dim(HierarchicalWildcardGlobbing(as.list(zFrame), wg))
#> [1] 4788    4
```
