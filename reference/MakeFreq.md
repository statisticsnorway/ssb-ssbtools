# Microdata frequency data conversion

Microdata frequency data conversion

## Usage

``` r
MakeFreq(x, freqName = "freq", all = FALSE, hierarchies = FALSE)

MakeMicro(x, freqVar)
```

## Arguments

- x:

  A data frame

- freqName:

  Name of new frequency variable in output data

- all:

  Whether to include level combinations not in microdata (zero
  frequency)

- hierarchies:

  Whether to treat hierarchical variables automatically when `all=TRUE`

- freqVar:

  The frequency variable in input data, name or number.

## Value

A data frame

## Examples

``` r
z <- SSBtoolsData("sprt_emp")[c(1, 2, 4, 7:12, 15, 17, 18), -4]
z$eu <- z$geo != "Iceland"
z
#>       age      geo year    eu
#> 1  Y15-29    Spain 2014  TRUE
#> 2  Y15-29  Iceland 2014 FALSE
#> 4  Y30-64    Spain 2014  TRUE
#> 7  Y15-29    Spain 2015  TRUE
#> 8  Y15-29  Iceland 2015 FALSE
#> 9  Y15-29 Portugal 2015  TRUE
#> 10 Y30-64    Spain 2015  TRUE
#> 11 Y30-64  Iceland 2015 FALSE
#> 12 Y30-64 Portugal 2015  TRUE
#> 15 Y15-29 Portugal 2016  TRUE
#> 17 Y30-64  Iceland 2016 FALSE
#> 18 Y30-64 Portugal 2016  TRUE

MakeFreq(z)
#>       age      geo year    eu freq
#> 1  Y15-29  Iceland 2014 FALSE    1
#> 2  Y15-29  Iceland 2015 FALSE    1
#> 3  Y15-29 Portugal 2015  TRUE    1
#> 4  Y15-29 Portugal 2016  TRUE    1
#> 5  Y15-29    Spain 2014  TRUE    1
#> 6  Y15-29    Spain 2015  TRUE    1
#> 7  Y30-64  Iceland 2015 FALSE    1
#> 8  Y30-64  Iceland 2016 FALSE    1
#> 9  Y30-64 Portugal 2015  TRUE    1
#> 10 Y30-64 Portugal 2016  TRUE    1
#> 11 Y30-64    Spain 2014  TRUE    1
#> 12 Y30-64    Spain 2015  TRUE    1
MakeFreq(z[, -2])
#>       age year    eu freq
#> 1  Y15-29 2014 FALSE    1
#> 2  Y15-29 2014  TRUE    1
#> 3  Y15-29 2015 FALSE    1
#> 4  Y15-29 2015  TRUE    2
#> 5  Y15-29 2016  TRUE    1
#> 6  Y30-64 2014  TRUE    1
#> 7  Y30-64 2015 FALSE    1
#> 8  Y30-64 2015  TRUE    2
#> 9  Y30-64 2016 FALSE    1
#> 10 Y30-64 2016  TRUE    1
MakeFreq(z[, -(2:3)])
#>      age    eu freq
#> 1 Y15-29 FALSE    2
#> 2 Y15-29  TRUE    4
#> 3 Y30-64 FALSE    2
#> 4 Y30-64  TRUE    4
MakeFreq(z[, -1])
#>        geo year    eu freq
#> 1  Iceland 2014 FALSE    1
#> 2  Iceland 2015 FALSE    2
#> 3  Iceland 2016 FALSE    1
#> 4 Portugal 2015  TRUE    2
#> 5 Portugal 2016  TRUE    2
#> 6    Spain 2014  TRUE    2
#> 7    Spain 2015  TRUE    2
MakeFreq(z[, -1], all = TRUE)
#>         geo year    eu freq
#> 1     Spain 2014  TRUE    2
#> 2   Iceland 2014  TRUE    0
#> 3  Portugal 2014  TRUE    0
#> 4     Spain 2015  TRUE    2
#> 5   Iceland 2015  TRUE    0
#> 6  Portugal 2015  TRUE    2
#> 7     Spain 2016  TRUE    0
#> 8   Iceland 2016  TRUE    0
#> 9  Portugal 2016  TRUE    2
#> 10    Spain 2014 FALSE    0
#> 11  Iceland 2014 FALSE    1
#> 12 Portugal 2014 FALSE    0
#> 13    Spain 2015 FALSE    0
#> 14  Iceland 2015 FALSE    2
#> 15 Portugal 2015 FALSE    0
#> 16    Spain 2016 FALSE    0
#> 17  Iceland 2016 FALSE    1
#> 18 Portugal 2016 FALSE    0

x <- MakeFreq(z[, -1], all = TRUE, hierarchies = TRUE)
x
#>        geo    eu year freq
#> 1    Spain  TRUE 2014    2
#> 2  Iceland FALSE 2014    1
#> 3 Portugal  TRUE 2014    0
#> 4    Spain  TRUE 2015    2
#> 5  Iceland FALSE 2015    2
#> 6 Portugal  TRUE 2015    2
#> 7    Spain  TRUE 2016    0
#> 8  Iceland FALSE 2016    1
#> 9 Portugal  TRUE 2016    2

MakeMicro(x, "freq")
#>         geo    eu year freq
#> 1     Spain  TRUE 2014    1
#> 2     Spain  TRUE 2014    1
#> 3   Iceland FALSE 2014    1
#> 4     Spain  TRUE 2015    1
#> 5     Spain  TRUE 2015    1
#> 6   Iceland FALSE 2015    1
#> 7   Iceland FALSE 2015    1
#> 8  Portugal  TRUE 2015    1
#> 9  Portugal  TRUE 2015    1
#> 10  Iceland FALSE 2016    1
#> 11 Portugal  TRUE 2016    1
#> 12 Portugal  TRUE 2016    1
```
