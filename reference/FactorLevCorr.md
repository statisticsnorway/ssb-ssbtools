# Factor level correlation

A sort of correlation matrix useful to detect (hierarchical)
relationships between the levels of factor variables.

## Usage

``` r
FactorLevCorr(x)
```

## Arguments

- x:

  Input matrix or data frame containing the variables

## Value

Output is a sort of correlation matrix.

Here we refer to ni as the number of present levels of variable i (the
number of unique elements) and we refer to mij as the number of present
levels obtained by crossing variable i and variable j (the number unique
rows of x\[,c(i,j)\]).

The diagonal elements of the output matrix contains the number of
present levels of each variable (=ni).

The absolute values of off-diagonal elements:

- 0:

  when mij = ni\*nj

- 1:

  when mij = max(ni,nj)

- Other values:

  Computed as (ni\*nj-mij)/(ni\*nj-max(ni,nj))

So 0 means that all possible level combinations exist in the data and 1
means that the two variables are hierarchically related.

The sign of off-diagonal elements:

- positive:

  when ni\<nj

- negative:

  when ni\>nj

In cases where ni=nj elements will be positive above the diagonal and
negative below.

## Author

Øyvind Langsrud

## Examples

``` r
 x <- rep(c("A","B","C"),3)
 y <- rep(c(11,22,11),3)
 z <- c(1,1,1,2,2,2,3,3,3)
 zy <- paste(z,y,sep="")
 m <- cbind(x,y,z,zy)
 FactorLevCorr(m)
#>        x  y  z   zy
#> x   3.00 -1  0 0.75
#> y   1.00  2  0 1.00
#> z   0.00  0  3 1.00
#> zy -0.75 -1 -1 6.00
```
