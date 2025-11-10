# Apply a function to subsets defined by a dummy matrix

For each column, `i`, of the matrix `x` of zeros and ones, the output
value is equivalent to `FUN(y[x[, i] != 0])`.

## Usage

``` r
DummyApply(x, y, FUN = sum, simplify = TRUE)
```

## Arguments

- x:

  A (sparse) dummy matrix

- y:

  Vector of input values

- FUN:

  A function

- simplify:

  Parameter to [`aggregate`](https://rdrr.io/r/stats/aggregate.html).
  When `FALSE`, list output is ensured.

## Value

Vector of output values or a matrix when multiple outputs from `FUN`
(see examples). List output is also possible (ensured when
`simplify = FALSE`).

## Details

With a dummy `x` and `FUN = sum`, output is equivalent to
`z = t(x) %*% y`.

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"

a <- ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)

cbind(as.data.frame(a$crossTable), 
      sum1 = (Matrix::t(a$modelMatrix) %*% z$ths_per)[,1],
      sum2 = DummyApply(a$modelMatrix, z$ths_per, sum),
       max = DummyApply(a$modelMatrix, z$ths_per, max))
#>                  age      geo  sum1  sum2   max
#> Total-Total    Total    Total 680.8 680.8 122.1
#> old-Total        old    Total 437.3 437.3 122.1
#> young-Total    young    Total 243.5 243.5  69.1
#> Total-Iceland  Total  Iceland  10.6  10.6   1.9
#> Total-Portugal Total Portugal 108.8 108.8  25.8
#> Total-Spain    Total    Spain 561.4 561.4 122.1
       
DummyApply(a$modelMatrix, z$ths_per, range)
#>      [,1]  [,2]
#> [1,]  1.5 122.1
#> [2,]  1.5 122.1
#> [3,]  1.8  69.1
#> [4,]  1.5   1.9
#> [5,] 11.6  25.8
#> [6,] 63.4 122.1
DummyApply(a$modelMatrix, z$ths_per, range, simplify = FALSE)  
#> [[1]]
#> [1]   1.5 122.1
#> 
#> [[2]]
#> [1]   1.5 122.1
#> 
#> [[3]]
#> [1]  1.8 69.1
#> 
#> [[4]]
#> [1] 1.5 1.9
#> 
#> [[5]]
#> [1] 11.6 25.8
#> 
#> [[6]]
#> [1]  63.4 122.1
#> 

a$modelMatrix[, c(3, 5)] <- 0   # Introduce two empty columns. 
DummyApply(a$modelMatrix, z$ths_per, function(x){ 
  c(min = min(x), 
    max = max(x), 
    mean = mean(x), 
    median = median(x), 
    n = length(x))})   
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#>       min   max      mean median  n
#> [1,]  1.5 122.1 37.822222  17.20 18
#> [2,]  1.5 122.1 48.588889  24.30  9
#> [3,]  Inf  -Inf       NaN     NA  0
#> [4,]  1.5   1.9  1.766667   1.85  6
#> [5,]  Inf  -Inf       NaN     NA  0
#> [6,] 63.4 122.1 93.566667  94.35  6
    
DummyApply(a$modelMatrix, z$ths_per, function(x) x, simplify = FALSE)          
#> [[1]]
#>  [1]  66.9   1.8  11.6 120.3   1.5  20.2  63.4   1.9  14.2 119.6   1.6  24.3
#> [13]  69.1   1.9  12.7 122.1   1.9  25.8
#> 
#> [[2]]
#> [1] 120.3   1.5  20.2 119.6   1.6  24.3 122.1   1.9  25.8
#> 
#> [[3]]
#> numeric(0)
#> 
#> [[4]]
#> [1] 1.8 1.5 1.9 1.6 1.9 1.9
#> 
#> [[5]]
#> numeric(0)
#> 
#> [[6]]
#> [1]  66.9 120.3  63.4 119.6  69.1 122.1
#> 
```
