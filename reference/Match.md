# Matching rows in data frames

The algorithm is based on converting variable combinations to whole
numbers. The final matching is performed using
[`match`](https://rdrr.io/r/base/match.html).

## Usage

``` r
Match(x, y)
```

## Arguments

- x:

  data frame

- y:

  data frame

## Value

An integer vector giving the position in y of the first match if there
is a match, otherwise NA.

## Details

When the result of multiplying together the number of unique values in
each column of x exceeds 9E15 (largest value stored exactly by the
numeric data type), the algorithm is recursive.

## Author

Øyvind Langsrud

## Examples

``` r
a <- data.frame(x = c("a", "b", "c"), y = c("A", "B"), z = 1:6)
b <- data.frame(x = c("b", "c"), y = c("B", "K", "A", "B"), z = c(2, 3, 5, 6))

Match(a, b)
#> [1] NA  1 NA NA  3  4
Match(b, a)
#> [1]  2 NA  5  6

# Slower alternative
match(data.frame(t(a), stringsAsFactors = FALSE), data.frame(t(b), stringsAsFactors = FALSE))
#> [1] NA  1 NA NA  3  4
match(data.frame(t(b), stringsAsFactors = FALSE), data.frame(t(a), stringsAsFactors = FALSE))
#> [1]  2 NA  5  6

# More comprehensive example (n, m and k may be changed)
n <- 10^4
m <- 10^3
k <- 10^2
data(precip)
data(mtcars)
y <- data.frame(car = sample(rownames(mtcars), n, replace = TRUE), 
                city = sample(names(precip), n, replace = TRUE),
                n = rep_len(1:k, n), a = rep_len(c("A", "B", "C", "D"), n),
                b = rep_len(as.character(rnorm(1000)), n),
                d = sample.int(k + 10, n, replace = TRUE),
                e = paste(sample.int(k * 2, n, replace = TRUE), 
                          rep_len(c("Green", "Red", "Blue"), n), sep = "_"),
                r = rnorm(k)^99)
x <- y[sample.int(n, m), ]
row.names(x) <- NULL
ix <- Match(x, y)
```
