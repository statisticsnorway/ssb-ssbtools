# Weighted quantiles

The default method (`type=2`) corresponds to weighted percentiles in
SAS.

## Usage

``` r
quantile_weighted(
  x,
  probs = (0:4)/4,
  weights = rep(1, length(x)),
  type = 2,
  eps = 1e-09
)
```

## Arguments

- x:

  Numeric vector

- probs:

  Numeric vector of probabilities

- weights:

  Numeric vector of weights of the same length as `x`

- type:

  An integer, `2` (default) or `5`. Similar to types 2 and 5 in
  [`quantile`](https://rdrr.io/r/stats/quantile.html).

- eps:

  Precision parameter used when `type=2` so that numerical inaccuracy is
  accepted (see details)

## Value

Quantiles as a named numeric vector.

## Details

When `type=2`, averaging is used in case of equal of probabilities.
Equal probabilities (`p[j]==probs[i]`) is determined by
`abs(1-p[j]/probs[i])<eps` with `p=cumsum(w)/sum(w)` where
`w=weights[order(x)]`.

With zero length of `x`, `NA`s are returned.

When all weights are zero and when when all `x`'s are not equal, `NaN`s
are returned except for the 0% and 100% quantiles.

## Note

Type 2 similar to type 5 in `DescTools::Quantile`

## Examples

``` r
x <- rnorm(27)/5 + 1:27
w <- (1:27)/27

quantile_weighted(x, (0:5)/5, weights = w)
#>        0%       20%       40%       60%       80%      100% 
#>  1.198987 11.932099 17.139868 20.893309 24.929626 26.667965 
quantile_weighted(x, (0:5)/5, weights = w, type = 5)
#>        0%       20%       40%       60%       80%      100% 
#>  1.198987 12.241779 17.385632 21.183700 24.492988 26.667965 

quantile_weighted(x) - quantile(x, type = 2)
#>   0%  25%  50%  75% 100% 
#>    0    0    0    0    0 
quantile_weighted(x, type = 5) - quantile(x, type = 5)
#>   0%  25%  50%  75% 100% 
#>    0    0    0    0    0 
```
