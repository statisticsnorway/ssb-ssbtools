# An `iFunction` argument to [`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.md)

Use this function as `iFunction` or write your own using the same seven
first parameters and also using `...`.

## Usage

``` r
GaussIterationFunction(i, I, j, J, true, false, na, filename = NULL, ...)
```

## Arguments

- i:

  Number of candidates processed (columns of `x`)

- I:

  Total number of candidates to be processed (columns of `x`)

- j:

  Number of eliminated dimensions (rows of `x`)

- J:

  Total number of dimensions (rows of `x`)

- true:

  Candidates decided to be suppressed

- false:

  Candidates decided to be not suppressed

- na:

  Candidates not decided

- filename:

  When non-NULL, the above arguments will be saved to this file. Note
  that `GaussSuppression` passes this parameter via `...`.

- ...:

  Extra parameters

## Value

`NULL`

## Details

The number of candidates decided (`true` and `false`) may differ from
the number of candidates processed (`i`) due to parameter
`removeDuplicated` and because the decision for some unprocessed
candidates can be found due to empty columns.
