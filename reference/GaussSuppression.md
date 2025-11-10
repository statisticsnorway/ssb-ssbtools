# Secondary suppression by Gaussian elimination

Sequentially the secondary suppression candidates (columns in x) are
used to reduce the x-matrix by Gaussian elimination. Candidates who
completely eliminate one or more primary suppressed cells (columns in x)
are omitted and made secondary suppressed. This ensures that the primary
suppressed cells do not depend linearly on the non-suppressed cells. How
to order the input candidates is an important choice. The singleton
problem and the related problem of zeros are also handled.

## Usage

``` r
GaussSuppression(
  x,
  candidates = 1:ncol(x),
  primary = NULL,
  forced = NULL,
  hidden = NULL,
  singleton = rep(FALSE, nrow(x)),
  singletonMethod = "anySum",
  printInc = TRUE,
  tolGauss = (.Machine$double.eps)^(1/2),
  whenEmptySuppressed = warning,
  whenEmptyUnsuppressed = message,
  whenPrimaryForced = warning,
  removeDuplicated = TRUE,
  iFunction = GaussIterationFunction,
  iWait = Inf,
  xExtraPrimary = NULL,
  unsafeAsNegative = FALSE,
  printXdim = FALSE,
  cell_grouping = NULL,
  table_id = NULL,
  auto_anySumNOTprimary = TRUE,
  auto_subSumAny = TRUE,
  ...
)
```

## Arguments

- x:

  Matrix that relates cells to be published or suppressed to inner
  cells. yPublish = crossprod(x,yInner)

- candidates:

  Indices of candidates for secondary suppression

- primary:

  Indices of primary suppressed cells

- forced:

  Indices forced to be not suppressed. `forced` has precedence over
  `primary`. See `whenPrimaryForced` below.

- hidden:

  Indices to be removed from the above `candidates` input (see details)

- singleton:

  Logical or integer vector of length `nrow(x)` specifying inner cells
  for singleton handling. Normally, for frequency tables, this means
  cells with 1s when 0s are non-suppressed and cells with 0s when 0s are
  suppressed. For some singleton methods, integer values representing
  the unique magnitude table contributors are needed. For all other
  singleton methods, only the values after conversion with `as.logical`
  matter.

- singletonMethod:

  Method for handling the problem of singletons and zeros: `"anySum"`
  (default), `"anySum0"`, `"anySumNOTprimary"`, `"subSum"`,
  `"subSpace"`, `"sub2Sum"`, `"none"` or a
  [`NumSingleton`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.md)
  method (see details).

- printInc:

  Printing "..." to console when TRUE

- tolGauss:

  A tolerance parameter for sparse Gaussian elimination and linear
  dependency. This parameter is used only in cases where integer
  calculation cannot be used.

- whenEmptySuppressed:

  Function to be called when empty input to primary suppressed cells is
  problematic. Supply NULL to do nothing.

- whenEmptyUnsuppressed:

  Function to be called when empty input to candidate cells may be
  problematic. Supply NULL to do nothing.

- whenPrimaryForced:

  Function to be called if any forced cells are primary suppressed
  (suppression will be ignored). Supply NULL to do nothing. The same
  function will also be called when there are forced cells marked as
  singletons (will be ignored).

- removeDuplicated:

  Specifies whether to remove duplicated columns and rows in `x` before
  running the main algorithm. Removing duplicates results in a faster
  algorithm while generally maintaining the same results. In some cases,
  singleton handling for magnitude tables may be affected. In such
  cases, singleton handling will generally be improved. Singletons are
  considered when removing duplicate rows, so not all duplicates are
  removed. The available options for `removeDuplicated` are as follows:

  - `TRUE` (default): Removes both duplicate columns and rows.

  - `FALSE`: No removal of duplicates.

  - `"cols"`: Removes only duplicate columns.

  - `"rows"`: Removes only duplicate rows.

  - `"rows2"`: Removes only duplicate non-singleton rows in a way that
    preserves singleton handling.

  - Combined possibilities: Variants can be combined with "\_". For
    example, `"cols_rows"` is equivalent to `TRUE`, and `"cols_rows2"`
    represents an alternative variant. Combining `"rows"` and `"rows2"`
    is possible, but superfluous calculations are then performed.

  - `"test"`: A special variant for testing purposes. The four
    configurations `TRUE`, `FALSE`, `"cols_rows2"`, and `"rows"` are
    executed.

- iFunction:

  A function to be called during the iterations. See the default
  function,
  [`GaussIterationFunction`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussIterationFunction.md),
  for description of parameters.

- iWait:

  The minimum number of seconds between each call to `iFunction`.
  Whenever `iWait<Inf`, `iFunction` will also be called after last
  iteration.

- xExtraPrimary:

  Extra x-matrix that defines extra primary suppressed cells in addition
  to those defined by other inputs.

- unsafeAsNegative:

  When `TRUE`, unsafe primary cells due to forced cells are included in
  the output vector as negative indices.

- printXdim:

  When set to `TRUE`, the `printInc` parameter is also automatically set
  to `TRUE`. Additionally, the dimensions of the `x` matrix are printed
  twice: first, the dimensions of the input `x`, potentially extended
  with `xExtraPrimary`; second, the dimensions after applying
  `singletonMethod` and `removeDuplicated`.

- cell_grouping:

  Numeric vector indicating suppression group membership. Cells with the
  same non-zero value belong to the same suppression group, meaning they
  will be suppressed or non-suppressed together. A value of 0 indicates
  that the cell is not a member of any suppression group.

- table_id:

  A parameter that can be provided in addition to `cell_grouping` to
  reduce computation time, when `x` is a block-diagonal matrix. Each
  block represents a separate table, and `table_id` indicates table
  affiliation. Note: no check is performed to verify that `table_id`
  corresponds to the block structure of `x`.

- auto_anySumNOTprimary:

  When `TRUE` (default), the `singletonMethod` `"anySumNOTprimary"` may
  be forced if a check indicates that singletons are not primary
  suppressed. Set this to `FALSE` in cases where the `x` matrix has
  already undergone duplicate row removal, as the check may then produce
  incorrect results.

- auto_subSumAny:

  When `TRUE` (default), and `singletonMethod` is `"anySum"`, it is
  internally changed to `"subSumAny"` if there are forced cells. This is
  done to give information about unsafe cells.

- ...:

  Extra unused parameters

## Value

Secondary suppression indices

## Details

It is possible to specify too many (all) indices as `candidates`.
Indices specified as `primary` or `hidded` will be removed. Hidden
indices (not candidates or primary) refer to cells that will not be
published, but do not need protection.

- **Singleton methods for frequency tables:** All singleton methods,
  except `"sub2Sum"` and the
  [`NumSingleton`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.md)
  methods, have been implemented with frequency tables in mind. The
  singleton method `"subSum"` makes new virtual primary suppressed
  cells, which are the sum of the singletons within each group. The
  `"subSpace"` method is conservative and ignores the singleton
  dimensions when looking for linear dependency. The default method,
  `"anySum"`, is between the other two. Instead of making virtual cells
  of sums within groups, the aim is to handle all possible sums, also
  across groups. In addition, `"subSumSpace"` and `"subSumAny"` are
  possible methods, primarily for testing. These methods are similar to
  `"subSpace"` and `"anySum"`, and additional cells are created as in
  `"subSum"`. It is believed that the extra cells are redundant. Note
  that in order to give information about unsafe cells, `"anySum"` is
  internally changed to `"subSumAny"` when there are forced cells. All
  the above methods assume that any published singletons are primary
  suppressed. If this is not the case, either `"anySumNOTprimary"` or
  `"anySum0"` must be used. Notably, `"anySum0"` is an enhancement of
  `"anySumNOTprimary"` for situations where zeros are singletons. Using
  that method avoids suppressing a zero marginal along with only one of
  its children.

- **Singleton methods for magnitude tables:** The singleton method
  `"sub2Sum"` makes new virtual primary suppressed cells, which are the
  sum of two inner cells. This is done when a group contains exactly two
  primary suppressed inner cells provided that at least one of them is
  singleton. This was the first method implemented. Other magnitude
  methods follow the coding according to
  [`NumSingleton`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.md).
  The `"sub2Sum"` method is equivalent to `"numFFT"`. Also note that
  `"num"`, `"numFFF"` and `"numFTF"` are equivalent to `"none"`.

- **Combined:** For advanced use, `singleton` can be a two-element list
  with names `"freq"` and `"num"`. Then `singletonMethod` must be a
  corresponding named two-element vector. For example:
  `singletonMethod = c(freq = "anySumNOTprimary", num = "sub2Sum")`

## References

Langsrud, Ø. (2024): “Secondary Cell Suppression by Gaussian
Elimination: An Algorithm Suitable for Handling Issues with Zeros and
Singletons”. Presented at: *Privacy in statistical databases*, Antibes,
France. September 25-27, 2024.
[doi:10.1007/978-3-031-69651-0_6](https://doi.org/10.1007/978-3-031-69651-0_6)

## Examples

``` r
# Input data
df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
                 var1 = rep(1:3, each = 5), 
                 var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)

# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))

# Add primary suppression 
datF$primary <- datF$values
datF$primary[datF$values < 5 & datF$values > 0] <- NA
datF$suppressedA <- datF$primary
datF$suppressedB <- datF$primary
datF$suppressedC <- datF$primary

# zero secondary suppressed
datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA
#> GaussSuppression_anySum: .....................

# zero not secondary suppressed by first in ordering
datF$suppressedB[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
                            primary = is.na(datF$primary))] <- NA
#> GaussSuppression_anySum: .....................

# with singleton
datF$suppressedC[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
                            primary = is.na(datF$primary), singleton = df$values == 1)] <- NA
#> GaussSuppression_anySum: .....................

datF
#>     var1  var2 values primary suppressedA suppressedB suppressedC
#> 1  Total Total     72      72          72          72          72
#> 2      1 Total     13      13          13          13          13
#> 3      2 Total     45      45          45          45          45
#> 4      3 Total     14      14          14          14          14
#> 5  Total     A     10      10          10          10          10
#> 6  Total     B     10      10          10          10          10
#> 7  Total     C     10      10          10          10          10
#> 8  Total     D     21      21          21          21          21
#> 9  Total     E     21      21          21          21          21
#> 10     1     A      1      NA          NA          NA          NA
#> 11     1     B      1      NA          NA          NA          NA
#> 12     1     C      1      NA          NA          NA          NA
#> 13     1     D      5       5           5           5           5
#> 14     1     E      5       5           5           5          NA
#> 15     2     A      9       9           9          NA          NA
#> 16     2     B      9       9           9          NA          NA
#> 17     2     C      9       9           9          NA          NA
#> 18     2     D      9       9           9           9           9
#> 19     2     E      9       9           9           9          NA
#> 20     3     A      0       0          NA           0           0
#> 21     3     B      0       0          NA           0           0
#> 22     3     C      0       0          NA           0           0
#> 23     3     D      7       7           7           7           7
#> 24     3     E      7       7           7           7           7



#### with cell_grouping

candidates <- c(which(datF$values == 0), which(datF$values > 0))
primary <- 10:12
cell_grouping <- rep(0, 24)

# same as without cell_grouping
GaussSuppression(x, candidates, primary, cell_grouping = cell_grouping)
#> GaussSuppression_anySum: .....................
#> [1] 15 16 17

cell_grouping[c(16, 20:21)] <- 1
cell_grouping[c(10, 4)] <- 2  # 10 is primary

GaussSuppression(x, candidates, primary, cell_grouping = cell_grouping)
#> GaussSuppression_anySum: ....................
#> [1]  3  4  6  9 15 17 24
```
