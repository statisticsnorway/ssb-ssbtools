#
# Below are internal functions for GaussSuppression relataed to:
#
#    Singleton methods for frequency tables for cases where 
#    non-suppressed singletons (zeros) are allowed
#
# In a separate file for a better overview.
#


# Simplified version of AnyProportionalGaussInt 
# The function checks whether secondary suppression is needed in cases where 
# the alternative is that the sum of singletons can be revealed or published
Any0GaussInt <- function(r, rB) {
  for (i in seq_along(rB)) {
    ni <- length(rB[[i]])
    if (ni) {    
      if( all(rB[[i]] %in% r) )
        return(TRUE)
    }
  }
  FALSE
}




FindDiffMatrix <- function(x, y = x, max_colSums_diff = Inf) {
  xty <- As_TsparseMatrix(crossprod(x, y))
  colSums_y_xty_j_1 <- colSums(y)[xty@j + 1]
  # finds children in x and parents in y
  r <- colSums(x)[xty@i + 1] == xty@x & 
    colSums_y_xty_j_1     != xty@x & 
    (colSums_y_xty_j_1 - xty@x) <= max_colSums_diff
  child <- xty@i[r] + 1L
  parent <- xty@j[r] + 1L
  diff_matrix <- y[, parent, drop = FALSE] - 
    x[, child, drop = FALSE]
  colnames(diff_matrix) <- parent
  diff_matrix
}