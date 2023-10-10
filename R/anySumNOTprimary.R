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




# Some of the code is similar to GaussSuppression:::FindDifferenceCells
# The code is based on SSBtools:::FindDiffMatrix (in file GaussSuppression.R)
# Example: mm <- ModelMatrix(SSBtoolsData("sprt_emp_withEU")[1:6, 1:2])
#          FindParentChild(mm[, c(1, 5, 6)])
FindParentChild <- function(x) {
  xtx <- As_TsparseMatrix(crossprod(x))
  colSums_x_xtx_j_1 <- colSums(x)[xtx@j + 1]
  r <- colSums(x)[xtx@i + 1] == xtx@x & colSums_x_xtx_j_1 != xtx@x 
  child <- xtx@i[r] + 1L
  parent <- xtx@j[r] + 1L
  list(parent = parent, child = child)
}