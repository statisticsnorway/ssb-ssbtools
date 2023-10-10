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