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
  if (!(length(r))) {
    return(TRUE)  # Empty regarded as proportional. As in AnyProportionalGaussInt and needed here since ParentChildExtension.
  }
  for (i in seq_along(rB)) {
    ni <- length(rB[[i]])
    if (ni) {    
      if( all(rB[[i]] %in% r) )
        return(TRUE)
    }
  }
  FALSE
}


# Extend rows, r, to be checked by Any0GaussInt using “parentChildSingleton”
# Parents and childs can be found in both A (candidates) and B (primary)
# easy1 means possible quick extension decision since length-1 difference must be a meaningful child
ParentChildExtension1 <- function(r, rA, rB, pc, easy1) {
  # pc = parentChildSingleton
  r_new <- integer(0)
  nr <- length(r)
  rP <- rA[pc$unique_parent[pc$unique_parent > 0]]
  nAparent <- length(rP)
  rP <- c(rP, rB[-pc$unique_parent[pc$unique_parent < 0]])
  for (i in seq_along(rP)) {
    if (any(r %in% rP[[i]])) {
      # Now a parent is found
      rD <- rP[[i]][!(rP[[i]] %in% r)] # rD is difference between parent and (parts of) r
      length_rD <- length(rD)
      if (length_rD) {
        if (easy1 & length_rD == 1) {
          r_new <- c(r_new, rD)
        } else {
          # Going to check childs
          childi <- pc$child[pc$match_parent == i]
          rC <- rA[childi[childi > 0]]
          nAchild <- length(rC)
          rC <- c(rC, rB[-childi[childi < 0]])
          for (k in rev(seq_along(rC))) {  # rev since fast decision is seen earlier
            if (length_rD == length(rC[[k]])) {
              if (all(rD == rC[[k]])) {    # uses knowledge that r is sorted
                if (k > nAchild) {
                  return(integer(0))  # Fast decision when primary found. Empty regarded as proportional in Any0GaussInt.
                }
                r_new <- c(r_new, rD)
                break
              }
            }
          }
        }
      }
    }
  }
  if (length(r_new)) {
    return(sort(c(r, unique(r_new))))
  }
  r
}


ParentChildExtension <- function(r, rA, rB, pc, easy1, maxiter) {
  rold <- r
  for (i in seq_len(maxiter)) {
    r <- ParentChildExtension1(rold, rA, rB, pc, easy1)
    if (length(r) == length(rold)) {
      return(r)
    }
    rold <- r
  }
  warning("Iteration limit exceeded in anySum0 routine.")
  r
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


# Special parent-child for singletons
# Parents and childs can be found in both A (candidates) and B (primary) 
# Special indices in output, candidates as positive, primary as negative  
# In addition to parent and child, extra output elements to be used later included
FindParentChildSingleton <- function(x, candidates, primary, singleton, 
                                     ncol_x_input, idxDD) {
  if (!any(singleton)) {
    return(NULL)
  }
  candidates_primary <- c(candidates, primary)
  ncol_x <- Ncol_x_input_removedDuplicated(ncol_x_input, idxDD) 
  colSums_x <- colSums(x)[seq_len(ncol_x)]
  colSums_x_singleton <- colSums(x[singleton, seq_len(ncol_x), drop = FALSE])
  colSingleton <- which(colSums_x_singleton > 0L & colSums_x_singleton == colSums_x)
  colSingleton <- colSingleton[colSingleton %in% candidates_primary]
  if (!length(colSingleton)) {
    return(NULL)
  }
  pc <- FindParentChild(x[, colSingleton, drop = FALSE])
  if (!length(pc$child)) {
    return(NULL)
  }
  pc$parent <- match(colSingleton[pc$parent], candidates_primary)
  pc$child <- match(colSingleton[pc$child], candidates_primary)
  order_parent <- order(pc$parent)      # order needed since candidates first and primary last  
  pc$parent <- pc$parent[order_parent]  # assumed in ParentChildExtension 
  pc$child <- pc$child[order_parent] 
  lc <- length(candidates)
  pc$parent[pc$parent > lc] <- -(pc$parent[pc$parent > lc] - lc)  # primary as negative indices  
  pc$child[pc$child > lc] <- -(pc$child[pc$child > lc] - lc)
  pc$unique_parent <- unique(pc$parent)
  pc$match_parent <- match(pc$parent, pc$unique_parent)
  pc$uniqueA <- unique(c(pc$unique_parent, pc$child))
  pc$uniqueA <- sort(pc$uniqueA[pc$uniqueA > 0])
  col1 <- colSums(x[singleton, colSingleton, drop = FALSE]) == 1
  row1 <- rowSums(x[singleton, colSingleton[col1], drop = FALSE]) > 0
  pc$all1 <- all(row1)  # TRUE when all rows included as childs 
  pc
}

# parent-child relevant for FindParentChildSingleton will initially be without xExtraPrimary
# The number of columns after using removeDuplicated must be calculated
Ncol_x_input_removedDuplicated <- function(ncol_x_input, idxDD) {
  if (is.null(idxDD)) {
    return(ncol_x_input)
  }
  length(unique(idxDD[seq_len(ncol_x_input)]))
}












