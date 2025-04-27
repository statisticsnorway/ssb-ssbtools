
################################################################################
# This file provides helper functions related to GaussSuppression functionality,
# including functions for linked table methodology and cell_grouping.
################################################################################


# Replaces non-zero elements occurring at least twice 
# with unique integer group codes; all others become 0.
repeated_as_integer <- function(a) {
  a_non0 <- a[a != 0]
  a_dup <- a %in% unique(a_non0[duplicated(a_non0)])
  b <- rep(0L, length(a))
  b[a_dup] <- as.integer(factor(a[a_dup]))
  b
}



update_cell_grouping_from_duplicated <- function(cell_grouping, idxDD, idxDDunique = unique(idxDD)) {
  cg_non0 <- cell_grouping != 0
  rg <- RowGroups(cbind(cell_grouping[cg_non0]), returnGroups = TRUE)  # similar to factor
  rg_groups <- as.vector(rg$groups)
  for (idx in idxDDunique) {
    groups_to_merge <- rg_groups %in% cell_grouping[idxDD == idx]
    if (any(groups_to_merge)) {
      rg_groups[groups_to_merge] <- max(rg_groups[groups_to_merge])
    }
  }
  cell_grouping[cg_non0] <- rg_groups[rg$idx]
  ma <- match(idxDD[!cg_non0], idxDD[cg_non0])
  cell_grouping[!cg_non0][!is.na(ma)] <- cell_grouping[cg_non0][ma[!is.na(ma)]]
  cell_grouping
}




# Verifies that the cell grouping is consistent with the provided selection.
# If an inconsistency is found, it stops with an error message.
check_consistent_cell_grouping <- function(cell_grouping, selected_indices, selected_indices_name = "selected_indices") {
  n <- length(unique(c(0L, cell_grouping)))
  
  selected <- rep(FALSE, length(cell_grouping))
  selected[selected_indices] <- TRUE
  
  m <- length(unique(c(0L, 
                       -unique(cell_grouping[selected]), 
                       unique(cell_grouping[!selected]))))
  if(m != n) {
    stop(paste(selected_indices_name, "must be consistent with cell_grouping."))
  }
  NULL
}


# Adjusts selected_indices based on cell_grouping to ensure consistency.
# When add = TRUE, cells in the same group as a selected cell are added.
# When add = FALSE, selected cells in groups with non-selected cells are removed.
# Returns a list containing the updated selected_indices and the indices that were added or removed.
ensure_consistency_from_cell_grouping <- function(cell_grouping, selected_indices, add = TRUE) {
  if (!length(selected_indices)) {
    return(list(selected_indices, integer(0)))
  }
  selected <- rep(FALSE, length(cell_grouping))
  selected[selected_indices] <- TRUE
  not_selected <- !selected
  selected[cell_grouping == 0] <- FALSE
  not_selected[cell_grouping == 0] <- FALSE
  
  if (add) {
    found_indices <- which(not_selected)[cell_grouping[not_selected] %in% cell_grouping[selected]]
    selected_indices <- c(selected_indices, found_indices)
  } else {
    found_indices <- which(selected)[cell_grouping[selected] %in% cell_grouping[not_selected]]
    selected_indices <- selected_indices[!(selected_indices %in% found_indices)]
  }
  check_consistent_cell_grouping(cell_grouping, selected_indices)
  list(selected_indices, found_indices)
} 


order_candidates_by_cell_grouping <- function(candidates, cell_grouping) {
  candidates_order <- seq_along(candidates)
  cell_grouping_candidates <- cell_grouping[candidates]
  pos_cell_grouping_candidates <- cell_grouping_candidates > 0
  candidates_order_g0 <- candidates_order[pos_cell_grouping_candidates]
  cell_grouping_g0 <- cell_grouping_candidates[pos_cell_grouping_candidates]
  ma <- match(cell_grouping_g0, cell_grouping_g0)
  candidates_order_g0 <- candidates_order_g0[ma]
  candidates_order[pos_cell_grouping_candidates] <- candidates_order_g0
  candidates[order(candidates_order)]
}




# A version that not only checks if any cells are proportional,
# but returns whether each cell is proportional or not.
AnyProportionalGaussInt_OLD_ALL <- function(r, x, rB, xB, tolGauss,  kk_2_factorsB) {
  n <- length(r)
  if(!n){
    stop("Unforeseen problem")
  }
  out <- rep(FALSE, length(rB))
  for (i in seq_along(rB)) {
    ni <- length(xB[[i]])
    if (ni) {    # Empty "B-input" not regarded as proportional
      if (ni == n) {
        if (identical(r, rB[[i]])) {
          if (n==1L)
          {out[i] <- TRUE; next;}
          if (identical(x, xB[[i]])) 
          {out[i] <- TRUE; next;}
          if (identical(-x, xB[[i]])) 
          {out[i] <- TRUE; next;}
          
          cx1xBi1 <- c(x[1], xB[[i]][1])
          if(is.integer(cx1xBi1)){
            kk <- ReduceGreatestDivisor(cx1xBi1)
            suppressWarnings({
              kk_2_x <- kk[2] * x 
              kk_1_xB_i <- kk[1] * xB[[i]]
            })
            if(anyNA(kk_2_x) | anyNA(kk_1_xB_i)){
              kk <- as.numeric(kk)
              kk_2_x <- kk[2] * x 
              kk_1_xB_i <- kk[1] * xB[[i]]
              
            }   
            if (identical(kk_2_x, kk_1_xB_i)) 
            {out[i] <- TRUE; next;}
            if(is.numeric(kk)){
              if( all(abs( xB[[i]] - kk_2_x/kk[1]) < tolGauss))
              {out[i] <- TRUE; next;}
            }
          }
          else {
            if( all(abs(  xB[[i]] - (cx1xBi1[2]/cx1xBi1[1])* x) < tolGauss*abs(kk_2_factorsB[i]) )  )
            {out[i] <- TRUE; next;}
          }
        }
      }
    }
  }
  out
}




# Checks that the same cell group is always together
check_cell_grouping_within_gauss <- function(cell_grouping) {
  a <- cell_grouping[c(TRUE, diff(cell_grouping) != 0)]
  a <- a[a != 0]
  if (anyDuplicated(a)) {
    stop("Something wrong in cell_grouping algorithm")
  }
  NULL
}
