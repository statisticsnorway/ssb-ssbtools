#' Linearly independent rows and columns by Gaussian elimination
#' 
#' The function is written primarily for large sparse matrices with integers 
#' and even more correctly it is primarily written for dummy matrices (0s and 1s in input matrix).
#' 
#' @note The main algorithm is based on integers and exact calculations. When integers cannot be used (because of input or overflow), the algorithm switches.  
#' With `printInc = TRUE` as a parameter, `.....` change to `-----` when switching to numeric algorithm. 
#' With numeric algorithm, a kind of tolerance for linear dependency is included. 
#' This tolerance is designed having in mind that the input matrix is a dummy matrix.
#'
#' @param x A (sparse) matrix 
#' @param printInc Printing "..." to console when `TRUE`
#' @param tolGauss A tolerance parameter for sparse Gaussian elimination and linear dependency. This parameter is used only in cases where integer calculation cannot be used.
#' @param testMaxInt Parameter for testing: The Integer overflow situation will be forced when testMaxInt is exceeded   
#' @param allNumeric Parameter for testing: All calculations use numeric algorithm (as integer overflow) when TRUE
#'
#' @return List of logical vectors specifying independent rows and columns
#' @export 
#'
#' @examples
#' 
#' x <- ModelMatrix(SSBtoolsData("z2"), formula = ~fylke + kostragr * hovedint - 1)
#' 
#' GaussIndependent(x)
#' GaussRank(x)
#' GaussRank(t(x))
#' 
#' \dontrun{
#' # For comparison, qr-based rank may not work
#' rankMatrix(x, method = "qr")
#' 
#' # Dense qr works 
#' qr(as.matrix(x))$rank
#' }
GaussIndependent <- function(x, printInc = FALSE, tolGauss = (.Machine$double.eps)^(1/2), testMaxInt = 0, allNumeric = FALSE) {
  
  # testMaxInt is parameter for testing 
  # The Integer overflow situation will be forced when testMaxInt is exceeded   
  DoTestMaxInt = testMaxInt > 0
  
  # allNumeric is parameter for testing 
  # All calculations use numeric algorithm when TRUE
  if(allNumeric){
    Matrix2listInt <- SSBtools::Matrix2list 
  }
  
  A <- Matrix2listInt(x)
  m <- nrow(x)
  n <- length(A$r)
  
  nrA <- rep(NA_integer_, n)
  
  # To store cumulative factors from ReduceGreatestDivisor
  # Used to rescale when switching to numeric algorithm (caused by integer overflow).
  kk_2_factorsA <- rep(1, n)
  
  dot <- "."
  # dot will change to "-" when integer overflow occur (then numeric algorithm)  
  
  indcol <- rep(FALSE, n)
  indrow <- rep(FALSE, m)
  
  if (printInc)
    if(!is.numeric(printInc))
      printInc = 25L
  
  for (j in seq_len(n)) {
    if (printInc)
      if (j%%max(1, n%/%printInc) == 0) {
        cat(dot)
        flush.console()
      }
    
  if (length(A$r[[j]])) {
    
    ind <- A$r[[j]][1]   # integer(0)[1] is NA, but never after  if (length(A$r[[j]]))
    
    indcol[j] <- TRUE # !is.na(ind)
    indrow[ind] <- TRUE
    
    for (i in SeqInc(j + 1L, n)) nrA[i] <- match(ind, A$r[[i]])
    
    Arj <- A$r[[j]][-1L]
    Axj <- A$x[[j]][-1L]
    Axj1 <- A$x[[j]][1L]
    A$r[[j]] <- integer(0) # NA_integer_
    A$x[[j]] <- integer(0) # NA_integer_
    
    if (length(Arj) == 0L) {
      for (i in which(!is.na(nrA))) {
        if(length(A$r[[i]]) == 1L){
          A$r[[i]] <- integer(0)
          A$x[[i]] <- integer(0)
        } else {
          A$r[[i]] <- A$r[[i]][-nrA[i]]
          A$x[[i]] <- A$x[[i]][-nrA[i]]
          if (Scale2one(A$x[[i]])) {
            A$x[[i]][] <- 1L
            kk_2_factorsA[i] <- 1
          }
        }
      }
    } else {
      for (i in which(!is.na(nrA))) {
        if (length(A$x[[i]]) == 1L) {
          A$r[[i]] <- Arj
          A$x[[i]] <- Axj
          kk_2_factorsA[i] <- kk_2_factorsA[j] # Factors are inherited when all values are inherited
        } else {
          ai <- Arj
          bi <- A$r[[i]][-nrA[i]]
          ma <- match(ai, bi)
          isnama <- is.na(ma)
          ma_isnama <- ma[!isnama]
          di <- c(bi, ai[isnama])
          if (abs(A$x[[i]][nrA[i]]) == abs(Axj1)) {
            suppressWarnings({
              if (A$x[[i]][nrA[i]] == Axj1) {
                dx <- c(A$x[[i]][-nrA[i]], -Axj[isnama])
                dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
              } else {
                dx <- c(A$x[[i]][-nrA[i]], Axj[isnama])
                dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
              }
              if (DoTestMaxInt) {
                if (!anyNA(dx)) {
                  if (max(dx) > testMaxInt) {
                    dx[1] <- NA
                    warning("testMaxInt exceeded")
                  }
                }
              }
            })
            
            if (anyNA(dx)) 
            {
              dot <- "-"
              if (A$x[[i]][nrA[i]] == Axj1) {
                dx <- as.numeric(c(A$x[[i]][-nrA[i]], -Axj[isnama]))
                dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
              } else {
                dx <- as.numeric(c(A$x[[i]][-nrA[i]], Axj[isnama]))
                dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
              }
              dx <- dx/kk_2_factorsA[i]    # rescale needed since change to numeric
              kk_2_factorsA[i] <- 1
            } else {
              if(!is.integer(dx)){
                if(is.integer(A$x[[i]])){  # Change to numeric caused by Axj, rescale needed here also
                  dx <- dx/kk_2_factorsA[i]
                  kk_2_factorsA[i] <- 1
                }
              }
            }
          } else {
            kk <- ReduceGreatestDivisor(c(A$x[[i]][nrA[i]], Axj1))
            if(is.integer(kk)){
              kk_2_factorsA[i] <- kk[2] * kk_2_factorsA[i]
            }
            suppressWarnings({
              dx <- c(kk[2] * A$x[[i]][-nrA[i]], -kk[1] * Axj[isnama])
              dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
              if (DoTestMaxInt) {
                if (!anyNA(dx)) {
                  if (max(dx) > testMaxInt) {
                    dx[1] <- NA
                    warning("testMaxInt exceeded")
                  }
                }
              }
            })
            if (anyNA(dx)) 
            {
              dot <- "-"
              kk <- as.numeric(kk)
              dx <- c(kk[2] * A$x[[i]][-nrA[i]], -kk[1] * Axj[isnama])
              dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
              dx <- dx/kk_2_factorsA[i]   # rescale needed since change to numeric
              kk_2_factorsA[i] <- 1
            } else {
              if(!is.integer(dx)){
                if(is.integer(A$x[[i]])){      # Change to numeric caused by Axj, rescale needed here also
                  dx <- dx/kk_2_factorsA[i]
                  kk_2_factorsA[i] <- 1
                }
              }
            }
          }
          if(is.integer(dx)){
            rows <- (dx != 0L)
          } else {
            rows <- (abs(dx) >= tolGauss)
          }
          di <- di[rows]
          dx <- dx[rows]
          r <- order(di)
          A$r[[i]] <- di[r]
          A$x[[i]] <- dx[r]
          if (Scale2one(A$x[[i]])) {
            A$x[[i]][] <- 1L
            kk_2_factorsA[i] <- 1
          }
        }
      }
    }
    nrA[] <- NA_integer_
  }
  }
  
  return(list(rows = indrow, columns = indcol))
}



#' @rdname GaussIndependent
#' @details GaussRank returns the rank 
#' @export
GaussRank <- function(x, printInc = FALSE) {
  sum(GaussIndependent(x = x, printInc = printInc)[[1]])
}