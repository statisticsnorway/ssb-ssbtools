#' Linearly independent rows and columns by Gaussian elimination
#' 
#' The function is written primarily for large sparse matrices 
#'
#' @param x A (sparse) matrix 
#' @param printInc Printing "..." to console when `TRUE`
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
GaussIndependent <- function(x, printInc = FALSE) {
  
  A <- Matrix2listInt(x)
  m <- nrow(x)
  n <- length(A$r)
  
  nrA <- rep(NA_integer_, n)
  
  indcol <- rep(FALSE, n)
  indrow <- rep(FALSE, m)
  
  for (j in seq_len(n)) {
    if (printInc)
      if (j%%max(1, n%/%25) == 0) {
        cat(".")
        flush.console()
      }
    
    ind <- A$r[[j]][1]
    
    indcol[j] <- !is.na(ind)
    indrow[ind] <- TRUE
    
    for (i in SeqInc(j + 1L, n)) nrA[i] <- match(ind, A$r[[i]])
    
    Arj <- A$r[[j]][-1L]
    Axj <- A$x[[j]][-1L]
    Axj1 <- A$x[[j]][1L]
    A$r[[j]] <- NA_integer_
    A$x[[j]] <- NA_integer_
    
    if (length(Arj) == 0L) {
      for (i in which(!is.na(nrA))) {
        A$r[[i]] <- A$r[[i]][-nrA[i]]
        A$x[[i]] <- A$x[[i]][-nrA[i]]
      }
    } else {
      for (i in which(!is.na(nrA))) {
        if (length(A$x[[i]]) == 1L) {
          A$r[[i]] <- Arj
          A$x[[i]] <- Axj
        } else {
          ai <- Arj
          bi <- A$r[[i]][-nrA[i]]
          ma <- match(ai, bi)
          isnama <- is.na(ma)
          ma_isnama <- ma[!isnama]
          di <- c(bi, ai[isnama])
          if (abs(A$x[[i]][nrA[i]]) == abs(Axj1)) {
            if (A$x[[i]][nrA[i]] == Axj1) {
              dx <- c(A$x[[i]][-nrA[i]], -Axj[isnama])
              dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
            } else {
              dx <- c(A$x[[i]][-nrA[i]], Axj[isnama])
              dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
            }
          } else {
            kk <- ReduceGreatestDivisor(c(A$x[[i]][nrA[i]], Axj1))
            dx <- c(kk[2] * A$x[[i]][-nrA[i]], -kk[1] * Axj[isnama])
            dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
          }
          rows <- (dx != 0L)
          di <- di[rows]
          dx <- dx[rows]
          r <- order(di)
          A$r[[i]] <- di[r]
          A$x[[i]] <- dx[r]
        }
      }
    }
    nrA[] <- NA_integer_
  }
  if (printInc) {
    cat("\n")
    flush.console()
  }
  
  return(list(rows = indrow, columns = indcol))
}



#' @rdname GaussIndependent
#' @details GaussRank returns the rank 
#' @export
GaussRank <- function(x, printInc = FALSE) {
  sum(GaussIndependent(x = x, printInc = printInc)[[1]])
}