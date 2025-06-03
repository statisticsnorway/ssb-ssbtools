#
# This is an internal function for GaussSuppression.
# In a separate file for easiness and to avoid misunderstandings.
# When this code is used, both A and B internally in the function are parts of the original B
#
# return_all parameter added for reuse for another purpose
#
AnyEliminatedBySingleton <- function(A, B, kk_2_factorsA, kk_2_factorsB, singleton, DoTestMaxInt, tolGauss,
                                     N_GAUSS_DUPLICATES, dash, maxInd, testMaxInt,
                                     return_all = FALSE){
  n <- length(A$r)
  if (!n) {
    if (return_all) {
      stop("Unforeseen problem")
    }
    return(FALSE)
  }
  nB <- length(B$r)
  if (return_all) {
    out <- rep(FALSE, nB)
  }
  nrA <- rep(NA_integer_, n)
  nrB <- rep(NA_integer_, nB)
  
  for (j in seq_len(n-1)) {             # Code in this for loop is copied from the main elimination loop in GaussSuppression  
if (length(A$r[[j]])) {                                  # extra for special case 
    ind <- A$r[[j]][1]                  # No function call in the main elimination loop due to speed/memory
    nrA[] <- NA_integer_
    nrB[] <- NA_integer_
    for (i in SeqInc(j + 1L, n)) 
      nrA[i] <- match(ind, A$r[[i]])
    for (i in seq_len(nB)) 
      nrB[i] <- match(ind, B$r[[i]])
    
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
              dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
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
              dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
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
    if (!is.null(singleton)) {
      okInd <- (Arj <= maxInd)
      Arj <- Arj[okInd]
      Axj <- Axj[okInd]
    }
    if (length(Arj) == 0L) {
      for (i in which(!is.na(nrB))) {
        B$r[[i]] <- B$r[[i]][-nrB[i]]
        B$x[[i]] <- B$x[[i]][-nrB[i]]
        if (Scale2one(B$x[[i]])) {
          B$x[[i]][] <- 1L
          kk_2_factorsB[i] <- 1
        }
      }
    } else {
      for (i in which(!is.na(nrB))) {
        if (length(B$x[[i]]) == 1L) {
          B$r[[i]] <- Arj
          B$x[[i]] <- Axj
          kk_2_factorsB[i] <- kk_2_factorsA[j] # Factors are inherited when all values are inherited
        } else {
          ai <- Arj
          bi <- B$r[[i]][-nrB[i]]
          ma <- match(ai, bi)
          isnama <- is.na(ma)
          ma_isnama <- ma[!isnama]
          di <- c(bi, ai[isnama])
          if (abs(B$x[[i]][nrB[i]]) == abs(Axj1)) {
            suppressWarnings({
              if (B$x[[i]][nrB[i]] == Axj1) {
                dx <- c(B$x[[i]][-nrB[i]], -Axj[isnama])
                dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
              } else {
                dx <- c(B$x[[i]][-nrB[i]], Axj[isnama])
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
              dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
              if (B$x[[i]][nrB[i]] == Axj1) {
                dx <- as.numeric(c(B$x[[i]][-nrB[i]], -Axj[isnama]))
                dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
              } else {
                dx <- as.numeric(c(B$x[[i]][-nrB[i]], Axj[isnama]))
                dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
              }
              dx <- dx/kk_2_factorsB[i]
              kk_2_factorsB[i] <- 1
            }
            else {
              if(!is.integer(dx)){
                if(is.integer(B$x[[i]])){
                  dx <- dx/kk_2_factorsB[i]
                  kk_2_factorsB[i] <- 1
                }
              }
            }
          } else {
            kk <- ReduceGreatestDivisor(c(B$x[[i]][nrB[i]], Axj1))
            if(is.integer(kk)){
              kk_2_factorsB[i] <- kk[2] * kk_2_factorsB[i]
            }
            suppressWarnings({
              dx <- c(kk[2] * B$x[[i]][-nrB[i]], -kk[1] * Axj[isnama])
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
              dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
              kk <- as.numeric(kk)
              dx <- c(kk[2] * B$x[[i]][-nrB[i]], -kk[1] * Axj[isnama])
              dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
              dx <- dx/kk_2_factorsB[i]
              kk_2_factorsB[i] <- 1
            } else {
              if(!is.integer(dx)){
                if(is.integer(B$x[[i]])){
                  dx <- dx/kk_2_factorsB[i]
                  kk_2_factorsB[i] <- 1
                }
              }
            }
          }
          if(is.integer(dx)){
            rows <- (dx != 0L)
          } else {
            rows <- (abs(dx) >= tolGauss)
          }
          if(!length(rows)){
            stop("Suppression method failed")
          }
          di <- di[rows]
          dx <- dx[rows]
          r <- order(di)
          B$r[[i]] <- di[r]
          B$x[[i]] <- dx[r]
          if (Scale2one(B$x[[i]])) {
            B$x[[i]][] <- 1L
            kk_2_factorsB[i] <- 1
          }
        }
      }
    }
    for (i in which(!is.na(nrB))) {
      if(!length(B$r[[i]])){
        if (!return_all) {
          return(TRUE)
        }
        out[i] <- TRUE
      }
    }
  }
}
  if (length(A$r[[n]])) {
    if (!return_all) {
      return(AnyProportionalGaussInt_OLD(A$r[[n]], A$x[[n]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB))
    }
    out_n <- AnyProportionalGaussInt_OLD_ALL(A$r[[n]], A$x[[n]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB)
    out <- out | out_n  
  }
  if (!return_all) {
    return(FALSE)
  }
  out
}
