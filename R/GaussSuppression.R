
#' Secondary suppression by Gaussian elimination
#' 
#' Sequentially the secondary suppression candidates (columns in x) are used to reduce the x-matrix by Gaussian elimination. 
#' Candidates who completely eliminate one or more primary suppressed cells (columns in x) are omitted and made secondary suppressed. 
#' This ensures that the primary suppressed cells do not depend linearly on the non-suppressed cells.  
#' How to order the input candidates is an important choice. 
#' The singleton problem and the related problem of zeros are also handled.
#' 
#' It is possible to specify too many (all) indices as `candidates`. 
#' Indices specified as `primary` or `hidded` will be removed. 
#' Hidden indices (not candidates or primary) refer to cells that will not be published, but do not need protection. 
#' The singleton method `"subSum"` makes new imaginary primary suppressed cells, which are the sum of the singletons 
#' within each group. The `"subSpace"` method is conservative and ignores the singleton dimensions when looking for 
#' linear dependency. The default method, `"anySum"`, is between the other two. Instead of making imaginary cells of 
#' sums within groups, the aim is to handle all possible sums, also across groups. In addition, `"subSumSpace"`  and 
#' `"subSumAny"` are possible methods, primarily for testing These methods are similar to `"subSpace"` and `"anySum"`,
#'  and additional cells are created as in `"subSum"`. It is believed that the extra cells are redundant.
#'
#' @param x Matrix that relates cells to be published or suppressed to inner cells. yPublish = crossprod(x,yInner)
#' @param candidates Indices of candidates for secondary suppression   
#' @param primary    Indices of primary suppressed cells
#' @param forced     Indices forced to be not suppressed 
#' @param hidden     Indices to be removed from the above `candidates` input (see details)  
#' @param singleton Logical vector specifying inner cells for singleton handling. 
#'                 Normally, this means cells with 1s when 0s are non-suppressed and cells with 0s when 0s are suppressed.   
#' @param singletonMethod Method for handling the problem of singletons and zeros: `"anySum"` (default), `"subSum"`, `"subSpace"` or `"none"` (see details).
#' @param printInc Printing "..." to console when TRUE
#' @param ... Extra unused parameters
#'
#' @return Secondary suppression indices  
#' @importFrom Matrix colSums t
#' @export
#'
#' @examples
#' # Input data
#' df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
#'                  var1 = rep(1:3, each = 5), 
#'                  var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)
#' 
#' # Make output data frame and x 
#' fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
#' x <- fs$modelMatrix
#' datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))
#' 
#' # Add primary suppression 
#' datF$primary <- datF$values
#' datF$primary[datF$values < 5 & datF$values > 0] <- NA
#' datF$suppressedA <- datF$primary
#' datF$suppressedB <- datF$primary
#' datF$suppressedC <- datF$primary
#' 
#' # zero secondary suppressed
#' datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA
#' 
#' # zero not secondary suppressed by first in ordering
#' datF$suppressedB[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
#'                             primary = is.na(datF$primary))] <- NA
#' 
#' # with singleton
#' datF$suppressedC[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
#'                             primary = is.na(datF$primary), singleton = df$values == 1)] <- NA
#' 
#' datF
#' 
GaussSuppression <- function(x, candidates = 1:ncol(x), primary = NULL, forced = NULL, hidden = NULL, 
                             singleton = rep(FALSE, NROW(x)), singletonMethod = "anySum", printInc = TRUE, 
                             ...) {
  if (is.logical(primary)) 
    primary <- which(primary) 
  else 
    primary <- unique(primary)
    
  if (!length(primary)) 
    return(integer(0))
    
  if (is.logical(candidates)) 
    candidates <- which(candidates) 
  else 
    candidates <- unique(candidates)
      
  if (is.logical(hidden)) 
    hidden <- which(hidden) 
  else 
    hidden <- unique(hidden)
        
  if (is.logical(forced)) 
    forced <- which(forced) 
  else forced <- unique(forced)
          
  if (length(hidden)) 
    candidates <- candidates[!(candidates %in% hidden)]
  
  candidates <- candidates[!(candidates %in% primary)]
          
  nForced <- length(forced)
  
  if (nForced) {
    primary <- primary[!(primary %in% forced)]
    candidates <- candidates[!(candidates %in% forced)]
    candidates <- c(forced, candidates)
  }
  
  if (!is.logical(singleton)) {
    singletonA <- rep(FALSE, NROW(x))
    singletonA[singleton] <- TRUE
    singleton <- singletonA
  }
  
  if (is.function(singletonMethod)) {   # Alternative function possible
    return(singletonMethod(x, candidates, primary, printInc, singleton = singleton, nForced = nForced))
  }
  
  if (singletonMethod %in% c("subSum", "subSpace", "anySum", "subSumSpace", "subSumAny", "none")) {
    return(GaussSuppression1(x, candidates, primary, printInc, singleton = singleton, nForced = nForced, singletonMethod = singletonMethod))
  }
  
  stop("wrong singletonMethod")
}


GaussSuppression1 <- function(x, candidates, primary, printInc, singleton, nForced, singletonMethod) {
  
  if (singletonMethod == "none") {
    singleton <- FALSE
  }
  
  # make new primary suppressed subSum-cells
  if (grepl("subSum", singletonMethod)) {
    if (any(singleton)) {
      pZ <- x * singleton
      colZ <- colSums(pZ) > 1
      if (any(colZ)) {
        pZ <- pZ[, colZ, drop = FALSE]
        nodupl <- which(!duplicated(as.matrix(t(pZ))))
        pZ <- pZ[, nodupl, drop = FALSE]
        primary <- c(primary, NCOL(x) + seq_len(NCOL(pZ)))
        x <- cbind(x, pZ)
      }
    }
    if (singletonMethod == "subSum") 
      singleton <- FALSE
  }
  
  if (!any(singleton)) 
    singleton <- NULL
  
  if (printInc) {
    cat(paste0("GaussSuppression_", singletonMethod))
    flush.console()
  }
  
  if (!is.null(singleton)) {
    ordSingleton <- order(singleton)
    singleton <- singleton[ordSingleton]
    
    maTRUE <- match(TRUE, singleton)
    
    if (!is.na(maTRUE)) {
      ordyB <- ordSingleton[seq_len(maTRUE - 1)]
      maxInd <- maTRUE - 1
    } else {
      ordyB <- ordSingleton
      maxInd <- length(singleton)
    }
    
    # maxInd made for subSpace, maxInd2 needed by anySum
    maxInd2 <- maxInd
    
    # Removes cells that are handled by anySum/subSpace anyway
    if (!grepl("subSum", singletonMethod)) {
      primary <- primary[colSums(x[ordyB, primary, drop = FALSE]) != 0]
    }
    
    A <- Matrix2listInt(x[ordSingleton, candidates, drop = FALSE])
    if (grepl("Space", singletonMethod)) {
      B <- Matrix2listInt(x[ordyB, primary, drop = FALSE])
    } else {
      B <- Matrix2listInt(x[ordSingleton, primary, drop = FALSE])
      maxInd <- nrow(x)
    }
  } else {
    A <- Matrix2listInt(x[, candidates, drop = FALSE])
    B <- Matrix2listInt(x[, primary, drop = FALSE])
    maxInd <- nrow(x)
  }
  
  m <- nrow(x)
  n <- length(A$r)
  nB <- length(B$r)
  secondary <- rep(FALSE, n)
  
  if (printInc) {
    cat(": ")
    flush.console()
  }
  ii <- 1L
  nrA <- rep(NA_integer_, n)
  nrB <- rep(NA_integer_, nB)
  
  subUsed <- rep(FALSE, m)  # needed by anySum
  
  # The main Gaussian elimination loop 
  # Code made for speed, not readability
  for (j in seq_len(n)) {
    if (printInc) 
      if (j%%max(1, n%/%25) == 0) {
        cat(".")
        flush.console()
      }
    if (ii > m) 
      return(candidates[secondary])
    
    if (nForced > 0 & j == 1) {
      is0Br <- sapply(B$r, length) == 0
    }
    if (nForced > 0 & j == (nForced + 1)) {
      is0Br_ <- sapply(B$r, length) == 0
      if (any(is0Br != is0Br_)) {
        warning("Forced cells -> All primary cells are not safe")
      }
    }
    if (length(A$r[[j]])) {
      if (j > nForced) {
        if (is.null(singleton)) {
          isSecondary <- AnyProportionalGaussInt(A$r[[j]], A$x[[j]], B$r, B$x)
        } else {
          subSubSec <- A$r[[j]][1] > maxInd2
          if (grepl("Space", singletonMethod)) {
            okArj <- A$r[[j]] <= maxInd
            isSecondary <- subSubSec | (AnyProportionalGaussInt(A$r[[j]][okArj], A$x[[j]][okArj], B$r, B$x))
          } else {
            if (subSubSec) {
              if (length(unique(A$x[[j]])) > 1) {  # Not proportional to original sum, 
                if (!any(subUsed[A$r[[j]]])) {     # but can’r be sure after gaussian elimination of another “Not proportional to sum”.
                  subSubSec <- FALSE               # To be sure, non-overlapping restriction introduced (subUsed) 
                  subUsed[A$r[[j]]] <- TRUE
                } else {
                  # if(printInc) # "Can't-be-sure-suppression" if "AnyProportionalGaussInt(.." is FALSE
                  #   cat('@')   # More advanced method may improve
                }
              }
            }
            isSecondary <- subSubSec | (AnyProportionalGaussInt(A$r[[j]], A$x[[j]], B$r, B$x))
          }
        }
      } else {
        isSecondary <- FALSE
      }
      if (!isSecondary) {
        ind <- A$r[[j]][1]
        for (i in SeqInc(j + 1L, n)) 
          nrA[i] <- match(ind, A$r[[i]])
        for (i in seq_len(nB)) 
          nrB[i] <- match(ind, B$r[[i]])
        
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
        if (!is.null(singleton)) {
          okInd <- (Arj <= maxInd)
          Arj <- Arj[okInd]
          Axj <- Axj[okInd]
        }
        if (length(Arj) == 0L) {
          for (i in which(!is.na(nrB))) {
            B$r[[i]] <- B$r[[i]][-nrB[i]]
            B$x[[i]] <- B$x[[i]][-nrB[i]]
          }
        } else {
          for (i in which(!is.na(nrB))) {
            if (length(B$x[[i]]) == 1L) {
              B$r[[i]] <- Arj
              B$x[[i]] <- Axj
            } else {
              ai <- Arj
              bi <- B$r[[i]][-nrB[i]]
              ma <- match(ai, bi)
              isnama <- is.na(ma)
              ma_isnama <- ma[!isnama]
              di <- c(bi, ai[isnama])
              if (abs(B$x[[i]][nrB[i]]) == abs(Axj1)) {
                if (B$x[[i]][nrB[i]] == Axj1) {
                  dx <- c(B$x[[i]][-nrB[i]], -Axj[isnama])
                  dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
                } else {
                  dx <- c(B$x[[i]][-nrB[i]], Axj[isnama])
                  dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
                }
              } else {
                kk <- ReduceGreatestDivisor(c(B$x[[i]][nrB[i]], Axj1))
                dx <- c(kk[2] * B$x[[i]][-nrB[i]], -kk[1] * Axj[isnama])
                dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
              }
              rows <- (dx != 0L)
              di <- di[rows]
              dx <- dx[rows]
              r <- order(di)
              B$r[[i]] <- di[r]
              B$x[[i]] <- dx[r]
            }
          }
        }
        nrA[] <- NA_integer_
        nrB[] <- NA_integer_
        ii <- ii + 1L
      } else {
        secondary[j] <- TRUE
      }
    }
  }
  if (printInc) {
    cat("\n")
    flush.console()
  }
  candidates[secondary]
}







AnyProportionalGaussInt <- function(r, x, rB, xB) {
  n <- length(r)
  if(!n){
    return(TRUE) # Empty "A-input" regarded as proportional
  }
  for (i in seq_along(rB)) {
    ni <- length(xB[[i]])
    if (ni) {    # Empty "B-input" not regarded as proportional
      if (ni == n) {
        if (identical(r, rB[[i]])) {
          if (identical(x, xB[[i]])) 
            return(TRUE)
          if (identical(-x, xB[[i]])) 
            return(TRUE)
          kk <- ReduceGreatestDivisor(c(x[1], xB[[i]][1]))
          if (identical(kk[2] * x, kk[1] * xB[[i]])) 
            return(TRUE)
        }
      }
    }
  }
  FALSE
}



# Reduce by Greatest common divisor (integer input)
ReduceGreatestDivisor <- function(ab) {
  a <- ab[1]
  b <- ab[2]
  while (TRUE) {
    r <- a%%b
    a <- b
    if (!r) 
      return(ab%/%b)
    b <- r
  }
  stop("Something wrong")
}
