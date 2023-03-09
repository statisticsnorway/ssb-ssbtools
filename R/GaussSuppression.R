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
#' All singleton methods, except `"sub2Sum"`, have been implemented with frequency tables in mind.
#' The singleton method `"subSum"` makes new imaginary primary suppressed cells, which are the sum of the singletons 
#' within each group. The `"subSpace"` method is conservative and ignores the singleton dimensions when looking for 
#' linear dependency. The default method, `"anySum"`, is between the other two. Instead of making imaginary cells of 
#' sums within groups, the aim is to handle all possible sums, also across groups. In addition, `"subSumSpace"`  and 
#' `"subSumAny"` are possible methods, primarily for testing These methods are similar to `"subSpace"` and `"anySum"`,
#'  and additional cells are created as in `"subSum"`. It is believed that the extra cells are redundant.
#'  All the above methods assume that any published singletons are primary suppressed. 
#'  When this is not the case, `"anySumNOTprimary"` must be used.
#'  The singleton method `"sub2Sum"` makes new imaginary primary suppressed cells, which are the sum of two inner cells. 
#'  This is done when a group contains exactly two primary suppressed inner cells provided that at least one of them is singleton.
#'  The `"sub2SumUnique"` method is an extension of `"sub2Sum"` that takes into account that the same contributor can appear in several input rows.
#'  NOTE: The name `"sub2SumUnique"` will probably be changed.
#'  
#'
#' @param x Matrix that relates cells to be published or suppressed to inner cells. yPublish = crossprod(x,yInner)
#' @param candidates Indices of candidates for secondary suppression   
#' @param primary    Indices of primary suppressed cells
#' @param forced     Indices forced to be not suppressed. `forced` has precedence over `primary`. See `whenPrimaryForced` below.
#' @param hidden     Indices to be removed from the above `candidates` input (see details)  
#' @param singleton Logical vector specifying inner cells for singleton handling. 
#'                 Normally, this means cells with 1s when 0s are non-suppressed and cells with 0s when 0s are suppressed.   
#' @param singletonMethod Method for handling the problem of singletons and zeros: `"anySum"` (default), `"anySumNOTprimary"`, `"subSum"`, `"subSpace"`, `"sub2Sum"`, `"sub2SumUnique"` or `"none"` (see details).
#' @param printInc Printing "..." to console when TRUE
#' @param tolGauss A tolerance parameter for sparse Gaussian elimination and linear dependency. This parameter is used only in cases where integer calculation cannot be used.
#' @param whenEmptySuppressed Function to be called when empty input to primary suppressed cells is problematic. Supply NULL to do nothing.
#' @param whenEmptyUnsuppressed Function to be called when empty input to candidate cells may be problematic. Supply NULL to do nothing.
#' @param whenPrimaryForced Function to be called if any forced cells are primary suppressed (suppression will be ignored). Supply NULL to do nothing.
#' @param removeDuplicated Whether to remove duplicated columns in `x` before running the main algorithm. 
#' @param iFunction A function to be called during the iterations. See the default function, \code{\link{GaussIterationFunction}}, for description of parameters. 
#' @param iWait The minimum number of seconds between each call to `iFunction`.
#'              Whenever `iWait<Inf`, `iFunction` will also be called after last iteration. 
#' @param xExtraPrimary Extra x-matrix that defines extra primary suppressed cells in addition to those defined by other inputs.               
#' @param ... Extra unused parameters
#'
#' @return Secondary suppression indices  
#' @importFrom Matrix colSums t Matrix
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
                             singleton = rep(FALSE, nrow(x)), singletonMethod = "anySum", printInc = TRUE, tolGauss = (.Machine$double.eps)^(1/2),
                             whenEmptySuppressed = warning, 
                             whenEmptyUnsuppressed = message,
                             whenPrimaryForced = warning,
                             removeDuplicated = TRUE, 
                             iFunction = GaussIterationFunction, iWait = Inf,
                             xExtraPrimary = NULL,
                             ...) {
  
  if (identical(removeDuplicated, "test")){
    sysCall <- sys.call()
    parentFrame <- parent.frame()
    sysCall["removeDuplicated"] <- TRUE
    outTRUE <- eval(sysCall, envir = parentFrame)
    sysCall["removeDuplicated"] <- FALSE
    outFALSE <- eval(sysCall, envir = parentFrame)
    if(isTRUE(all.equal(outTRUE, outFALSE))){
      return(outTRUE)
    }
    print(outTRUE)
    print(outFALSE)
    stop("removeDuplicated test: Not all equal")
  }
  
  if (is.logical(primary)) 
    primary <- which(primary) 
  else 
    primary <- unique(primary)
  
  
  if (!is.null(xExtraPrimary)) {
    # primary has already been converted to indexes
    primary <- c(primary, ncol(x) + seq_len(ncol(xExtraPrimary)))
    # forced and hidden can be untreated since conversion to indexes below
    x <- cbind(x, xExtraPrimary)
  }
  
    
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
  
  if (!is.null(whenPrimaryForced)) {
    if (any(primary %in% forced)) {
      whenPrimaryForced("Primary suppression of forced cells ignored")
    }
  }
  
  if (removeDuplicated) {
    # idxDD <- DummyDuplicated(x, idx = TRUE, rnd = TRUE)
    idxDD <- DummyDuplicatedSpec(x,  candidates, primary, forced)
    idxDDunique <- unique(idxDD)
    
    if (length(idxDDunique) == length(idxDD)) {
      removeDuplicated <- FALSE
    } else {
      if (length(forced)) { # Needed for warning
        primary <- primary[!(primary %in% forced)]
      }
      
      idNew <- rep(0L, ncol(x))
      idNew[idxDDunique] <- seq_len(length(idxDDunique))
      
      candidatesOld <- candidates
      primaryOld <- primary
      
      primary <- idNew[unique(idxDD[primary])]
      candidates <- idNew[unique(idxDD[candidates])]
      forced <- idNew[unique(idxDD[forced])]
      x <- x[, idxDDunique, drop = FALSE]
      
      if (any(primary %in% forced)) {
        warning("Forced cells -> All primary cells are not safe (duplicated)")
      }
    }
  }
  
  if (!removeDuplicated) {
    idxDD <- NULL
    idxDDunique <- NULL
    candidatesOld <- NULL
    primaryOld <- NULL
  }
  
  
  candidates <- candidates[!(candidates %in% primary)]
          
  nForced <- length(forced)
  
  if (nForced) {
    primary <- primary[!(primary %in% forced)]
    candidates <- candidates[!(candidates %in% forced)]
    candidates <- c(forced, candidates)
  }
  
  if(is.null(singleton)){
    singleton <- rep(FALSE, nrow(x))
  }
  if (is.logical(singleton)) {
    if(length(singleton) == 1L){
      singleton <- rep(singleton, nrow(x))
    }
  }
  if(is.integer(singleton)){
    if (min(singleton) < 0) {
      stop("integer singletons must be nonzero")
    }
    singleton_integer <- singleton
    singleton <- as.logical(singleton)
  } else {
    singleton_integer <- NULL
  }
  if (!is.logical(singleton)) {
    stop("singleton must be logical or integer")
  }
  if(length(singleton) != nrow(x)){
    stop("length(singleton) must be nrow(x)")
  }
  
  if (is.function(singletonMethod)) {   # Alternative function possible
    return(singletonMethod(x, candidates, primary, printInc, singleton = singleton, nForced = nForced))
  }
  
  if (singletonMethod %in% c("subSum", "subSpace", "anySum", "anySumNOTprimary", "subSumSpace", "subSumAny", "sub2Sum", "sub2SumUnique", "none")) {
    
    if(!is.null(whenEmptySuppressed)){
      if(min(colSums(abs(x[, primary, drop = FALSE]))) == 0){
        whenEmptySuppressed("Suppressed cells with empty input will not be protected. Extend input data with zeros?")
      }
    }
    
    secondary <- GaussSuppression1(x, candidates, primary, printInc, singleton = singleton, nForced = nForced, 
                                           singletonMethod = singletonMethod, singleton_integer = singleton_integer, tolGauss=tolGauss, 
                                           iFunction = iFunction, iWait = iWait,
                                   main_primary = primary, idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld,
                                           ...)
    
    if(length(secondary) & !is.null(whenEmptyUnsuppressed)){
      lateUnsuppressed <- candidates[SeqInc(1L + min(match(secondary, candidates)), length(candidates))]
      lateUnsuppressed <- lateUnsuppressed[!(lateUnsuppressed %in% secondary)]
      if(length(lateUnsuppressed)){
        if(min(colSums(abs(x[, lateUnsuppressed, drop = FALSE]))) == 0){
          whenEmptyUnsuppressed("Cells with empty input will never be secondary suppressed. Extend input data with zeros?")
        }
      }
    }

    secondary <- SecondaryFinal(secondary = secondary, primary = primary, idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld)
    
    return(secondary)
  }
  
  stop("wrong singletonMethod")
}

# Function to handle removeDuplicated
SecondaryFinal <- function(secondary, primary, idxDD, idxDDunique, candidatesOld, primaryOld) {
  if (is.null(idxDD)) {
    return(secondary)
  }
  ma <- match(idxDD[candidatesOld], c(idxDDunique[secondary], idxDDunique[primary]))
  secondary <- candidatesOld[!is.na(ma)]
  secondary[!(secondary %in% primaryOld)]
}



GaussSuppression1 <- function(x, candidates, primary, printInc, singleton, nForced, singletonMethod, singleton_integer, tolGauss, testMaxInt = 0, allNumeric = FALSE,
                              iFunction, iWait, 
                              main_primary, idxDD, idxDDunique, candidatesOld, primaryOld, # main_primary also since primary may be changed 
                              ...) {
  
  if (!is.numeric(iWait)) {
    iWait <- Inf
  } else {
    if (is.na(iWait)) iWait <- Inf
  }
  if (!is.function(iFunction)) iWait <- Inf
  use_iFunction <- iWait < Inf
  
  if (use_iFunction) {
    sys_time <- Sys.time()
  }
  
  # testMaxInt is parameter for testing 
  # The Integer overflow situation will be forced when testMaxInt is exceeded   
  DoTestMaxInt = testMaxInt > 0
  
  # allNumeric is parameter for testing 
  # All calculations use numeric algorithm when TRUE
  if(allNumeric){
    Matrix2listInt <- SSBtools::Matrix2list 
  }
  
  if (printInc) {
    cat(paste0("GaussSuppression_", singletonMethod))
    flush.console()
  }
  
  if (singletonMethod == "none") {
    singleton <- FALSE
  }
  if (singletonMethod == "anySumNOTprimary") {
    singletonMethod <- "anySum"
    singletonNOTprimary <- TRUE
  } else {
    if (any(singleton)) {
      colSums_x <- colSums(x)
      singletonZ <- (colSums(x[singleton, , drop = FALSE]) == 1 & colSums_x == 1)
      singletonNOTprimary <- (sum(singletonZ) > sum(singletonZ[primary]))
    } else {
      singletonNOTprimary <- FALSE
    }
    if (singletonNOTprimary) {
      if (singletonMethod != "anySum")
        stop('singletonMethod must be "anySumNOTprimary" when singletons not primary suppressed')
      warning('singletonMethod is changed to "anySumNOTprimary"')
    }
  }
  
  
  # make new primary suppressed subSum-cells
  if (grepl("subSum", singletonMethod) | grepl("sub2Sum", singletonMethod)) {
    if (any(singleton)) {
      if (grepl("sub2Sum", singletonMethod)) {
  
        singletonHierarchySearch <- get0("singletonHierarchySearch", ifnotfound = FALSE)   # provisional
        singletonExtraPrimary <- get0("singletonExtraPrimary", ifnotfound = FALSE)         # provisional
        if (singletonExtraPrimary) {
          singletonNotInPublish <- singleton
          singletonNotInPublish[rowSums(x[, primary[colSums(x[, primary, drop = FALSE]) == 1], drop = FALSE]) > 0] <- FALSE  # singletonNotInPublish[innerprimary] <- FALSE
          if (any(singletonNotInPublish)) {
            message("singletonExtraPrimary is used")   # provisional
            pZ <- Matrix(0, length(singletonNotInPublish), sum(singletonNotInPublish))
            pZ[cbind(which(singletonNotInPublish), seq_len(sum(singletonNotInPublish)))] <- 1
            primary <- c(primary, NCOL(x) + seq_len(NCOL(pZ)))  # same code as below
            x <- cbind(x, pZ)                                   # ---- // -----
          }
        }
        
        pZs <- x * singleton
        pZ <- x * (rowSums(x[, primary[colSums(x[, primary, drop = FALSE]) == 1], drop = FALSE]) > 0)  #  x * innerprimary
        pZ[ , primary] <- 0  # Not relevant when already suppressed 
        if (singletonMethod == "sub2SumUnique") {
          message("The name sub2SumUnique will probably be changed")
          if (is.null(singleton_integer)) {
            stop("singleton as integer needed when sub2SumUnique")
          }
          relevant_unique_index <- -seq_len(nrow(x))  # negative is guaranteed different from singleton_integer
          relevant_unique_index[singleton] <- singleton_integer[singleton]
          colSums_pZ_g_1 <- colSums(pZ) > 1
          if (any(colSums_pZ_g_1)) { # with this, DummyApply problem when onlys zeros in pZ also avoided
            cols_g_2 <- DummyApply(pZ, relevant_unique_index, function(x) length(unique(x))) > 2
            colSums_pZ_requirement <- !cols_g_2 & colSums_pZ_g_1
          } else {
            colSums_pZ_requirement <- colSums_pZ_g_1
            cols_g_2 <- FALSE
          }
          # colSums(pZ) > 1 since primary already exists when colSums(pZ) == 1
          # =2 before "&" here similar to =2 in sub2Sum: 
          #      * two primary suppressed inner cells provided that at least one of them is singleton (colSums(pZs) > 0)
          #      * Difference is that same singleton counted as 1
          # =1 before "&" here is extra 
          #      * All primary suppressed inner cells in group are same singleton and counted as 1 
          #      * The sum of this group needs protection
          # =0 before "&" here 
          #      * will never happen when colSums(pZ) > 1)
          #
        } else {  # singletonMethod == "sub2Sum"
          colSums_pZ_requirement <- colSums(pZ) == 2
          if (singletonHierarchySearch) {
            cols_g_2 <- colSums(pZ) > 2
          }
        }
        if (singletonHierarchySearch) {
          if (any(cols_g_2)) {
            cols_g_2 <- which(cols_g_2)
            diffMatrix <- FindDiffMatrix(x[, primary[colSums(x[, primary, drop = FALSE]) > 1], drop = FALSE], # primary with more than 1, =1 already treated  
                                         pZ[, cols_g_2, drop = FALSE])  # (x * innerprimary) with more than 2
            colnames(diffMatrix) <- cols_g_2[as.integer(colnames(diffMatrix))]  # now colnames correspond to pZ columns
            # Is there any difference column that corresponds to a unique contributor? The code below tries to answer.
            if (ncol(diffMatrix)) {
              diffMatrix <- diffMatrix[, colSums(diffMatrix[!singleton, , drop = FALSE]) == 0, drop = FALSE]
              diffMatrix <- diffMatrix[singleton, , drop = FALSE]
              if (ncol(diffMatrix)) {
                colSums_diffMatrix_is1 <- colSums(diffMatrix) == 1
                if (any(colSums_diffMatrix_is1)) {
                  message("singletonHierarchySearch is used in the standard way")  # provisional
                  if (printInc) {
                    cat("\n   singletonHierarchySearch is used in the standard way:", paste(colnames(pZ)[as.integer(colnames(diffMatrix)[which(colSums_diffMatrix_is1)])], collapse = ", "), "\n")
                  }
                  colSums_pZ_requirement[as.integer(colnames(diffMatrix)[colSums_diffMatrix_is1])] <- TRUE
                  diffMatrix <- diffMatrix[, !colSums_diffMatrix_is1, drop = FALSE]
                }
                if (singletonMethod == "sub2SumUnique" & ncol(diffMatrix)) {
                  cols_eq_1 <- DummyApply(diffMatrix, relevant_unique_index[singleton], function(x) length(unique(x))) == 1
                  if (any(cols_eq_1)) {
                    message("singletonExtraPrimary is used in combination with sub2SumUnique")  # provisional
                    if (printInc) {
                      cat("\n   singletonExtraPrimary is used in combination with sub2SumUnique:", paste(colnames(pZ)[as.integer(colnames(diffMatrix)[which(cols_eq_1)])], collapse = ", "), "\n")
                    }
                    colSums_pZ_requirement[as.integer(colnames(diffMatrix)[cols_eq_1])] <- TRUE
                  }
                }
              }
            }
          }
        }
        colZ <- ((colSums(pZs) > 0) & colSums_pZ_requirement)
      } else {
        pZ <- x * singleton
        colZ <- colSums(pZ) > 1
      }
      if (any(colZ)) {
        pZ <- pZ[, colZ, drop = FALSE]
        nodupl <- which(!duplicated(as.matrix(t(pZ))))     # Possible improvement by DummyDuplicated
        pZ <- pZ[, nodupl, drop = FALSE]
        primary <- c(primary, NCOL(x) + seq_len(NCOL(pZ)))
        x <- cbind(x, pZ)
      }
    }
    if (singletonMethod == "subSum" | grepl("sub2Sum", singletonMethod)) 
      singleton <- FALSE
  }
  

  
  if (!any(singleton)) 
    singleton <- NULL
  
  
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
    if (!singletonNOTprimary) {
      if (!grepl("subSum", singletonMethod)) {
        primary <- primary[colSums(x[ordyB, primary, drop = FALSE]) != 0]
      }
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
  
  
  # To store cumulative factors from ReduceGreatestDivisor
  # Used to rescale when switching to numeric algorithm (caused by integer overflow).
  kk_2_factorsA <- rep(1, n)
  kk_2_factorsB <- rep(1, nB)
  
  
  subUsed <- rep(FALSE, m)  # needed by anySum
  
  dot <- "."
  # dot will change to "-" when integer overflow occur (then numeric algorithm)  
  
  
  # The main Gaussian elimination loop 
  # Code made for speed, not readability
  for (j in seq_len(n)) {
    if (printInc) 
      if (j%%max(1, n%/%25) == 0) {
        cat(dot)
        flush.console()
      }
    if (ii > m){ 
      if (printInc) {
        cat("\n")
        flush.console()
      }
      return(candidates[secondary])
    }
    
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
      reduced <- FALSE
      if (j > nForced) {
        if (is.null(singleton)) {
          isSecondary <- AnyProportionalGaussInt(A$r[[j]], A$x[[j]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB)
        } else {
          subSubSec <- A$r[[j]][1] > maxInd2
          if (grepl("Space", singletonMethod)) {
            okArj <- A$r[[j]] <= maxInd
            isSecondary <- subSubSec | (AnyProportionalGaussInt(A$r[[j]][okArj], A$x[[j]][okArj], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB))
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
            if (subSubSec & singletonNOTprimary) {
              if (!Any0GaussInt(A$r[[j]], B$r)) {
                subSubSec <- FALSE
                for (i in SeqInc(j + 1L, n)) {
                  j_in_i <- A$r[[i]] %in% A$r[[j]]
                  if (all(j_in_i)) {
                    A$r[[i]] <- integer(0)
                    A$x[[i]] <- integer(0)
                  } else {
                  if (any(j_in_i)) {
                    A$r[[i]] <- A$r[[i]][!j_in_i]
                    A$x[[i]] <- A$x[[i]][!j_in_i]
                  }
                  }
                }
                for (i in seq_len(nB)) {
                  j_in_i <- B$r[[i]] %in% A$r[[j]]
                  if (any(j_in_i)) {
                    B$r[[i]] <- B$r[[i]][!j_in_i]
                    B$x[[i]] <- B$x[[i]][!j_in_i]
                  }
                }
                A$r[[j]] <- integer(0)
                A$x[[j]] <- integer(0)  
                isSecondary <- FALSE
                reduced <- TRUE
              } else {
                isSecondary <- TRUE
              }
              
            } else {
              isSecondary <- subSubSec | (AnyProportionalGaussInt(A$r[[j]], A$x[[j]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB))
            }
          }
        }
      } else {
        isSecondary <- FALSE
      }
      if (!isSecondary) {
       if (!reduced) { 
        ind <- A$r[[j]][1]
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
                  dot <- "-"
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
                  dot <- "-"
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
       }  
        nrA[] <- NA_integer_
        nrB[] <- NA_integer_
        ii <- ii + 1L
      } else {
        A$r[[j]] <- integer(0)
        A$x[[j]] <- integer(0)
        secondary[j] <- TRUE
      }
    }
    if (use_iFunction) {
      sys_time2 <- Sys.time()
      if (ii-1L == m) {
        j_ <- n
      } else {
        j_ <- j
      }
      if (j_ == n) {
        iWait <- 0
      }
      if (as.numeric(difftime(sys_time2, sys_time), units = "secs") >= iWait){
        sys_time <- sys_time2
        false_ <- !secondary
        
        allEmptyDecided <- TRUE 
        if(allEmptyDecided){
          false_[SeqInc(j_+1,n)] <- (lengths(A$r) == 0)[SeqInc(j_+1,n)]
          na_ <- !(secondary | false_)  
        } else { # old code 
          false_[SeqInc(j_+1,n)] <- FALSE
          na_    <- !secondary
          na_[SeqInc(1,j_)] <- FALSE
        }
        
        iFunction(i = j_, I = n, j = ii-1L, J = m,
                  true =  SecondaryFinal(secondary = candidates[secondary], primary = main_primary, idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld),
                  false = SecondaryFinal(secondary = candidates[false_],    primary = integer(0),   idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = integer(0)),
                  na =    SecondaryFinal(secondary = candidates[na_],       primary = integer(0),   idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = integer(0)),
                  ...)
      }
    }
  }
  
  # cat("\n")
  # print(table(kk_2_factorsA))
  # print(table(kk_2_factorsB))
  # print(table(sapply(A$x,class)))
  # print(table(sapply(B$x,class)))
  
  if (printInc) {
    cat("\n")
    flush.console()
  }
  candidates[secondary]
}



# Simplified version of AnyProportionalGaussInt 
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




AnyProportionalGaussInt <- function(r, x, rB, xB, tolGauss,  kk_2_factorsB) {
  n <- length(r)
  if(!n){
    return(TRUE) # Empty "A-input" regarded as proportional
  }
  for (i in seq_along(rB)) {
    ni <- length(xB[[i]])
    if (ni) {    # Empty "B-input" not regarded as proportional
      if (ni == n) {
        if (identical(r, rB[[i]])) {
          if (n==1L)
            return(TRUE)
          if (identical(x, xB[[i]])) 
            return(TRUE)
          if (identical(-x, xB[[i]])) 
            return(TRUE)
          
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
              return(TRUE)
            if(is.numeric(kk)){
              if( all(abs( xB[[i]] - kk_2_x/kk[1]) < tolGauss))
                return(TRUE)
            }
          }
          else {
            #if (FALSE) {
            #
            #  Possible code here to look at distribution of numeric computing errors  
            #
            #  aabb <- abs((xB[[i]] - (cx1xBi1[2]/cx1xBi1[1]) * x)/kk_2_factorsB[i])
            #  aabb <- aabb[aabb > 0 & aabb < 1e-04]
            #}
            if( all(abs(  xB[[i]] - (cx1xBi1[2]/cx1xBi1[1])* x) < tolGauss*abs(kk_2_factorsB[i]) )  )
              return(TRUE)
          }
        }
      }
    }
  }
  FALSE
}



# Reduce by Greatest common divisor (when integer input)
ReduceGreatestDivisor <- function(ab) {
  if(!is.integer(ab)){
    return(c(ab[1]/ab[2], 1))
  }
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


Scale2one <- function(x) {
  if (!length(x))
    return(FALSE)
  if (x[1] == 1L)
    return(FALSE)
  if (length(x) == 1L)
    return(TRUE)
  identical(min(x), max(x))
}




# Special version of DummyDuplicated(x, idx = TRUE, rnd = TRUE)
# Some 0’s changed to other values 
DummyDuplicatedSpec <- function(x, candidates, primary, forced) {
  
  xtu <- XprodRnd(x = x, duplic = FALSE, idx = FALSE, seed = 123)
  
  if(length(primary)) xtu[primary][xtu[primary] == 0] <- -1L   # negative values are unused
  if(length(forced))  xtu[forced][xtu[forced] == 0] <- -2L
  
  # to ensure whenEmptyUnsuppressed message as without removeDuplicated
  cand0 <- candidates[xtu[candidates] == 0]
  cand0 <- cand0[!(cand0 %in% primary)]
  cand0 <- cand0[!(cand0 %in% forced)]
  cand0 <- cand0[length(cand0)]
  xtu[cand0] <- -3L
  
  match(xtu, xtu)
}
# # Test using GaussSuppression that DummyDuplicatedSpec works as expected
# library(GaussSuppression)
# z3 <- SSBtoolsData("z3")
# set.seed(102)
# a <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, candidates = sample(1350), forced = sample(1350, size = 50), primary = sample(1350, size = 300), 
#                               singletonMethod = "none", whenEmptyUnsuppressed = warning)
# aw <- length(warnings())
# set.seed(102)
# b <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, candidates = sample(1350), forced = sample(1350, size = 50), primary = sample(1350, size = 300), 
#                               singletonMethod = "none", whenEmptyUnsuppressed = warning, removeDuplicated = FALSE)
# bw <- length(warnings())
# 
# # TRUE TRUE
# identical(a, b)
# identical(c(aw, bw), 4:3)



# Some of the code is similar to GaussSuppression:::FindDifferenceCells
# Example: mm <- ModelMatrix(SSBtoolsData("sprt_emp_withEU")[1:6, 1:2])
#          FindDiffMatrix(mm[, 5:6], mm[, c(1, 5)])
FindDiffMatrix <- function(x, y = x) {
  xty <- As_TsparseMatrix(crossprod(x, y))
  # finds children in x and parents in y
  r <- colSums(x)[xty@i + 1] == xty@x & 
       colSums(y)[xty@j + 1] != xty@x
  child <- xty@i[r] + 1L
  parent <- xty@j[r] + 1L
  diff_matrix <- y[, parent, drop = FALSE] - 
                 x[, child, drop = FALSE]
  colnames(diff_matrix) <- parent
  diff_matrix
}





