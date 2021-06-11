
#' Iterative proportional fitting from matrix input
#' 
#' The linear equation, \code{z = t(x) \%*\% y}, is (hopefully)  solved for \code{y} by
#' iterative proportional fitting
#' 
#' The algorithm will work similar to \code{\link{loglin}} when the input x-matrix is a overparameterized model matrix 
#' – as can be created by \code{\link{ModelMatrix}} and \code{\link{FormulaSums}}. See Examples.
#'
#' @param x a matrix 
#' @param z a single column matrix
#' @param iter maximum number of iterations
#' @param yStart a starting estimate of \code{y}
#' @param eps stopping criterion. Maximum allowed value of \code{max(abs(z - t(x) \%*\% yHat))}
#' @param tol Another stopping criterion. Maximum absolute difference between two iterations. 
#' @param reduceBy0 When TRUE, \code{\link{Reduce0exact}} used within the function 
#' @param reduceByColSums Parameter to \code{\link{Reduce0exact}} (when TRUE)
#' @param reduceByLeverage Parameter to \code{\link{Reduce0exact}} (when TRUE)
#' @param returnDetails More output when TRUE. 
#' @param y It is possible to set \code{z} to NULL and supply original \code{y} instead  (\code{z = t(x) \%*\% y})
#' @param startCol Within the algorithm, \code{x} needs to be split into pieces. 
#'                Vector \code{startCol} is the indices of the first column of each piece (first element is always 1). 
#'                When \code{NULL} (default), splitting is performed automatically. 
#' @param ordSplit Whether to order the pieces of the split x according to a rule
#' @param altSplit Whether to try alternative splitting when ordinary not working properly
#'        
#'
#' @return \code{yHat}, the estimate of \code{y} 
#' 
#' @importFrom methods as
#' @importFrom utils flush.console
#' @importFrom Matrix drop0
#' 
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' 
#' \dontrun{
#' data2 <- SSBtoolsData("z2")
#' x <- ModelMatrix(data2, formula = ~fylke + kostragr * hovedint - 1)
#' z <- t(x) %*% data2$ant  # same as FormulaSums(data2, ant~fylke + kostragr * hovedint -1)
#' yHat <- Mipf(x, z)
#' 
#' #############################
#' # loglm comparison  
#' #############################
#' 
#' if (require(MASS)){
#' 
#' # Increase accuracy
#' yHat <- Mipf(x, z, eps = 1e-04)
#' 
#' # Run loglm and store fitted values in a data frame
#' outLoglm <- loglm(ant ~ fylke + kostragr * hovedint, data2, eps = 1e-04, iter = 100)
#' dfLoglm <- as.data.frame.table(fitted(outLoglm))
#' 
#' # Problem 1: Variable region not in output, but instead the variable .Within.  
#' # Problem 2: Extra zeros since hierarchy not treated. Impossible combinations in output.
#' 
#' # By sorting data, it becomes clear that the fitted values are the same.
#' max(abs(sort(dfLoglm$Freq, decreasing = TRUE)[1:nrow(data2)] - sort(yHat, decreasing = TRUE)))
#' 
#' # Modify so that region is in output. Problem 1 avoided.
#' x <- ModelMatrix(data2, formula = ~region + kostragr * hovedint - 1)
#' z <- t(x) %*% data2$ant  # same as FormulaSums(data2, ant~fylke + kostragr * hovedint -1)
#' yHat <- Mipf(x, z, eps = 1e-04)
#' outLoglm <- loglm(ant ~ region + kostragr * hovedint, data2, eps = 1e-04, iter = 100)
#' dfLoglm <- as.data.frame.table(fitted(outLoglm))
#' 
#' # Now it is possible to merge data
#' merg <- merge(cbind(data2, yHat), dfLoglm)
#' 
#' # Identical output
#' max(abs(merg$yHat - merg$Freq))
#' 
#' }
#' }
#' 
#' #############################
#' # loglin comparison  
#' #############################
#' 
#' 
#' # Generate input data for loglin
#' n <- 5:9
#' tab <- array(sample(1:prod(n)), n)
#' 
#' # Input parameters
#' iter <- 20
#' eps <- 1e-05
#' 
#' # Estimate yHat by loglin
#' out <- loglin(tab, list(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3, 4), c(3, 4, 5)), 
#'               fit = TRUE, iter = iter, eps = eps)
#' yHatLoglin <- matrix(((out$fit)), ncol = 1)
#' 
#' # Transform the data for input to Mipf
#' df <- as.data.frame.table(tab)
#' names(df)[1:5] <- c("A", "B", "C", "D", "E")
#' x <- ModelMatrix(df, formula = ~A:B + A:C + A:D + A:E + B:C:D + C:D:E - 1)
#' z <- t(x) %*% df$Freq
#' 
#' # Estimate yHat by Mipf
#' yHatPMipf <- Mipf(x, z, iter = iter, eps = eps)
#' 
#' # Maximal absolute difference
#' max(abs(yHatPMipf - yHatLoglin))
#' 
#' # Note: loglin reports one iteration extra 
#' 
#' # Another example. Only one iteration needed.
#' max(abs(Mipf(x = FormulaSums(df, ~A:B + C - 1), 
#'              z = FormulaSums(df, Freq ~ A:B + C -1)) 
#'              - matrix(loglin(tab, list(1:2, 3), fit = TRUE)$fit, ncol = 1)))
#' 
#' 
#' #########################################
#' # Examples utilizing Reduce0exact 
#' #########################################
#' 
#' z3 <- SSBtoolsData("z3")
#' x <- ModelMatrix(z3, formula = ~region + kostragr * hovedint + region * mnd2 + fylke * mnd + 
#'                      mnd * hovedint + mnd2 * fylke * hovedint - 1)
#' 
#' # reduceBy0, but no iteration improvement. Identical results.
#' t <- 360
#' y <- z3$ant
#' y[round((1:t) * 432/t)] <- 0
#' z <- t(x) %*% y
#' a1 <- Mipf(x, z, eps = 0.1)
#' a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 0.1)
#' a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 0.1)
#' max(abs(a1 - a2))
#' max(abs(a1 - a3))
#' 
#' 
#' \dontrun{
#' # Improvement by reduceByColSums. Changing eps and iter give more similar results.
#' t <- 402
#' y <- z3$ant
#' y[round((1:t) * 432/t)] <- 0
#' z <- t(x) %*% y
#' a1 <- Mipf(x, z, eps = 1)
#' a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 1)
#' a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 1)
#' max(abs(a1 - a2))
#' max(abs(a1 - a3))
#' 
#' 
#' # Improvement by ReduceByLeverage. Changing eps and iter give more similar results.
#' t <- 378
#' y <- z3$ant
#' y[round((1:t) * 432/t)] <- 0
#' z <- t(x) %*% y
#' a1 <- Mipf(x, z, eps = 1)
#' a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 1)
#' a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 1)
#' a4 <- Mipf(x, z, reduceByLeverage = TRUE, eps = 1)
#' max(abs(a1 - a2))
#' max(abs(a1 - a3))
#' max(abs(a1 - a4))
#' 
#' 
#' # Example with small eps and "Iteration stopped since tol reached"
#' t <- 384
#' y <- z3$ant
#' y[round((1:t) * 432/t)] <- 0
#' z <- t(x) %*% y
#' a1 <- Mipf(x, z, eps = 1e-14)
#' a2 <- Mipf(x, z, reduceBy0 = TRUE, eps = 1e-14)
#' a3 <- Mipf(x, z, reduceByColSums = TRUE, eps = 1e-14)
#' max(abs(a1 - a2))
#' max(abs(a1 - a3))
#' }
#' 
#' # All y-data found by reduceByColSums (0 iterations). 
#' t <- 411
#' y <- z3$ant
#' y[round((1:t) * 432/t)] <- 0
#' z <- t(x) %*% y
#' a1 <- Mipf(x, z)
#' a2 <- Mipf(x, z, reduceBy0 = TRUE)
#' a3 <- Mipf(x, z, reduceByColSums = TRUE)
#' max(abs(a1 - y))
#' max(abs(a2 - y))
#' max(abs(a3 - y))
Mipf <- function(x, z = NULL, iter = 100, yStart = matrix(1, nrow(x), 1), eps = 0.01, tol = 1e-10, 
                  reduceBy0 = FALSE, reduceByColSums = FALSE, 
                  reduceByLeverage = FALSE,
                  returnDetails = FALSE, y = NULL,
                  startCol = NULL,
                  ordSplit = FALSE, altSplit = FALSE) {
  
  if (is.null(z))
    z <- Matrix::crossprod(x, y)
  
  if (is.null(startCol)) {
    xT <- as(drop0(x), "dgTMatrix")
    startCol <- MatrixComponentSplit(xT = xT, altSplit = altSplit)
  } else {
    xT <- NULL
  }
  
  
  if (reduceByLeverage) reduceByColSums <- TRUE
  
  if (reduceByColSums) reduceBy0 <- TRUE
  
  if (reduceBy0)  {
    
    a <- Reduce0exact(x=x, z = z,  
                                 reduceByColSums = reduceByColSums, 
                                 reduceByLeverage = reduceByLeverage,
                                 y = y, yStart = yStart,  printInc =TRUE) 
    
    cat("(",dim(x)[1],"*",dim(x)[2],"->", dim(a$x)[1],"*",dim(a$x)[2],")",sep="")
    
    if(is.na(returnDetails))
      return(a)
    
    yKnown <- a$yKnown
    yHat <-a$y
    
    # Fix startCol after reduce 
    sC <- rep(0, length(a$zSkipped))
    sCseq <- seq_len(length(a$zSkipped))
    for (i in seq_along(startCol)) 
      sC[sCseq >= startCol[i]] <- i
    sC <- sC[!a$zSkipped]
    startCol <- which(!duplicated(sC))
      
    if(any(!yKnown))
      yHat[seq_along(yKnown)[!yKnown], 1] <- Mipf(a$x, a$z, iter = iter, yStart = yStart[seq_along(yKnown)[!yKnown], 1], 
                                                   eps = eps, tol = tol, ordSplit = ordSplit, altSplit = altSplit, startCol=startCol)
    else
      cat("   0 iterations\n")
    
    deviation <- max(abs(crossprod(x, yHat) - z))
    
    
    if (!(deviation < eps)) 
      warning("Deviation limit exceeded")
    cat("Final deviation", deviation, "\n")
    if(returnDetails){
      return(list(x = a$x, z = a$z, yKnown = yKnown, y = yHat))
    }
    return(yHat)
  }
  
  
  cat(":")
  flush.console()
  
  
  
  # Split input into components
  if (is.null(xT)) 
    xT <- as(drop0(x), "dgTMatrix")
  
  startCol <- c(startCol, ncol(x) + 1)
  
  n <- length(startCol) - 1
  
  xL <- vector("list", n)
  zL <- vector("list", n)
    
  for (i in seq_len(n)) {
    xL[[i]] <- xT[, startCol[i]:(startCol[i + 1] - 1), drop = FALSE]
    zL[[i]] <- z[startCol[i]:(startCol[i + 1] - 1), 1, drop = FALSE]
  }
  
  
  
  if (ordSplit) {
    ColSumsMean <- function(x) {
      sum(x)/dim(x)[2]
    }
    ord <- order(sapply(xL, ColSumsMean), decreasing = TRUE)
  } else {
    ord <- seq_len(n)
  }
  
  
  # Run iterative proportional fitting 
  t <- 0
  deviation <- max(abs(crossprod(x, yStart) - z))
  deviationLast  <- Inf 
  k1 <- -1  # Used for printing progress
  while (t < iter & deviation > eps & abs(deviation - deviationLast) > tol) {
    t <- t + 1
    
    # Printing part
    k2 <- round(25 * sqrt(t/iter))
    if (k2 > k1) {
      cat(".")
      flush.console()
    }
    k1 <- k2
    
    # Computation part
    for (i in ord) {
      faktorZ <- zL[[i]]/crossprod(xL[[i]], yStart)
      faktorZ[is.na(faktorZ)] <- 1
      faktor <- rep(1, nrow(x))
      faktor[xL[[i]]@i + 1] <- faktorZ[xL[[i]]@j + 1]
      yStart <- faktor * yStart
    }
    deviationLast <- deviation 
    deviation <- max(abs(crossprod(x, yStart) - z))
    
  }
  if (!(deviation < eps)){
    if(!(abs(deviation - deviationLast) > tol)){
      warning("Iteration stopped since tol reached")  
    } else {
      warning("Iteration limit exceeded")
    }
  }
  cat("   ", t, "iterations: deviation", deviation, "\n")
  if(returnDetails){
    return(list(x = x, z = z, yKnown = rep(FALSE, dim(yStart)[1]), y = yStart))
  }
  yStart
}


MatrixComponentSplit <- function(x = NULL, xT = NULL, altSplit = FALSE, altSplitFORCE = FALSE) {
  if (is.null(xT)) 
    xT <- as(drop0(x), "dgTMatrix")
  cat("-")
  flush.console()
  us <- UniqueSeq(xT@i, xT@j)
  cat("-")
  n <- max(us)
  ma <- match(seq_len(n), us)
  cat("-")
  startCol <- c(xT@j[ma] + 1, ncol(xT) + 1)
  cat("-")
  flush.console()
  
  for (i in seq_len(n)) {
    if (max(Matrix::rowSums(xT[, startCol[i]:(startCol[i + 1] - 1), drop = FALSE])) > 1) 
      altSplitFORCE <- TRUE
  }
  
  
  if (altSplitFORCE) {
    if (!altSplit) {
      stop("Split into component not working properly. Try altSplit=TRUE ?")
    } else {
      warning("Alternative algorithm - since split into component not working properly")
      
      xU <- xT
      
      us <- UniqueSeq(xT@i, xT@j)
      xU@x <- as.numeric(us)
      
      m0 <- 0
      ma <- 0
      
      startCol <- 1
      
      while (!is.na(ma)) {
        ma <- match(2, xU@x)
        if (is.na(ma)) {
          mEnd <- ncol(xU)
        } else {
          mEnd <- min(xU@j[xU@x == 2])
        }
        startCol <- c(startCol, m0 + mEnd + 1)
        rS1 <- rowSums(xT[, m0 + (1:mEnd), drop = FALSE])
        
        if (max(rS1) > 1) 
          stop("something wrong")
        if (!is.na(ma)) {
          xU <- xU[, -(1:mEnd), drop = FALSE]
          w1 <- which(rS1 == 1)
          i1 <- xU@i %in% (w1 - 1)
          xU@x[i1] <- xU@x[i1] - 1
        }
        m0 <- m0 + mEnd
      }
    }
  }
  
  return(startCol[-length(startCol)])
}
