#' Reducing a non-negative regression problem 
#' 
#' The linear equation problem, \code{z = t(x) \%*\% y} with y non-negative and x as a design (dummy) matrix, 
#' is reduced to a smaller problem by identifying elements of `y` that can be found exactly from `x` and `z`. 
#' 
#' Exact elements can be identified in three ways in an iterative manner: 
#' 1.	By zeros in `z`. This is always done.
#' 2.	By columns in x with a singe nonzero value. Done when `reduceByColSums` or `reduceByLeverage` is `TRUE`.  
#' 3.	By exact linear regression fit (when leverage is one). Done when `reduceByLeverage` is `TRUE`. 
#'    The leverages are computed by \code{hat(as.matrix(x), intercept = FALSE)}, which can be very time and memory consuming.
#'    Furthermore, without `y` in input, known values will be computed by \code{\link{ginv}}. 
#' 
#'
#' @param x A matrix 
#' @param z A single column matrix
#' @param reduceByColSums See Details
#' @param reduceByLeverage  See Details 
#' @param leverageLimit Limit to determine perfect fit 
#' @param digitsRoundWhole \code{\link{RoundWhole}} parameter for fitted values (when `leverageLimit` and `y` not in input) 
#' @param y A single column matrix. With `y` in input, `z` in input can be omitted and estimating `y` (when `leverageLimit`) is avoided. 
#' @param yStart A starting estimate when this function is combined with iterative proportional fitting. Zeros in yStart will be used to reduce the problem. 
#' @param printInc Printing iteration information to console when TRUE
#'        
#' @return A list of five elements:
#' * `x`: A reduced version of input `x`
#' * `z`: Corresponding reduced `z`
#' * `yKnown`: Logical, specifying known values of `y`
#' * `y`: A version of `y` with known values correct and others zero
#' * `zSkipped`: Logical, specifying omitted columns of `x`   
#' 
#' 
#' @importFrom methods as
#' @importFrom utils flush.console
#' @importFrom Matrix rowSums crossprod
#' @importFrom MASS ginv
#' @importFrom stats hat
#' 
#' @export
#' @author Øyvind Langsrud
#' 
#' @examples 
#' # Make a special data set
#' d <- SSBtoolsData("sprt_emp")
#' d$ths_per <- round(d$ths_per)
#' d <- rbind(d, d)
#' d$year <- as.character(rep(2014:2019, each = 6))
#' to0 <- rep(TRUE, 36)
#' to0[c(6, 14, 17, 18, 25, 27, 30, 34, 36)] <- FALSE
#' d$ths_per[to0] <- 0
#' 
#' # Values as a single column matrix
#' y <- Matrix(d$ths_per, ncol = 1)
#' 
#' # A model matrix using a special year hierarchy
#' x <- Hierarchies2ModelMatrix(d, hierarchies = list(geo = "", age = "", year = 
#'     c("y1418 = 2014+2015+2016+2017+2018", "y1519 = 2015+2016+2017+2018+2019", 
#'       "y151719 = 2015+2017+2019", "yTotal = 2014+2015+2016+2017+2018+2019")), 
#'       inputInOutput = FALSE)
#' 
#' # Aggregates 
#' z <- t(x) %*% y
#' sum(z == 0)  # 5 zeros
#' 
#' # From zeros in z
#' a <- Reduce0exact(x, z)
#' sum(a$yKnown)   # 17 zeros in y is known
#' dim(a$x)        # Reduced x, without known y and z with zeros 
#' dim(a$z)        # Corresponding reduced z 
#' sum(a$zSkipped) # 5 elements skipped 
#' t(a$y)          # Just zeros (known are 0 and unknown set to 0) 
#' 
#' # It seems that three additional y-values can be found directly from z
#' sum(colSums(a$x) == 1)
#' 
#' # But it is the same element of y (row 18)
#' a$x[18, colSums(a$x) == 1]
#' 
#' # Make use of ones in colSums
#' a2 <- Reduce0exact(x, z, reduceByColSums = TRUE)
#' sum(a2$yKnown)          # 18 values in y is known
#' dim(a2$x)               # Reduced x
#' dim(a2$z)               # Corresponding reduced z
#' a2$y[which(a2$yKnown)]  # The known values of y (unknown set to 0)
#' 
#' # Six ones in leverage values 
#' # Thus six extra elements in y can be found by linear estimation
#' hat(as.matrix(a2$x), intercept = FALSE)
#' 
#' # Make use of ones in leverages (hat-values)
#' a3 <- Reduce0exact(x, z, reduceByLeverage = TRUE)
#' sum(a3$yKnown)          # 26 values in y is known (more than 6 extra)
#' dim(a3$x)               # Reduced x
#' dim(a3$z)               # Corresponding reduced z
#' a3$y[which(a3$yKnown)]  # The known values of y (unknown set to 0)
#' 
#' # More than 6 extra is caused by iteration 
#' # Extra checking of zeros in z after reduction by leverages 
#' # Similar checking performed also after reduction by colSums
#'
Reduce0exact <- function(x, z = NULL, reduceByColSums = FALSE, reduceByLeverage = FALSE, 
                         leverageLimit = 0.999999, digitsRoundWhole = 9, y = NULL, yStart = NULL, 
                         printInc = FALSE) {
  if (is.null(z)) 
    z <- crossprod(x, y)
  
  if (reduceByLeverage) 
    reduceByColSums <- TRUE
  
  if (printInc) {
    cat("-")
    flush.console()
  }
  snx <- seq_len(nrow(x))
  flush.console()
  a <- ReduceBy0(x, z, yStart)
  aKnown <- as.vector(a$yKnown)
  
  # yHat <- Matrix(0, length(aKnown), 1)
  if (is.null(y)) {
    yHat <- x[, 1, drop = FALSE]
    colnames(yHat) <- NULL
  } else {
    yHat <- y
  }
  
  yHat[] <- 0
  
  yKnown <- aKnown
  zSkipped <- as.vector(a$zSkipped)
  if (printInc & any(aKnown)) {
    cat("z")
    flush.console()
  }
  rerun <- reduceByColSums
  
  while (rerun) {
    rerun <- FALSE
    a <- ReduceByColSums(a$x, a$z)
    aKnown <- as.vector(a$yKnown)
    if (any(aKnown)) {
      if (printInc) {
        cat("x")
        flush.console()
      }
      rerun <- TRUE
    } else {
      if (reduceByLeverage & min(dim(a$x)) > 0) {
        if (printInc) {
          cat("=")
          flush.console()
        }
        a <- ReduceByLeverage(a$x, a$z, y = y[!yKnown, , drop = FALSE], leverageLimit = leverageLimit, digits = digitsRoundWhole)
        aKnown <- as.vector(a$yKnown)
        if (any(aKnown)) {
          if (printInc) 
            cat("H")  # rerun only when ReduceBy0, code below
        }
      }
    }
    if (any(aKnown)) {
      flush.console()
      yHat[(snx[!yKnown])[seq_along(aKnown)[aKnown]], 1] <- a$y[seq_along(aKnown)[aKnown], 1]
      yKnown[!yKnown] <- aKnown
      zSkipped[!zSkipped] <- as.vector(a$zSkipped)
      if (printInc) {
        cat("-")
        flush.console()
      }
      a <- ReduceBy0(a$x, a$z)
      aKnown <- as.vector(a$yKnown)
      if (any(aKnown)) {
        rerun <- TRUE
        if (printInc) {
          cat("z")
          flush.console()
        }
        yKnown[!yKnown] <- aKnown
        zSkipped[!zSkipped] <- as.vector(a$zSkipped)
      }
    }
  }
  
  if (printInc == 2) {
    cat("(", dim(x)[1], "*", dim(x)[2], "->", dim(a$x)[1], "*", dim(a$x)[2], ")", sep = "")
  }
  
  return(list(x = a$x, z = a$z, yKnown = yKnown, y = yHat, zSkipped = zSkipped))
}





# A modified version of reduceX in package RegSDC
# In this modified version use of Z2Yhat and RoundWhole is removed.
ReduceByColSums <- function(x, z = NULL, y = NULL) {
  yNULL <- is.null(y)
  zNULL <- is.null(z)
  if (yNULL & zNULL) 
    stop("z or y must be supplied")
  colSums_1 <- which(colSums(x) == 1)
  x1 <- x[, colSums_1, drop = FALSE]
  x1dgT <- as(x1, "dgTMatrix")
  nonDub <- x1dgT@j[x1dgT@x != 0][!duplicated(x1dgT@i[x1dgT@x != 0])] + 1L
  x1 <- x1[, nonDub, drop = FALSE]
  if (!zNULL) 
    zA <- z[colSums_1[nonDub], , drop = FALSE]
  zA1 <- matrix(1, NCOL(x1), 1)
  yKnown1 <- round(x1 %*% zA1)
  yKnown1_0 <- which(yKnown1 == 0)
  if (yNULL) {
    yHat <- x1 %*% zA
  } else {
    yHat <- y
    yHat[yKnown1_0, ] <- 0
  }
  if (!zNULL) 
    z <- z - crossprod(x, yHat)
  x <- x[yKnown1_0, , drop = FALSE]
  colSumsXnot0 <- colSums(x) != 0
  colSums_ok <- which(colSumsXnot0)
  if (!zNULL) 
    z <- z[colSums_ok, , drop = FALSE]
  x <- x[, colSums_ok, drop = FALSE]
  if (yNULL) {
    if (length(yKnown1_0)) 
      yHat[yKnown1_0, ] <- 0
  } else {
    yHat <- y
  }
  list(x = x, z = z, yKnown = yKnown1 != 0, y = yHat, zSkipped = !colSumsXnot0)
}



ReduceBy0 <- function(x, z, yStart = NULL) {
  z0 <- as.vector(as.matrix(z)) == 0
  y0 <- rowSums(x[, z0, drop = FALSE]) > 0
  
  if (!is.null(yStart)) 
    y0 <- y0 | (yStart == 0)
  
  if (!any(y0)) 
    return(list(x = x, z = z, yKnown = y0, zSkipped = rep(FALSE, length(z0))))
  
  ny0 <- seq_along(y0)[!y0]
  nz0 <- seq_along(z0)[!z0]
  x <- x[ny0, nz0, drop = FALSE]
  return(list(x = x, z = z[nz0, , drop = FALSE], yKnown = y0, zSkipped = z0))
}



# Reduce based on perfect fit
ReduceByLeverage <- function(x, z, leverageLimit = 0.999999, y = NULL, digits = 9) {
  
  leverages <- hat(as.matrix(x), intercept = FALSE)
  
  if (is.null(y)) {
    yHat <- ginv(as.matrix(t(x))) %*% z
    yHat <- RoundWhole(yHat, digits = digits)
  } else {
    yHat <- y
  }
  
  yKnown <- leverages > leverageLimit
  
  if (!any(yKnown)) 
    return(list(x = x, z = z, yKnown = yKnown, y = yHat))
  
  yK <- which(yKnown)
  yN <- which(!yKnown)
  
  zKnown <- colSums(x[yN, , drop = FALSE]) == 0
  
  zK <- which(zKnown)
  zN <- which(!zKnown)
  
  z <- z[zN, , drop = FALSE]
  
  z <- z - crossprod(x[yK, zN, drop = FALSE], yHat[yK, , drop = FALSE])
  
  return(list(x = x[yN, zN, drop = FALSE], z = z, yKnown = yKnown, y = yHat, zSkipped = zKnown))
}



#' Sequence within unique values 
#'
#' @param x vector 
#' @param sortdata matrix or vector to determine sequence order
#'
#' @return integer vector
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # 1:4 within A and 1:2 within B
#' UniqueSeq(c("A", "A", "B", "B", "A", "A"))
#' 
#' # Ordered differently
#' UniqueSeq(c("A", "A", "B", "B", "A", "A"), c(4, 5, 20, 10, 3, 0))
UniqueSeq <- function(x, sortdata = matrix(1L, length(x), 0)) {
  ix <- SortRows(cbind(x, sortdata), index.return = TRUE)
  nr <- seq_len(length(x))
  sortx <- x[ix]
  (1L + nr - nr[match(sortx, sortx)])[order(ix)]
}



#' Round values that are close two whole numbers
#'
#' @param x vector or matrix
#' @param digits parameter to \code{\link{round}}
#' @param onlyZeros Only round values close to zero 
#'
#' @return Modified x
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- c(0.0002, 1.00003, 3.00014)
#' RoundWhole(x)     # No values rounded
#' RoundWhole(x, 4)  # One value rounded
#' RoundWhole(x, 3)  # All values rounded
#' RoundWhole(x, 3, TRUE)  # One value rounded
RoundWhole <- function(x, digits = 9, onlyZeros = FALSE) {
  if (is.null(digits))
    return(x)
  round_x <- round(x)
  round_x_digits <- round(x, digits = digits)
  if (onlyZeros) 
    toWhole <- round_x_digits == 0 
  else 
    toWhole <- round_x == round_x_digits
  toWhole[is.na(toWhole)] <- FALSE
  x[toWhole] <- round_x[toWhole]
  x
}