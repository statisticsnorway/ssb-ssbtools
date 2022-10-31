

#' Duplicated columns in dummy matrix  
#' 
#' The algorithm is based on `crossprod(x)` or `crossprod(x, u)` where `u` is a vector of random numbers  
#' 
#' The efficiency of the default algorithm depends on the sparsity of `crossprod(x)`.
#' The random values are generated locally within the function without affecting the random value stream in R.
#'
#' @param x A matrix
#' @param idx Indices returned when TRUE
#' @param rows Duplicated rows instead when TRUE
#' @param rnd Algorithm based on cross product with random numbers when TRUE (dummy matrix not required)  
#'
#' @note `DummyDuplicated` calls `XprodRnd` with `123` as seed when `rnd` is `TRUE`.
#'        `XprodRnd` performs three runs with different random numbers.  
#'        A warning is produced if one deviates. Error occurs if all three are different.
#'
#' @return Logical vectors specifying duplicated columns or vector of indices (first match)
#' @importFrom stats runif
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- cbind(1, rbind(diag(2), diag(2)), diag(4)[, 1:2])
#' z <- Matrix(x[c(1:4, 2:3), c(1, 2, 1:5, 5, 2)])
#' 
#' DummyDuplicated(z)
#' which(DummyDuplicated(z, rows = TRUE))
#' 
#' # Four ways to obtain the same result
#' DummyDuplicated(z, idx = TRUE)
#' DummyDuplicated(z, idx = TRUE, rnd = TRUE)
#' DummyDuplicated(t(z), idx = TRUE, rows = TRUE)
#' DummyDuplicated(t(z), idx = TRUE, rows = TRUE, rnd = TRUE)
#' 
#' # The unique values in four ways 
#' which(!DummyDuplicated(z), )
#' which(!DummyDuplicated(z, rnd = TRUE))
#' which(!DummyDuplicated(t(z), rows = TRUE))
#' which(!DummyDuplicated(t(z), rows = TRUE, rnd = TRUE))
DummyDuplicated <- function(x, idx = FALSE, rows = FALSE, rnd = FALSE) {
  if (rnd) {
    return(XprodRnd(x = x, idx = idx, rows = rows, seed = 123)) 
  }
  if (inherits(x, "matrix")) {  #  if (class(x)[1] == "matrix") {
    x <- Matrix(x)
  }
  if (rows) {
    #      k <- as(as(triu(tcrossprod(x), 1), "dgCMatrix"), "dgTMatrix")
    k <- As_TsparseMatrix(triu(tcrossprod(x), 1)) 
    colSums_x <- rowSums(x)
    if (idx) {
      o <- seq_len(nrow(x))
    } else {
      o <- rep(FALSE, nrow(x))
    }
  } else {
    #      k <- as(as(triu(crossprod(x), 1), "dgCMatrix"), "dgTMatrix")
    k <- As_TsparseMatrix(triu(crossprod(x), 1))
    colSums_x <- colSums(x)
    if (idx) {
      o <- seq_len(ncol(x))
    } else {
      o <- rep(FALSE, ncol(x))
    }
  }
  r <- which(colSums_x[k@j + 1] == k@x & colSums_x[k@i + 1] == k@x)
  if (idx) {
    r <- r[!(k@i[r] %in%  k@j[r])]
    o[k@j[r] + 1] <- o[k@i[r] + 1]
    return(o)
  }
  o[unique(k@j[r] + 1)] <- TRUE
  o
}



#' @rdname DummyDuplicated
#' @param duplic XprodRnd parameter: When `duplic` and `idx` are `FALSE`, this function returns `crossprod(x,u)` or `x%*%u` instead of indices or duplicated.
#' @param seed XprodRnd parameter: Seed to be used. When NULL the ordinary random value stream in R continues.
#' @export
XprodRnd <- function(x, duplic = TRUE, idx = FALSE, rows = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(seed)
  }
  
  nRep <- 7
  nClaim <- 3
  n <- dim(x)[2-rows]
  
  ma <- matrix(0L, n, nRep)
  
  for (i in 1:nRep) {
    xtu <- XprodRnd1(x = x, rows = rows)
    ma[, i] <- match(xtu, xtu)
  }
  
  maMax <- apply(ma, 1, max)  # RowMax
  
  min_n_maMax <- min(rowSums(matrix(maMax, n, nRep) == ma))
  
  # if (min_n_maMax < 7) cat("min_n_maMax = ", min_n_maMax, "\n")
  
  if (min_n_maMax < nClaim) {
    stop("Duplicated by random method did not work")
  }
  
  if (idx) {
    return(maMax)
  }
  
  if (duplic) {
    return(maMax != seq_along(maMax))
  }
  
  return(maMax + 0.5)   # Will be removed. This is a temporary version of the function. 
  
}



#  Now changed to whole numbers by Sample_Symmetric_integer.max
XprodRnd1 <- function(x, rows) {
  if (rows) {
    n <- ncol(x)
  } else {
    n <- nrow(x)
  }
  u <- Sample_Symmetric_integer.max(size = n)
  if (rows) {
    return(as.vector(x %*% u))
  }
  as.vector(crossprod(x, u))
}


# From uniform dist:  std = 2 * .Machine$integer.max * sqrt(1/12) = 1239850262
# Std of sum of .Machine$integer.max values = stdMax = std * sqrt(.Machine$integer.max) = 5.745584e+13
# Number of stdMax to reach 9E15 (largest whole number stored exactly by the numeric data type) 9E15/stdMax = 157
#    prob reach 157 std = 0, Prob reach 37 std = 2*pnorm(-37) = 1.145114e-299
# Conclusion: With x dummy, Abs of all values of crossprod(x, u) always exactly calculated when u sampled from this function. 
# Note: replace = FALSE
Sample_Symmetric_integer.max <- function(size, replace = FALSE, n = .Machine$integer.max) {
  a <- sample.int(n = n, size = size, replace = replace)
  s <- sample(c(-1L, 1L), size = size, replace = TRUE)
  as.numeric(s * a)
}


















