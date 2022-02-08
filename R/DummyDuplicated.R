

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
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(123)
    if (rows) {
      xtu <- as.vector(x %*% runif(ncol(x)))
    } else {
      xtu <- as.vector(crossprod(x, runif(nrow(x))))
    }
    if (idx) {
      return(match(xtu, xtu))
    }
    return(duplicated(xtu))
  }
  if (class(x)[1] == "matrix") {
    x <- Matrix(x)
  }
  if (rows) {
    #      k <- as(as(triu(tcrossprod(x), 1), "dgCMatrix"), "dgTMatrix")
    k <- As_dgTMatrix(triu(tcrossprod(x), 1)) 
    colSums_x <- rowSums(x)
    if (idx) {
      o <- seq_len(nrow(x))
    } else {
      o <- rep(FALSE, nrow(x))
    }
  } else {
    #      k <- as(as(triu(crossprod(x), 1), "dgCMatrix"), "dgTMatrix")
    k <- As_dgTMatrix(triu(crossprod(x), 1))
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


As_dgTMatrix <- function(x) {
  class_x <- class(x)[1]
  if (class_x %in% c("dgCMatrix", "dgeMatrix")) {
    return(as(x, "dgTMatrix"))
  }
  if (class_x %in% c("dtrMatrix")) {
    return(as(as(x, "dgeMatrix"), "dgTMatrix"))
  }
  as(as(x, "dgCMatrix"), "dgTMatrix")
}