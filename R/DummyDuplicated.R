

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
  
  xtu <- vector("list", 3)
  ma <- vector("list", 3)
  
  return_i <- 1
  
  for (i in 1:3) {
    xtu[[i]] <- XprodRnd1(x = x, rows = rows)
    ma[[i]] <- match(xtu[[i]], xtu[[i]])
    if (duplic | idx) {
      xtu[[i]] <- 0
    }
  }
  
  if (!identical(ma[[1]], ma[[2]])) {
    return_i <- 3
    if (!(identical(ma[[1]], ma[[3]]) | identical(ma[[2]], ma[[3]]))) {
      warning("Rare random event occurred")
    } else {
      stop("Duplicated by random method did not work")
    }
  } else {
    if (!identical(ma[[1]], ma[[3]])) {
      warning("Rare random event occurred when duplicated by random method")
    }
  }
  if (idx) {
    return(ma[[return_i]])
  }
  
  if (duplic) {
    return(ma[[return_i]] != seq_along(ma[[return_i]]))
  }
  
  xtu[[return_i]]
  
}



# Note that u <- runif(n) leads to problems since not perfect uniform distribution  
#    set.seed(111)
#    u <- runif(100000)
#    identical(sum(u[c(604, 3986)]), sum(u[c(78844, 78998)]))
#    identical(sum(u[c(16136, 18320, 18478)]), u[74159])
XprodRnd1 <- function(x, rows) {
  if (rows) {
    n <- ncol(x)
  } else {
    n <- nrow(x)
  }
  u <- (runif(n) - 0.5) * (2 + ((1:n)/n))
  if (rows) {
    return(as.vector(x %*% u))
  }
  as.vector(crossprod(x, u))
}






