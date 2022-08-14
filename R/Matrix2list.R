#' Convert matrix to sparse list
#' 
#' @details  Within the function, the input matrix is first converted to a dgTMatrix matrix (Matrix package).
#'
#' @param x Input matrix 
#'
#' @return A two-element list: List of row numbers (r) and a list of numeric or integer values (x)
#' @importFrom methods as
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' m = matrix(c(0.5, 1.1, 3.14, 0, 0, 0, 0, 4, 5), 3, 3)
#' Matrix2list(m)
#' Matrix2listInt(m)
Matrix2list <- function(x) {
  x <- uniqTsparse(As_TsparseMatrix(x)) # x <- uniqTsparse(as(drop0(x), "dgTMatrix"))
  ncolx <- ncol(x) 
  
  if(length(x@j)){
    colf <- list(factor(x@j + 1L, levels = seq_len(ncolx)))
    z <- list(r = aggregate(x@i + 1L, by = colf, FunX, drop = FALSE, simplify = FALSE)[[2]], 
              x = aggregate(x@x, by = colf, FunX, drop = FALSE, simplify = FALSE)[[2]])
  } else {
    z <- list(r = vector("list",ncolx), 
             x = vector("list",ncolx))
  }
  if(length(z[[1]]) != ncolx){  # Fix for aggregate in old R versions (< 3.5.0)
    z_ <- z
    z <- list(r = vector("list",ncolx), 
              x = vector("list",ncolx))
    ind <- aggregate(rep(1L, length(colf[[1]])), by = colf, FunX, drop = FALSE, simplify = FALSE)[[1]]
    z$r[ind] <- z_$r
    z$x[ind] <- z_$x
  }
  for (i in seq_len(ncolx)) {
    if (is.null(z$r[[i]])) {
      z$r[[i]] <- integer(0)
      z$x[[i]] <- numeric(0)
    }
  }
  z
}


#' @rdname Matrix2list
#' @export
#' @note \code{Matrix2listInt} convers the values to integers by \code{as.integer} 
#'     and no checking is performed. Thus, zeros are possible. 
Matrix2listInt <- function(x) {
  x <- uniqTsparse(As_TsparseMatrix(x)) # x <- uniqTsparse(as(drop0(x), "dgTMatrix"))
  ncolx <- ncol(x)
  
  if(length(x@j)){
    colf <- list(factor(x@j + 1L, levels = seq_len(ncolx)))
    z <- list(r = aggregate(x@i + 1L, by = colf, FunX, drop = FALSE, simplify = FALSE)[[2]], 
              x = aggregate(x@x, by = colf, FunXint, drop = FALSE, simplify = FALSE)[[2]])
  } else {
    z <- list(r = vector("list",ncolx), 
              x = vector("list",ncolx))
  }
  if(length(z[[1]]) != ncolx){  # Fix for aggregate in old R versions (< 3.5.0)
    z_ <- z
    z <- list(r = vector("list",ncolx), 
              x = vector("list",ncolx))
    ind <- aggregate(rep(1L, length(colf[[1]])), by = colf, FunX, drop = FALSE, simplify = FALSE)[[1]]
    z$r[ind] <- z_$r
    z$x[ind] <- z_$x
  }
  for (i in seq_len(ncolx)) {
    if (is.null(z$r[[i]])) {
      z$r[[i]] <- integer(0)
      z$x[[i]] <- integer(0)
    }
  }
  z
}

FunX <- function(x) {
  x
}

FunXint <- function(x) {
  as.integer(x)
}

