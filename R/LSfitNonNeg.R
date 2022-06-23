
#' Non-negative regression fits with a sparse overparameterized model matrix
#' 
#' Assuming  `z = t(x) %*% y  + noise`, a non-negatively modified least squares estimate of `t(x) %*% y` is made.
#' 
#' The problem is first reduced by elimination some rows of `x`  (elements of `y`) using \code{\link{GaussIndependent}}. 
#' Thereafter least squares fits are obtained using \code{\link{solve}} or \code{\link{qr}}.  
#' Possible negative fits will be forced to zero in the next estimation iteration(s).
#' 
#' 
#' @encoding UTF8
#'
#' @param x A matrix
#' @param z A single column matrix
#' @param limit Lower limit for non-zero fits. Set to `NULL` or `-Inf` to avoid the non-zero restriction.  
#' @param viaQR Least squares fits obtained using \code{\link{qr}} when `TRUE`.
#' @param printInc Printing "..." to console when `TRUE`.
#'  
#' @return A fitted version of `z`
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' set.seed(123)
#' data2 <- SSBtoolsData("z2")
#' x <- ModelMatrix(data2, formula = ~fylke + kostragr * hovedint - 1)
#' z <- t(x) %*% data2$ant + rnorm(ncol(x), sd = 3)
#' LSfitNonNeg(x, z)
#' LSfitNonNeg(x, z, limit = NULL)
#' 
#' \dontrun{
#' mf <- ~region*mnd + hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd
#' data4 <- SSBtoolsData("sosialFiktiv")
#' x <- ModelMatrix(data4, formula = mf)
#' z <- t(x) %*% data4$ant + rnorm(ncol(x), sd = 3)
#' zFit <- LSfitNonNeg(x, z)
#' }
LSfitNonNeg <- function(x, z, limit = 1e-10, viaQR = FALSE, printInc = TRUE) {
  
  if (printInc) {
    cat("(")
    flush.console()
  }
  dd <- DummyDuplicated(x, rows = TRUE, rnd = TRUE)
  
  if (any(dd)) {
    if (printInc) {
      cat("-")
      flush.console()
    }
    x <- x[!dd, , drop = FALSE]
  }
  
  if (printInc) {
    cat(")")
    flush.console()
  }
  
  if (is.null(limit)) {
    limit <- -Inf
  }
  
  if (printInc) {
    printInc <- 10L
  }
  
  zFit <- LSfitted(z, xTranspose = x, viaQR = viaQR, printInc = printInc)
  
  if (printInc) {
    printInc <- 5L
  }
  
  sum_z0 <- 0
  z0 <- as.vector(zFit < limit)
  
  while (sum(z0) > sum_z0) {
    sum_z0 <- sum(z0)
    z[z0] <- 0
    zFit[z0] <- 0
    a <- Reduce0exact(x = x, z = z, printInc = printInc)
    zFit[a$zSkipped] <- 0
    z[a$zSkipped] <- 0
    zFit[!a$zSkipped] <- LSfitted(a$z, xTranspose = a$x, viaQR = viaQR, printInc = printInc)
    z0 <- zFit < limit
  }
  zFit
}


QRfitted <- function(qr_x, y, n_qr_col) {
  if (!inherits(qr_x, "sparseQR")) {    # if (class(qr_x) != 'sparseQR') {
    y <- as.matrix(y)
    drop0 <- function(x) x
  }
  qty <- qr.qty(qr_x, y)
  qty[SeqInc(n_qr_col + 1, nrow(qty)), ] <- 0
  qr.qy(qr_x, drop0(qty))
}



LSfitted <- function(y, x = NULL, xTranspose = NULL, viaQR = FALSE, viaTranspose = TRUE, printInc = TRUE) {
  if (is.null(x)) {
    if (viaTranspose) {
      columns <- GaussIndependent(xTranspose, printInc = printInc)[[1]]
    } else {
      columns <- GaussIndependent(t(xTranspose), printInc = printInc)[[2]]
    }
    x <- t(xTranspose[columns, , drop = FALSE])
  } else {
    if (viaTranspose) {
      columns <- GaussIndependent(t(x), printInc = printInc)[[1]]
    } else {
      columns <- GaussIndependent(x, printInc = printInc)[[2]]
    }
    x <- x[, columns, drop = FALSE]
  }
  if (viaQR) {
    return(QRfitted(qr(x), y, ncol(x)))
  }
  x %*% solve(crossprod(x), crossprod(x, y))
}

