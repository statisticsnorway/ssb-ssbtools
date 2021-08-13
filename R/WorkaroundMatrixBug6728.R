# WorkaroundMatrixBug6728
# https://r-forge.r-project.org/tracker/index.php?func=detail&aid=6728&group_id=61&atid=294
Mult <- function(a, b) {
  ab <- a %*% b
  if (ncol(ab)>0 & is.null(colnames(ab))) {
    colnames(ab) <- colnames(b)
  }
  if (nrow(ab)>0 & is.null(rownames(ab))) {
    rownames(ab) <- rownames(a)
  }
  ab
}


Mult_crossprod <- function(a, b) {
  Mult(t(a), b) 
}

Mult_tcrossprod <- function(a, b) {
  Mult(a, t(b)) 
}



  

