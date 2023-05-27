#' Factor level correlation
#'
#' A sort of correlation matrix useful to detect (hierarchical) relationships between the levels of factor variables.
#'
#'
#' @encoding UTF8
#' @noMd
#'
#' @param x Input matrix or data frame containing the variables
#'
#' @return Output is a sort of correlation matrix.
#'
#'         Here we refer to ni as the number of present levels of variable i (the number of unique elements) and we refer to nij as the number
#'         of present levels obtained by crossing variable i and variable j (the number unique rows of x[,c(i,j)]).
#'
#'         The diagonal elements of the output matrix contains the number of present levels of each variable (=ni).
#'
#'         The absolute values of off-diagonal elements:
#'         \item{0}{when nij = ni*nj}
#'         \item{1}{when nij = max(ni,nj)}
#'         \item{Other values}{Computed as (ni*nj-nij)/(ni*nj-max(ni,nj))}
#'
#'         So 0 means that all possible level combinations exist in the data and 1 means that the two variables are
#'         hierarchically related.
#'
#'         The sign of off-diagonal elements:
#'         \item{positive}{when ni<nj}
#'         \item{negative}{when ni>nj}
#'
#'         In cases where ni=nj elements will be positive above the diagonal and negative below.
#'
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#'  x <- rep(c("A","B","C"),3)
#'  y <- rep(c(11,22,11),3)
#'  z <- c(1,1,1,2,2,2,3,3,3)
#'  zy <- paste(z,y,sep="")
#'  m <- cbind(x,y,z,zy)
#'  FactorLevCorr(m)
#'
FactorLevCorr <- function(x) {
  if (NROW(x) <= 1) 
    stop(paste(NROW(x), "row in data."))
  x <- unique(x, MARGIN = 1)
  n <- NCOL(x)
  nLevels <- rep(NaN, n)
  for (i in matlabColon(1, n)) nLevels[i] <- Nlevels(x[, i])
  z <- diag(nLevels, nrow = length(nLevels))  # nrow input to allow length one as input
  for (i in matlabColon(1, n)) for (j in matlabColon(i + 1, n)) {
    ni <- nLevels[i]
    nj <- nLevels[j]
    nij <- Nlevels(x[, c(i, j)])
    multij <- ni * nj
    maxij <- max(ni, nj)
    if (ni <= nj) 
      one <- 1 else one <- -1
    if (nij == maxij) 
      z[i, j] <- one else z[i, j] <- one * (multij - nij)/(multij - maxij)
    z[j, i] <- -z[i, j]
  }
  colnames(z) <- colnames(x)
  rownames(z) <- colnames(x)
  z
}


Nlevels = function(x){
  NROW(unique(x,MARGIN=1))
}