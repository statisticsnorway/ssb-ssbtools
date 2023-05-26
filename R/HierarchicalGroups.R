
#' Finding hierarchical variable groups
#'
#' According to the (factor) levels of the variables
#'
#' @encoding UTF8
#'
#' @param x Matrix or data frame containing the variables
#' @param mainName When TRUE output list is named according to first variable in group.
#' @param eachName When TRUE variable names in output instead of indices.
#' @param fCorr When non-null, x is not needed as input.
#'
#' @return Output is a list containing the groups. First variable has most levels.
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' dataset <- SSBtoolsData("example1")
#' HierarchicalGroups(dataset[1:2], eachName = TRUE)
#' HierarchicalGroups(dataset[2:3])
#' HierarchicalGroups(dataset[1:4], eachName = TRUE)
#' 
#' HierarchicalGroups(SSBtoolsData("magnitude1")[1:4])
#' 
#'  x <- rep(c("A","B","C"),3)
#'  y <- rep(c(11,22,11),3)
#'  z <- c(1,1,1,2,2,2,3,3,3)
#'  zy <- paste(z,y,sep="")
#'  m <- cbind(x,y,z,zy)
#'  HierarchicalGroups(m)
HierarchicalGroups <- function(x = NULL, mainName = TRUE, eachName = FALSE, fCorr = FactorLevCorr(x)) {
  nLevels <- diag(fCorr)
  if (min(nLevels) <= 1) 
    stop("Number of levels < 2 in a variable")
  ix <- order(nLevels, decreasing = TRUE)
  # print(fCorr[ix,ix])
  if (length(fCorr) > 1) 
    z <- functionRecursive(fCorr[ix, ix], 1:NCOL(fCorr))$l else z <- list(1)
  z1 <- rep(NA, length(z))
  for (i in 1:length(z)) {
    z[[i]] <- ix[z[[i]]]
    z1[i] <- z[[i]][1]
    if (mainName) 
      names(z)[i] <- colnames(fCorr)[z1[i]]
  }
  z <- SortNrList(z)
  for (i in 1:length(z)) {
    if (length(unique(nLevels[z[[i]]])) != length(z[[i]])) 
      warning("There are identical variables")
  }
  if (eachName) {
    for (i in 1:length(z)) z[[i]] <- colnames(fCorr)[z[[i]]]
  }
  z
}


# Recursive algorithm ...
functionRecursive <- function(fCorr, ind) {
  drop <- numeric(0)
  x <- vector("list", 0)
  for (i in ind) if (!(i %in% drop)) {
    z <- functionRecursive(fCorr, (1:NCOL(fCorr))[fCorr[i, ] == -1])
    drop <- c(drop, i, z$drop)
    l <- z$l
    for (k in matlabColon(1, length(l))) l[[k]] <- c(i, l[[k]])
    if (!length(l)) 
      l <- list(i)
    x <- c(x, l)
  }
  list(drop = drop, l = x)
}

# Special sorting function
SortNrList <- function(x, index.return = FALSE) {
  m <- matrix(0, length(x), max(sapply(x, length)))
  for (i in seq_len(length(x))) m[i, seq_len(length(x[[i]]))] <- x[[i]]
  ix <- SortRows(m, index.return = TRUE)
  if (index.return) 
    return(ix)
  x[ix]
}