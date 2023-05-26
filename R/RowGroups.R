#' Create numbering according to unique rows
#'
#' @param x Data frame or matrix
#' @param returnGroups   When TRUE unique rows are returned
#' @param returnGroupsId When TRUE Index of unique rows are returned
#'
#' @return A vector with the numbering or, according to the arguments, 
#'         a list with more output.
#' @export
#' @author Øyvind Langsrud
#' 
#' @examples 
#' a <- data.frame(x = c("a", "b"), y = c("A", "B", "A"), z = rep(1:4, 3))
#' RowGroups(a)
#' RowGroups(a, TRUE)
#' RowGroups(a[, 1:2], TRUE, TRUE)
#' RowGroups(a[, 1, drop = FALSE], TRUE)
RowGroups <- function(x, returnGroups = FALSE, returnGroupsId = FALSE) {
  
  if (NROW(x) == 0) 
    return(RowGroups0rows(x = x, returnGroups = returnGroups, returnGroupsId = returnGroupsId))
  
  xInteger <- AsFactorInteger(x)
  if (!is.null(xInteger)) {
    if (NCOL(xInteger) == 1) {
      ix <- order(xInteger)
      dp <- duplicated(xInteger)
    } else {
      ix <- SortRows(xInteger, index.return = TRUE)
      dp <- duplicated(xInteger)
    }
  } else {
    ix <- SortRows(x, index.return = TRUE)
    dp <- duplicated(x)
  }
  a <- rep(NA_integer_, length(dp))
  a[ix] <- cumsum(!dp[ix])
  if (!(returnGroups | returnGroupsId)) 
    return(a)
  
  out <- NULL
  out$idx <- a
  
  idg <- ix[!dp[ix]]
  
  if (returnGroups) {
    out$groups <- x[idg, , drop = FALSE]
    row.names(out$groups) <- NULL
  }
  
  if (returnGroupsId) 
    out$idg <- idg
  
  out
}

RowGroups0rows <- function(x, returnGroups = FALSE, returnGroupsId = FALSE) {
  if (!(returnGroups | returnGroupsId)) 
    return(integer(0))
  out <- NULL
  out$idx <- integer(0)
  if (returnGroups) 
    out$groups <- x
  if (returnGroupsId) 
    out$idg <- integer(0)
  out
}


AsFactorInteger <- function(x) {
  
  for (i in seq_len(NCOL(x))) x[, i] <- as.integer(factor(x[, i, drop = TRUE], exclude = NULL)) - 1
  
  
  hyperN <- rev(cumprod(as.numeric(apply(x, MARGIN = c(2), max) + 1)))[1]
  
  a <- hyperN + 1
  b <- hyperN - 1
  
  if (!is.finite(a - b)) 
    return(x)
  
  if (a - b != 2) 
    return(x)
  
  k <- rev(c(1, cumprod(rev(as.numeric(apply(x, MARGIN = c(2), max)) + 1))))[-1]
  
  for (i in seq_len(NCOL(x))) 
    x[, i] <- k[i] * x[, i, drop = TRUE]
  
  rowSums(x)
  
}


