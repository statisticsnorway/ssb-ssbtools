#' Create numbering according to unique rows
#'
#' @param x Data frame or matrix
#' @param returnGroups   When TRUE unique rows are returned
#' @param returnGroupsId When TRUE Index of unique rows are returned
#' @param NAomit When `TRUE`, rows containing NAs are omitted, and the corresponding index numbers are set to `NA`. 
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
RowGroups <- function(x, returnGroups = FALSE, returnGroupsId = FALSE, NAomit = FALSE) {
  
  if (NROW(x) == 0) 
    return(RowGroups0rows(x = x, returnGroups = returnGroups, returnGroupsId = returnGroupsId))
  
  if (NAomit) {
    return(RowGroupsNAomit(x = x, returnGroups = returnGroups, returnGroupsId = returnGroupsId, NAomit = FALSE))
  }
  
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



RowGroupsNAomit <- function(x, ...) {
  isNa <- rep(FALSE, NROW(x))
  for (i in seq_len(NCOL(x))) {
    isNa <- isNa | is.na(x[, i, drop = TRUE])
  }
  
  if (!any(isNa)) {
    return(RowGroups(x, ...))
  }
  
  rg1 <- rep(NA_integer_, NROW(x))
  
  rg <- RowGroups(x[!isNa, , drop = FALSE], ...)
  
  if (is.list(rg)) {
    rg1[!isNa] <- rg[[1]]
    rg[[1]] <- rg1
  } else {
    rg1[!isNa] <- rg
    rg <- rg1
  }
  rg
}

