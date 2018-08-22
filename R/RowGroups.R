#' Create numbering according to unique rows
#'
#' @param x Data frame or matrix
#' @param returnGroups   When TRUE unique rows are returned
#' @param returnGroupsId When TRUE Index of unique rows are returned
#'
#' @return A vector with the numbering or, according to the arguments, 
#'         a list with more output.
#' @export
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
  
  for (i in seq_len(NCOL(x))) x[, i] <- as.integer(factor(x[, i], exclude = NULL)) - 1
  
  
  hyperN <- rev(cumprod(as.numeric(apply(x, MARGIN = c(2), max) + 1)))[1]
  
  a <- hyperN + 1
  b <- hyperN - 1
  
  if (a - b != 2) 
    return(x)
  
  k <- rev(c(1, cumprod(rev(as.numeric(apply(x, MARGIN = c(2), max)) + 1))))[-1]
  
  for (i in seq_len(NCOL(x))) x[, i] <- k[i] * x[, i]
  
  rowSums(x)
  
}



# match er mye raskere
MatchYTreg <- function(x, y) {
  a <- factor(x)
  b <- factor(y, levels = levels(a))
  k <- b[!is.na(b)]
  d <- seq_len(length(y))[!is.na(b)]
  z <- rep(0, length(x))
  z[k] <- d
  z
}


#' Matching rows in data frames
#' 
#' The algorithm is based on converting variable combinations to whole numbers. 
#' The final matching is performed using \code{\link{match}}.
#' 
#' When the result of multiplying together the number of unique values in each column of x exceeds 9E15 
#' (largest value stored exactly by the numeric data type), the algorithm is recursive.
#' 
#' @param x data frame
#' @param y data frame
#'
#' @return An integer vector giving the position in y of the first match if there is a match, otherwise NA.
#' @export
#'
#' @examples
#'
#' a <- data.frame(x = c("a", "b", "c"), y = c("A", "B"), z = 1:6)
#' b <- data.frame(x = c("b", "c"), y = c("B", "K", "A", "B"), z = c(2, 3, 5, 6))
#' 
#' Match(a, b)
#' Match(b, a)
#' 
#' # Slower alternative
#' match(data.frame(t(a), stringsAsFactors = FALSE), data.frame(t(b), stringsAsFactors = FALSE))
#' match(data.frame(t(b), stringsAsFactors = FALSE), data.frame(t(a), stringsAsFactors = FALSE))
#'
#' # More comprehensive example (n, m and k may be changed)
#' n <- 10^4
#' m <- 10^3
#' k <- 10^2
#' data(precip)
#' data(mtcars)
#' y <- data.frame(car = sample(rownames(mtcars), n, replace = TRUE), 
#'                 city = sample(names(precip), n, replace = TRUE),
#'                 n = rep_len(1:k, n), a = rep_len(c("A", "B", "C", "D"), n),
#'                 b = rep_len(as.character(rnorm(1000)), n),
#'                 d = sample.int(k + 10, n, replace = TRUE),
#'                 e = paste(sample.int(k * 2, n, replace = TRUE), 
#'                           rep_len(c("Green", "Red", "Blue"), n), sep = "_"),
#'                 r = rnorm(k)^99)
#' x <- y[sample.int(n, m), ]
#' row.names(x) <- NULL
#' ix <- Match(x, y)
Match <- function(x, y) {
  if (NROW(x) == 0) 
    return(integer(0))
  
  if (NROW(y) == 0) 
    return(rep(as.integer(NA), NROW(x)))
  
  if (any(!(names(x) == names(y)))) 
    y <- y[, names(x), drop = FALSE]
  
  for (i in seq_len(NCOL(x))) x[, i] <- factor(x[, i], exclude = NULL)
  
  for (i in seq_len(NCOL(x))) y[, i] <- factor(y[, i], levels = levels(x[, i]), exclude = NULL)
  
  
  for (i in seq_len(NCOL(x))) x[, i] <- as.integer(x[, i]) - 1
  
  for (i in seq_len(NCOL(x))) y[, i] <- as.integer(y[, i]) - 1
  
  
  
  asn <- as.numeric(apply(x, MARGIN = c(2), max)) + 1
  
  
  mg <- rep(1, length(asn))
  
  
  h <- rev(c(1, cumprod(rev(asn))))
  a <- h + 1
  b <- h - 1
  amb <- a - b
  while (any(amb != 2)) {
    ind <- seq_len(min(which(amb == 2)) - 1)
    mg[ind] <- mg[ind] + 1
    indh <- seq_len(min(which(amb == 2)))
    asn <- asn[ind]
    h[indh] <- rev(c(1, cumprod(rev(asn))))
    a <- h + 1
    b <- h - 1
    amb <- a - b
  }
  k <- h[-1]
  
  
  for (i in seq_len(NCOL(x))) x[, i] <- k[i] * x[, i]
  for (i in seq_len(NCOL(x))) y[, i] <- k[i] * y[, i]
  
  if (max(mg) == 1) {
    x <- rowSums(x)
    y <- rowSums(y)
    return(match(x, y))
  }
  
  ng <- max(mg)
  
  if (ng == NCOL(x)) {
    warning(paste("The problem cannot be handled as usual.", ng, "integer coded columns are pasted together."))
    return(match(do.call("paste", c(x, sep = "_")), do.call("paste", c(y, sep = "_"))))
  }
  
  X <- as.data.frame(matrix(0, NROW(x), ng))
  Y <- as.data.frame(matrix(0, NROW(y), ng))
  for (i in seq_len(ng)) {
    X[, i] <- rowSums(x[, mg == i, drop = FALSE])
    Y[, i] <- rowSums(y[, mg == i, drop = FALSE])
  }
  rm(x)
  rm(y)
  Match(X, Y)
}







#' Title UniqueIntegerSortMatrix
#'
#' @param x x
#'
#' @return Matrix
#' @importFrom Matrix Matrix
UniqueIntegerSortMatrix <- function(x) {
  if (!is.integer(x)) 
    stop("x must be integer")
  m <- Matrix(0, max(x), 1)
  m[x, 1] <- x
  m[m > 0]
}



# Raskere enn sort, men bruker mer minne
UniqueIntegerSort <- function(x) {
  if (!is.integer(x) | min(x) <= 0) 
    stop("x must be integer")
  m <- rep(0, max(x))
  m[x] <- x
  m[m > 0]
}



