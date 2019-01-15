#' Creating variables by splitting the elements of a character vector
#' without needing a split string
#'
#' @param s The character vector
#' @param split Split string. When NULL (default), automatic splitting without a split string.
#' @param border A split character 
#'        or an integer (move split) to be used when the exact split position 
#'        is not unique. 
#' @param revBorder When border is integer the split position is moved from the other side.
#' @param noSplit No splitting when TRUE.
#' @param varNames Variable names of the created variables (too many is ok)
#' @param tryReverse When TRUE, the automatic method tries to find more variables by 
#'       splitting from reversed strings.
#'
#' @return A data frame with s as row names. 
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' s <- c("A12-3-A-x","A12-3-B-x","B12-3-A-x","B12-3-B-x",
#'        "A12-3-A-y","A12-3-B-y","B12-3-A-y","B12-3-B-y")
#' AutoSplit(s)
#' AutoSplit(s,border="-")
#' AutoSplit(s,split="-")
#' AutoSplit(s,border=1)
#' AutoSplit(s,border=2)
#' AutoSplit(s,border=2,revBorder=TRUE)
#' AutoSplit(s,noSplit=TRUE)
#' AutoSplit(s,varNames=c("A","B","C","D"))
AutoSplit <- function(s, split = NULL, border = "_", revBorder = FALSE, noSplit = FALSE, 
                      varNames = paste("var", 1:100, sep = ""), tryReverse = TRUE) {
  if (!noSplit & !is.null(split)) {
    x <- strsplit(s, split = split)
    if (length(unique(sapply(x, length))) != 1) 
      stop("Unequal number of elements when splitting")
    x <- data.frame(t(as.matrix(data.frame(x))), row.names = s)
    names(x) <- varNames[seq_len(length(x))]
    return(x)
  }
  if (!noSplit) {
    x <- AutoSplit2(s = s, tryReverse = tryReverse, border = border, revBorder = revBorder)
  } else x <- list(s)
  names(x) <- varNames[seq_len(length(x))]
  x
  data.frame(x, stringsAsFactors = FALSE, row.names = s)
}


matlabColon <- function (from, to) # Stolen from own package ffmanova
{
  if (from > to)
    numeric(0)
  else from:to
}

MatchFirst <- function(s) {
  n <- length(s)
  m <- max(nchar(s))
  j <- 0
  ok <- TRUE
  while (ok) {
    j <- j + 1
    if (j > m) 
      nMatch <- 0 else nMatch <- sum(substring(s, 1, j) %in% substring(s[1], 1, j))
      ok <- (nMatch == n)
  }
  j - 1
}


MatchMatrix <- function(s) {
  n <- length(s)
  m <- max(nchar(s))
  nMatch <- matrix(1, n, m)
  for (i in 1:n) {
    nMatchij <- 1e+05
    j <- 1
    while (nMatchij > 1) {
      nMatchij <- sum(substring(s, 1, j) %in% substring(s[i], 1, j))
      nMatch[i, j] <- nMatchij
      j <- j + 1
    }
  }
  nMatch
}

Ncommon <- function(nMatch, useMax = FALSE) {
  x <- Reduce(intersect, as.list(data.frame(t(nMatch))))
  if (useMax) {
    n <- max(c(x[x < dim(nMatch)[1]], 1))
    if (n == 1) 
      n <- 0
    return(n)
  }
  n <- min(c(x[x > 1], 1e+05))
  if (n == dim(nMatch)[1]) 
    n <- 0
  if (n == 1e+05) 
    n <- 0
  n
}

Nwhere <- function(nMatch, n) {
  apply(col(nMatch) * (nMatch == n), 1, max)
}


AutoSplit0 <- function(s, border, revBorder) {
  nMatch <- MatchMatrix(s)
  nCommon <- Ncommon(nMatch)
  if (nCommon == 0) 
    return(NULL)
  nWhere <- Nwhere(nMatch, nCommon)
  s1 <- s
  s2 <- s
  # if(moveEnd){
  for (i in 1:length(s)) s1[i] <- reverse_chars(substring(s[i], 1, nWhere[i]))
  # nWhere = nWhere-MatchFirst(s1) }
  matchFirst <- MatchFirst(s1)
  if (matchFirst == 0) {
    for (i in 1:length(s)) {
      s1[i] <- substring(s[i], 1, nWhere[i])
      s2[i] <- substring(s[i], nWhere[i] + 1)
    }
    return(list(s1 = s1, s2 = s2))
  }
  
  a <- matchFirst
  b <- matchFirst + 1
  if (is.character(border)) 
    if (nchar(border)) {
      bord <- reverse_chars(substring(s1[1], 1, matchFirst))
      # print(bord)
      if (revBorder) 
        bord <- reverse_chars(bord)
      k <- regexpr(border, bord)
      if (k > 0) {
        a <- k - 1
        b <- k + 1
      }
    }
  if (is.numeric(border)) {
    a <- border[1]
    if (length(border) > 1) 
      b <- a + 1 + border[2] else b <- a + 1
  }
  
  if (revBorder) {
    bb <- b
    b <- matchFirst + 1 - a
    a <- matchFirst + 1 - bb
  }
  a <- max(0, a)
  b <- max(1, b)
  a <- min(matchFirst, a)
  b <- min(matchFirst + 1, b)
  
  for (i in 1:length(s)) {
    s1[i] <- substring(s[i], 1, nWhere[i] - matchFirst + a)
    s2[i] <- substring(s[i], nWhere[i] - matchFirst + b)
  }
  list(s1 = s1, s2 = s2)
}

AutoSplit1 <- function(s, border, revBorder) {
  # border er tall (antall) eller en split-character som tast bort
  sFac <- as.factor(as.character(s))
  sLev <- levels(sFac)
  sInt <- as.integer(sFac)  # now s =  sLev[sInt]
  as <- AutoSplit0(sLev, border, revBorder)
  if (is.null(as)) 
    return(NULL)
  list(s1 = as$s1[sInt], s2 = as$s2[sInt])
}

AutoSplit2 <- function(s, tryReverse = TRUE, border = "_", revBorder = FALSE) {
  if (tryReverse) 
    sReverse <- ReverseChars(s)
  s <- list(s)
  a <- TRUE
  while (any(a)) {
    k <- match(TRUE, a)
    n <- length(a)
    as <- AutoSplit1(s[[k]], border, revBorder)
    if (is.null(as)) 
      a[k] <- FALSE else {
        a <- c(a[matlabColon(1, k - 1)], TRUE, TRUE, a[matlabColon(k + 1, n)])
        s <- c(s[matlabColon(1, k - 1)], as, s[matlabColon(k + 1, n)])
      }
  }
  names(s) <- NULL
  if (!tryReverse) 
    return(s)
  sReverse <- AutoSplit2(sReverse, FALSE, border, !revBorder)
  sReverse <- rev(lapply(sReverse, ReverseChars))
  if (length(sReverse) > length(s)) 
    return(sReverse)
  s
}


# Taken from Handling and Processing Strings in R reversing a string by characters
reverse_chars <- function(string) {
  string_split <- strsplit(as.character(string), split = "")
  reversed_split <- string_split[[1]][rev(matlabColon(1, nchar(string)))]  #[nchar(string):1] matlabColon
  paste(reversed_split, collapse = "")
}


ReverseChars <- function(s) {
  x <- sapply(s, reverse_chars)
  names(x) <- NULL
  x
}


