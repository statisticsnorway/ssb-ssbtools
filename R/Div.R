


#' Microdata frequency data conversion
#'
#' @param x A data frame
#' @param freqName Name of new frequency variable in output data
#' @param all Whether to include level combinations not in microdata (zero frequency) 
#' @param hierarchies Whether to treat hierarchical variables automatically when  `all=TRUE` 
#'
#' @return A data frame
#' @keywords internal
#' @export
#'
#' @examples
#' z <- SSBtoolsData("sprt_emp")[c(1, 2, 4, 7:12, 15, 17, 18), -4]
#' z$eu <- z$geo != "Iceland"
#' z
#' 
#' MakeFreq(z)
#' MakeFreq(z[, -2])
#' MakeFreq(z[, -(2:3)])
#' MakeFreq(z[, -1])
#' MakeFreq(z[, -1], all = TRUE)
#' 
#' x <- MakeFreq(z[, -1], all = TRUE, hierarchies = TRUE)
#' x
#' 
#' MakeMicro(x, "freq")
MakeFreq <- function(x, freqName = "freq", all = FALSE, hierarchies = FALSE) {
  if (all) {
    return(MakeFreqAll(data = x, freqName = freqName, hierarchies = hierarchies))
  }
  z <- SortRows(x)
  uz <- !duplicated(z)
  fr <- matrix(-diff(c((NROW(x):1)[uz], 0)), ncol = 1, dimnames = list(NULL, freqName))
  b <- cbind(z[uz, , drop = FALSE], fr)
  row.names(b) <- NULL
  b
}

#' @rdname MakeFreq
#' @param freqVar The frequency variable in input data, name or number.
#' @keywords internal
#' @export
MakeMicro <- function(x, freqVar) {
  rows <- rep(seq_len(NROW(x)), x[, freqVar])
  x <- x[rows, , drop = FALSE]
  x[, freqVar] <- 1
  row.names(x) <- NULL
  x
}


MakeFreqAll <- function(data, freqName = "freq", hierarchies = TRUE) {
  x <- MakeFreq(data, freqName)
  freQ <- data.frame(0L)
  names(freQ) <- freqName
  z <- cbind(CrossAll(data, hierarchies), freQ)
  ma <- Match(x[, -(NCOL(data) + 1)], z[, -(NCOL(data) + 1)])
  z[ma, freqName] <- x[, freqName]
  z
}




CrossAll <- function(data, hierarchies = TRUE) {
  if (NCOL(data) == 1) 
    return(unique(data))
  if (hierarchies) 
    hg <- HierarchicalGroups3(data) else {
      hg <- as.list(seq_len(NCOL(data)))
      names(hg) <- colnames(data)
    }
  z <- unique(data[, hg[[1]], drop = FALSE])
  for (i in SeqInc(2, length(hg))) {
    x <- unique(data[, hg[[i]], drop = FALSE])
    z <- CrossCodeFrames(z, x)
  }
  z
}


# Kostra:::HeadEnd
# HeadEnd(1:1000) '1' '2' '3' '4' '...'  '1000'
HeadEnd <- function(x, n = 4L) {
  x <- as.character(x)
  if (length(x) > (n + 2))
    x <- c(head(x, n = n), "...", tail(x, n = 1))
  x
}


