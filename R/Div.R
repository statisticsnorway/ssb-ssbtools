#' Sequence within unique values 
#'
#' @param x vector 
#' @param sortdata matrix or vector to determine sequence order
#'
#' @return integer vector
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # 1:4 within A and 1:2 within B
#' UniqueSeq(c("A", "A", "B", "B", "A", "A"))
#' 
#' # Ordered differently
#' UniqueSeq(c("A", "A", "B", "B", "A", "A"), c(4, 5, 20, 10, 3, 0))
UniqueSeq <- function(x, sortdata = matrix(1L, length(x), 0)) {
  ix <- SortRows(cbind(x, sortdata), index.return = TRUE)
  nr <- seq_len(length(x))
  sortx <- x[ix]
  (1L + nr - nr[match(sortx, sortx)])[order(ix)]
}



#' Round values that are close two whole numbers
#' 
#' @details
#' When \code{digits} is \code{NA}, \code{Inf} or \code{NULL}, input is returned unmodified. 
#' When there is more than one element in \code{digits} or \code{onlyZeros}, 
#' rounding is performed column-wise.     
#' 
#'
#' @param x vector or matrix
#' @param digits parameter to \code{\link{round}}
#' @param onlyZeros Only round values close to zero 
#'
#' @return Modified x
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- c(0.0002, 1.00003, 3.00014)
#' RoundWhole(x)     # No values rounded
#' RoundWhole(x, 4)  # One value rounded
#' RoundWhole(x, 3)  # All values rounded
#' RoundWhole(x, NA) # No values rounded (always)
#' RoundWhole(x, 3, TRUE)  # One value rounded
#' RoundWhole(cbind(x, x, x), digits = c(3, 4, NA))
#' RoundWhole(cbind(x, x), digits = 3, onlyZeros = c(FALSE, TRUE))
RoundWhole <- function(x, digits = 9, onlyZeros = FALSE) {
  if (is.null(digits))
    return(x)
  if (length(digits) > 1 | length(onlyZeros) > 1){
    return(RoundWholeColumns(x, digits, onlyZeros))
  }
  if (!is.finite(digits))
    return(x)
  round_x <- round(x)
  round_x_digits <- round(x, digits = digits)
  if (onlyZeros) 
    toWhole <- round_x_digits == 0 
  else 
    toWhole <- round_x == round_x_digits
  toWhole[is.na(toWhole)] <- FALSE
  x[toWhole] <- round_x[toWhole]
  x
}


RoundWholeColumns <- function(x, digits = 9, onlyZeros = FALSE){
  ncolx  <- ncol(x)
  digits <- rep_len(digits,ncolx)
  onlyZeros <- rep_len(onlyZeros,ncolx)
  for(i in seq_len(ncol(x))){
    x[, i] <- RoundWhole(x[, i], digits[i], onlyZeros[i])
  }
  x
}


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









#' Duplicated columns in dummy matrix  
#' 
#' The algorithm is based on `crossprod(x)` or `crossprod(x, u)` where `u` is a vector of random numbers  
#' 
#' The efficiency of the default algorithm depends on the sparsity of `crossprod(x)`.
#' The random values are generated locally within the function without affecting the random value stream in R.
#'
#' @param x A matrix
#' @param idx Indices returned when TRUE
#' @param rows Duplicated rows instead when TRUE
#' @param rnd Algorithm based on cross product with random numbers when TRUE (dummy matrix not required)  
#'
#' @return Logical vectors specifying duplicated columns or vector of indices (first match)
#' @importFrom stats runif
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- cbind(1, rbind(diag(2), diag(2)), diag(4)[, 1:2])
#' z <- Matrix(x[c(1:4, 2:3), c(1, 2, 1:5, 5)])
#' 
#' DummyDuplicated(z)
#' which(DummyDuplicated(z, rows = TRUE))
#' 
#' # Four ways to obtain the same result
#' DummyDuplicated(z, idx = TRUE)
#' DummyDuplicated(z, idx = TRUE, rnd = TRUE)
#' DummyDuplicated(t(z), idx = TRUE, rows = TRUE)
#' DummyDuplicated(t(z), idx = TRUE, rows = TRUE, rnd = TRUE)
#' 
#' # The unique values in four ways 
#' which(!DummyDuplicated(z), )
#' which(!DummyDuplicated(z, rnd = TRUE))
#' which(!DummyDuplicated(t(z), rows = TRUE))
#' which(!DummyDuplicated(t(z), rows = TRUE, rnd = TRUE))
DummyDuplicated <- function(x, idx = FALSE, rows = FALSE, rnd = FALSE) {
  if (rnd) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(123)
    if (rows) {
      xtu <- as.vector(x %*% runif(ncol(x)))
    } else {
      xtu <- as.vector(crossprod(x, runif(nrow(x))))
    }
    if (idx) {
      return(match(xtu, xtu))
    }
    return(duplicated(xtu))
  }
  if (class(x)[1] == "matrix") {
    x <- Matrix(x)
  }
  if (rows) {
    k <- as(as(triu(tcrossprod(x), 1), "dgeMatrix"), "dgTMatrix")
    colSums_x <- rowSums(x)
    if (idx) {
      o <- seq_len(nrow(x))
    } else {
      o <- rep(FALSE, nrow(x))
    }
  } else {
    k <- as(as(triu(crossprod(x), 1), "dgeMatrix"), "dgTMatrix")
    colSums_x <- colSums(x)
    if (idx) {
      o <- seq_len(ncol(x))
    } else {
      o <- rep(FALSE, ncol(x))
    }
  }
  r <- colSums_x[k@j + 1] == k@x & colSums_x[k@i + 1] == k@x
  if (idx) {
    r <- r[!duplicated(k@j[r])]
    o[k@j[r] + 1] <- o[k@i[r] + 1]
    return(o)
  }
  o[unique(k@j[r] + 1)] <- TRUE
  o
}



