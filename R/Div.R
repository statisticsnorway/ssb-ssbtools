

#' ForceFactorDataFrame
#'
#' @param x data frame
#'
#' @return data frame
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
ForceFactorDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) if (is.character(x[, i, drop =TRUE])) 
    x[, i] <- as.factor(x[, i, drop =TRUE])
  x
}


#' CharacterDataFrame
#'
#' @param x data frame
#'
#' @return data frame
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
CharacterDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) 
    x[, i] <- as.character(x[, i, drop = TRUE])
  x
}

#' ForceCharacterDataFrame
#'
#' @param x data frame
#'
#' @return data frame
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
ForceCharacterDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) if (is.factor(x[, i, drop =TRUE])) 
    x[, i] <- as.character(x[, i, drop =TRUE])
  x
}


CrossCodeFramesA <- function(codeFrame1, codeFrame2) {
  n1 <- NROW(codeFrame1)
  n2 <- NROW(codeFrame2)
  # r1 = rep(seq_len(n1), times = n2, each = 1) # kan droppes
  r2 <- rep(seq_len(n2), times = 1, each = n1)
  
  rownames(codeFrame1) <- NULL
  # cbind(codeFrame1[r1, , drop = FALSE], codeFrame2[r2, , drop = FALSE])
  cbind(codeFrame1, codeFrame2[r2, , drop = FALSE])
}


#' Cross codes in data frames
#'
#' @param codeFrame1 data frame
#' @param codeFrame2 data frame
#' @param useMatrixToDataFrame useMatrixToDataFrame
#'
#' @return data frame
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
CrossCodeFrames <- function(codeFrame1, codeFrame2, useMatrixToDataFrame = TRUE) {
  if (!useMatrixToDataFrame) 
    return(CrossCodeFramesA(codeFrame1, codeFrame2))
  # cat('*')
  n1 <- NROW(codeFrame1)
  n2 <- NROW(codeFrame2)
  
  codeFrame1 <- DataFrameToMatrix(codeFrame1)
  codeFrame2 <- DataFrameToMatrix(codeFrame2)
  
  rownames(codeFrame1) <- NULL
  rownames(codeFrame2) <- NULL
  
  
  r1 <- rep(seq_len(n1), times = n2, each = 1)
  r2 <- rep(seq_len(n2), times = 1, each = n1)
  
  
  z <- cbind(codeFrame1[r1, , drop = FALSE], codeFrame2[r2, , drop = FALSE])
  
  attr(z, "namesDF") <- c(attr(codeFrame1, "namesDF"), attr(codeFrame2, "namesDF"))
  attr(z, "classDF") <- c(attr(codeFrame1, "classDF"), attr(codeFrame2, "classDF"))
  attr(z, "levelsDF") <- c(attr(codeFrame1, "levelsDF"), attr(codeFrame2, "levelsDF"))
  
  MatrixToDataFrame(z)
}




# Kostra:::HeadEnd
# HeadEnd(1:1000) '1' '2' '3' '4' '...'  '1000'
HeadEnd <- function(x, n = 4L) {
  x <- as.character(x)
  if (length(x) > (n + 2))
    x <- c(head(x, n = n), "...", tail(x, n = 1))
  x
}


