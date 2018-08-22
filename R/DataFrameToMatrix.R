

#' DataFrameToMatrix
#'
#' @param x data frame
#'
#' @return matrix
#' @export
#' @keywords internal
#'
DataFrameToMatrix <- function(x) {
  levelsDF <- vector("list", NCOL(x))
  namesDF <- names(x)
  classDF <- sapply(x, class)
  characterDF <- sapply(x, is.character)
  
  if (any(!(classDF %in% c("character", "factor", "integer", "logical", "numeric")))) 
    warning(paste("Classes not treated. MatrixToDataFrame() will not work:", paste(unique(classDF[!(classDF %in% c("character", "factor", "integer", "logical", "numeric"))]), collapse = ", ")))
  
  for (i in seq_len(NCOL(x))) {
    if (characterDF[i]) 
      x[, i] <- as.factor(x[, i])
    if (is.factor(x[, i])) {
      levelsDF[[i]] <- levels(x[, i])
      x[, i] <- as.integer(x[, i])
    }
  }
  x <- as.matrix(x)
  attr(x, "namesDF") <- namesDF
  attr(x, "classDF") <- classDF
  attr(x, "levelsDF") <- levelsDF
  x
}

#' MatrixToDataFrame
#'
#' @param x matrix
#' @param forceStringsAsFactors forceStringsAsFactors
#'
#' @return data frame
#' @export
#' @keywords internal
#'
MatrixToDataFrame <- function(x, forceStringsAsFactors = FALSE) {
  
  namesDF <- attr(x, "namesDF")
  classDF <- attr(x, "classDF")
  levelsDF <- attr(x, "levelsDF")
  
  
  
  if (any(!(classDF %in% c("character", "factor", "integer", "logical", "numeric")))) 
    stop(paste("Classes not treated:", paste(unique(classDF[!(classDF %in% c("character", "factor", "integer", "logical", "numeric"))]), collapse = ", ")))
  
  
  x <- as.data.frame(x)
  names(x) <- namesDF
  
  for (i in seq_len(NCOL(x))) {
    
    if (!classDF[i] == "numeric") 
      x[, i] <- as.integer(x[, i])
    
    
    if (classDF[i] == "logical") 
      x[, i] <- as.logical(x[, i])
    
    if (classDF[i] == "character" | classDF[i] == "factor") {
      attr(x[, i], "levels") <- levelsDF[[i]]
      class(x[, i]) <- "factor"
      if (classDF[i] == "character" & !forceStringsAsFactors) 
        x[, i] <- as.character(x[, i])
    }
  }
  x
  
}


#' ForceFactorDataFrame
#'
#' @param x data frame
#'
#' @return data frame
#' @export
#' @keywords internal
#'
ForceFactorDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) if (is.character(x[, i])) 
    x[, i] <- as.factor(x[, i])
  x
}


#' CharacterDataFrame
#'
#' @param x data frame
#'
#' @return data frame
#' @export
#' @keywords internal
#'
CharacterDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) x[, i] <- as.character(x[, i])
  x
}

#' ForceCharacterDataFrame
#'
#' @param x data frame
#'
#' @return data frame
#' @export
#' @keywords internal
#'
ForceCharacterDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) if (is.factor(x[, i])) 
    x[, i] <- as.character(x[, i])
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
#' @keywords internal
#'
#' @examples
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













