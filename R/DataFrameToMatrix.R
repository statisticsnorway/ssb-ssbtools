

#' DataFrameToMatrix
#'
#' @param x data frame
#'
#' @return matrix
#' @export
#' @author Øyvind Langsrud
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
#' @author Øyvind Langsrud
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







