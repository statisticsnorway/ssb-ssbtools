#' Combine several data frames by using id variables to match rows
#'
#' @param ... Several data frames as several input parameters or a list of data frames
#' @param addName NULL or vector of strings used to name columns according to origin frame
#' @param sep A character string to separate when addName apply
#' @param idNames Names of a id variable within each data frame
#' @param idNames1 Names of variables in first data frame that correspond to the id variable within each data frame
#' @param addLast When TRUE addName will be at end
#' 
#' @details The first data frame is the basis and the other frames will be matched by using id-variables. 
#'         The default id-variables are the first variable in each frame. Corresponding variables with 
#'         the same name in first frame is assumed. An id-variable is not needed if the number of rows 
#'         is one or the same as the first frame. Then the element of idNames can be set to a string 
#'         with zero length. 
#'
#' @return A single data frame
#' @export
#' @author Øyvind Langsrud
#' 
#' @seealso \code{\link{RbindAll}} (same example data)
#'
#' @examples
#' zA <- data.frame(idA = 1:10, idB = rep(10 * (1:5), 2), idC = rep(c(100, 200), 5), 
#'                  idC2 = c(100, rep(200, 9)), idC3 = rep(100, 10), 
#'                  idD = 99, x = round(rnorm(10), 3), xA = round(runif(10), 2))
#' zB <- data.frame(idB = 10 * (1:5), x = round(rnorm(5), 3), xB = round(runif(5), 2))
#' zC <- data.frame(idC = c(100, 200), x = round(rnorm(2), 3), xC = round(runif(2), 2))
#' zD <- data.frame(idD = 99, x = round(rnorm(1), 3), xD = round(runif(1), 2))
#' CbindIdMatch(zA, zB, zC, zD)
#' CbindIdMatch(a = zA, b = zB, c = zC, d = zD, idNames = c("", "idB", "idC", ""))
#' CbindIdMatch(a = zA, b = zB, c = zC, d = zD, idNames1 = c("", "idB", "idC2", ""))
#' CbindIdMatch(a = zA, b = zB, c = zC, d = zD, idNames1 = c("", "idB", "idC3", ""))
#' CbindIdMatch(zA, zB, zC, zD, addName = c("", "bbb", "ccc", "ddd"), sep = ".", addLast = TRUE)
#' try(CbindIdMatch(X = zA, Y = zA[, 4:5], Z = zC, idNames = NULL)) # Error
#' CbindIdMatch(X = zA, Y = zA[, 4:5], Z = zD, idNames = NULL)      # Ok since equal NROW or NROW==1
#' CbindIdMatch(list(a = zA, b = zB, c = zC, d = zD))               # List is alternative input
CbindIdMatch <-  function(..., addName=names(x), sep="_",
                   idNames = sapply(x,function(x)names(x)[1]),
                   idNames1 = idNames, addLast=FALSE){
  x <- list(...)
  if (length(x)==1) # Handle list input
    if (is.list(x[[1]]))
      if (!is.data.frame(x[[1]]))
        x <- x[[1]]
      
  n <- length(x)
  m <- NROW(x[[1]])
  if (is.null(idNames)) idNames <- rep("", n)
  if (is.null(idNames1)) idNames1 <- rep("", n)
  addName[addName == ""] <- NA
  idNames[idNames == ""] <- NA
  idNames1[idNames1 == ""] <- NA
  rows <- matrix(seq_len(m), m, n)
  cols <- rep("", n)  # cols will be used to remove id-variables to avoid equal variables 
  for (i in matlabColon(2, n)) {
    if (!is.na(idNames[i]) & !is.na(idNames1[i])) {
      rows[, i] <- match(x[[1]][, idNames1[i]], x[[i]][, idNames[i]])
      cols[i] <- paste("-", match(idNames[i], colnames(x[[i]])))
      if (length(unique(rows[, i])) != NROW(x[[i]])) 
        warning("All rows not used")
    } else {
      if (NROW(x[[i]]) == 1) 
        rows[, i] <- 1 else if (NROW(x[[i]]) != m) 
          stop("Element of idNames/idNames1 missing when needed")
    }
  }
  for (i in seq_len(n)) {
    if (!is.na(addName[i])) 
      if (addLast) 
        colnames(x[[i]]) <- paste(colnames(x[[i]]), addName[i], sep = sep) 
      else 
        colnames(x[[i]]) <- paste(addName[i], colnames(x[[i]]), sep = sep)
  }
  x <- eval(parse(text = paste("cbind(", paste("x[[", seq_len(n), "]][rows[,", seq_len(n), "],", 
            cols[seq_len(n)], ",drop=FALSE],", collapse = ""), "deparse.level = 0)")))
  if (length(unique(colnames(x))) != NCOL(x)) warning("Column names not unique")
  x   
}   
      

#' Combining several data frames when the columns don't match
#'
#' @param ... Several data frames as several input parameters or a list of data frames
#'
#' @return A single data frame
#' @export
#' @author Øyvind Langsrud
#' 
#' @note The function is an extended version of rbind.all.columns at 
#'       \url{https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/}
#' 
#' @seealso \code{\link{CbindIdMatch}} (same example data)
#'
#' @examples
#' zA <- data.frame(idA = 1:10, idB = rep(10 * (1:5), 2), idC = rep(c(100, 200), 5), 
#'                  idC2 = c(100, rep(200, 9)), idC3 = rep(100, 10), 
#'                  idD = 99, x = round(rnorm(10), 3), xA = round(runif(10), 2))
#' zB <- data.frame(idB = 10 * (1:5), x = round(rnorm(5), 3), xB = round(runif(5), 2))
#' zC <- data.frame(idC = c(100, 200), x = round(rnorm(2), 3), xC = round(runif(2), 2))
#' zD <- data.frame(idD = 99, x = round(rnorm(1), 3), xD = round(runif(1), 2))
#' RbindAll(zA, zB, zC, zD)
#' RbindAll(list(zA, zB, zC, zD))
RbindAll <-  function(...){
  x = list(...)
  if (length(x)==1) # Handle list input
    if (is.list(x[[1]]))
      if (!is.data.frame(x[[1]]))
        x <- x[[1]]
  n <- length(x)
  allColnames <- NULL
  for (i in seq_len(n)) 
    allColnames <- unique(c(allColnames, colnames(x[[i]])))
  nrow0 <- sapply(x, nrow) == 0
  if (any(nrow0)) {
    if(!any(!nrow0)) {
      return(as.data.frame(
        matrix(ncol = length(allColnames), 
               nrow = 1, 
               dimnames = list(NULL, allColnames)))[0, , drop = FALSE])
    }
    x <- x[!nrow0]
    n <- length(x)
  }
  for (i in seq_len(n)) 
    x[[i]][, c(as.character(setdiff(allColnames, colnames(x[[i]]))))] <- NA
  eval(parse(text = paste("rbind(", paste("x[[", seq_len(n), "]],", collapse = ""), "deparse.level = 0)")))
}  