
#' Finding dimList
#'
#' Finding lists of level-hierarchy as needed for the input parameter
#' dimList to the function makeProblem in package sdcTable
#'
#' @encoding UTF8
#'
#' @param x Matrix or data frame containing the variables (micro data or cell counts data).
#' @param groupVarInd List of vectors of indices defining the hierarchical variable groups.
#' @param addName When TRUE the variable name is added to the level names, except for variables with most levels.
#' @param sep A character string to separate when addName apply.
#' @param xReturn When TRUE x is also in output, possibly changed according to addName.
#' @param total String used to name totals. Can also be a vector of length `ncol(x)` 
#'              or a named vector/list. If named, the dimension names used in
#'              the output must be present among the names in `total`.
#'
#' @return Output is a list according to the specifications in sdcTable.
#'         When xReturn is TRUE output has an extra list level and x is the first element.
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' dataset <- SSBtoolsData("example1")
#' FindDimLists(dataset[1:2])
#' FindDimLists(dataset[2:3])
#' FindDimLists(dataset[1:4])
#' 
#' FindDimLists(SSBtoolsData("magnitude1")[1:4], 
#'                 total = c("TOTAL", "unused1", "Europe", "unused2"))
#'                 
#'  x <- rep(c('A','B','C'),3)
#'  y <- rep(c(11,22,11),3)
#'  z <- c(1,1,1,2,2,2,3,3,3)
#'  zy <- paste(z,y,sep='')
#'  m <- cbind(x,y,z,zy)
#'  FindDimLists(m)
#'  FindDimLists(m, total = paste0("A", 1:4))
FindDimLists <- function(x, groupVarInd = HierarchicalGroups(x = x), addName = FALSE, 
                         sep = ".", xReturn = FALSE, total = "Total") {
  
  # Generalization to `length(total)>1` could also have been implemented by changing 
  # CheckLevels, DimFromHier and DimFromHier1. Making the change here is easier and safer.
  
  total <- unlist(total)
  
  if (length(total) > 1) {
    if (is.null(names(total))) {
      if (length(total) != ncol(x)) {
        stop("wrong length of total")
      }
    }
    dimLists <- FindDimLists(x = x, groupVarInd = groupVarInd, addName = addName, sep = sep, xReturn = xReturn, total = "t_O2T_aL83")
    if (is.null(names(total))) {
      tot <- total[match(names(dimLists), colnames(x))]
    } else {
      ma <- match(names(dimLists), names(total))
      if (anyNA(ma)) {
        stop(paste("Missing total name for", paste(names(dimLists)[is.na(ma)], collapse = ", "))) 
      }
      tot <- total[ma]
    }
    
    for (i in seq_along(dimLists)) {
      if (tot[i] %in% dimLists[[i]][, 2, drop = TRUE]) {
        stop(paste0('"',tot[i], '"', " cannot be total code for ", "'", names(dimLists)[i], "'",  " since already a level name"))
      }
      dimLists[[i]][1, 2] <- tot[i]
    }
    return(dimLists)
  }
  
  hierGr <- GroupNrList(groupVarInd)
  CheckOk <- TRUE
  if (!addName) 
    for (i in seq_len(length(hierGr))) if (!CheckLevels(x, hierGr[[i]], CheckLevelsHandling = warning)) 
      CheckOk <- FALSE
  if (!CheckOk) {
    warning("Settting addName to TRUE (overriding input)")
    addName <- TRUE
  }
  if (addName) {
    addVar <- NULL
    for (i in matlabColon(1, length(hierGr))) addVar <- c(addVar, hierGr[[i]][matlabColon(2, 
                                                                                          length(hierGr[[i]]))])
    addVar <- unique(addVar)
    for (i in addVar) x[, i] <- paste(colnames(x)[i], x[, i], sep = sep)
  }
  if (addName) 
    for (i in seq_len(length(hierGr))) CheckLevels(x, hierGr[[i]], CheckLevelsHandling = stop)
  for (i in seq_len(length(groupVarInd))) CheckLevels(x, groupVarInd[[i]], CheckLevelsHandling = stop, 
                                                      checkDecreasing = TRUE, total = total)
  dimLists <- DimFromHier(x, groupVarInd, addName = FALSE, total = total)  # addName already done
  if (!xReturn) 
    return(dimLists)
  for (i in seq_len(NCOL(x))) x[, i] <- as.character(x[, i])
  list(x = x, dimLists = dimLists)
}

CheckLevels <- function(data, dimVarInd = 1:NCOL(data), CheckLevelsHandling = warning, 
                        checkDecreasing = FALSE, total = NULL) {
  x <- NULL
  oldlength <- Inf
  for (i in dimVarInd) {
    iunique <- unique(as.character(data[, i]))
    ilength <- length(iunique)
    if (checkDecreasing) 
      if (ilength > oldlength) 
        stop("Number of levels not decreasing")
    oldlength <- ilength
    x <- c(x, iunique)
  }
  if (!is.null(total)) {
    if (total %in% x) 
      CheckLevelsHandling(paste(total, "is used as a level name ...", paste(colnames(data)[dimVarInd], 
                                                                            collapse = ", ")))
  }
  if (length(x) == length(unique(x))) 
    return(TRUE)
  CheckLevelsHandling(paste("Levelnames must be different in", paste(colnames(data)[dimVarInd], 
                                                                     collapse = ", ")))
  return(FALSE)
}






DimFromHier <- function(x, hier, addName = FALSE, total = "Total") {
  for (i in matlabColon(1, length(hier))) hier[[i]] <- DimFromHier1(x, hier[[i]], 
                                                                    addName = addName, total = total)
  hier
}

# addName here use '.' as separator
DimFromHier1 <- function(x, indHier = 1:dim(x)[2], addName = FALSE, total = "Total") {
  start <- "@@"
  add <- "@"
  r1 <- data.frame(levels = "@", codes = total, stringsAsFactors = FALSE)
  
  b <- CrossLevels(x[, rev(indHier), drop = FALSE])
  
  m <- NCOL(b)
  n <- NROW(b)
  symbol <- start
  for (i in matlabColon(2, m)) symbol <- c(symbol, paste(symbol[i - 1], add, sep = ""))
  
  symbols <- rep(" ", m * n)
  codes <- rep(" ", m * n)
  k <- 0
  bb <- b[1, , drop = FALSE]
  for (i in matlabColon(1, n)) for (j in matlabColon(1, m)) {
    newrow <- FALSE
    if (i == 1) 
      newrow <- TRUE else if (bb[1, j] != b[i, j]) 
        newrow <- TRUE
      if (newrow) {
        k <- k + 1
        bb[1, j] <- b[i, j]
        symbols[k] <- symbol[j]
        if (addName) 
          codes[k] <- paste(colnames(b)[j], as.character(b[i, j]), sep = ".") else codes[k] <- as.character(b[i, j])
      }
      
  }
  rbind(r1, data.frame(levels = symbols[matlabColon(1, k)], codes = codes[matlabColon(1, k)], stringsAsFactors = FALSE))
}



# Setter sammen grupper hvis de har noe felles
GroupNrList <- function(x) {
  n <- length(x)
  z <- vector("list", n)
  z[[1]] <- x[[1]]
  k <- 1
  for (i in matlabColon(2, n)) {
    a <- x[[i]]
    jj <- 0
    for (j in seq_len(k)) {
      if (any(x[[i]] %in% z[[j]])) 
        jj <- j
    }
    if (jj == 0) {
      k <- k + 1
      z[[k]] <- x[[i]]
    } else {
      z[[jj]] <- unique(c(z[[jj]], x[[i]]))
    }
  }
  z[seq_len(k)]
}

CrossLevels <- function(x) {
  SortRows(unique(x, MARGIN = 1))
}





