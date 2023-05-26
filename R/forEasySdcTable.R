

#' Finding hierarchical variable groups
#'
#' According to the (factor) levels of the variables
#'
#' @encoding UTF8
#'
#' @param x Matrix or data frame containing the variables
#' @param mainName When TRUE output list is named according to first variable in group.
#' @param eachName When TRUE variable names in output instead of indices.
#' @param fCorr When non-null x is not needed as input.
#'
#' @return Output is a list containing the groups. First variable has most levels.
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#'  x <- rep(c("A","B","C"),3)
#'  y <- rep(c(11,22,11),3)
#'  z <- c(1,1,1,2,2,2,3,3,3)
#'  zy <- paste(z,y,sep="")
#'  m <- cbind(x,y,z,zy)
#'  HierarchicalGroups(m)
HierarchicalGroups <- function(x = NULL, mainName = TRUE, eachName = FALSE, fCorr = FactorLevCorr(x)) {
  nLevels <- diag(fCorr)
  if (min(nLevels) <= 1) 
    stop("Number of levels < 2 in a variable")
  ix <- order(nLevels, decreasing = TRUE)
  # print(fCorr[ix,ix])
  if (length(fCorr) > 1) 
    z <- functionRecursive(fCorr[ix, ix], 1:NCOL(fCorr))$l else z <- list(1)
  z1 <- rep(NA, length(z))
  for (i in 1:length(z)) {
    z[[i]] <- ix[z[[i]]]
    z1[i] <- z[[i]][1]
    if (mainName) 
      names(z)[i] <- colnames(fCorr)[z1[i]]
  }
  z <- SortNrList(z)
  for (i in 1:length(z)) {
    if (length(unique(nLevels[z[[i]]])) != length(z[[i]])) 
      warning("There are identical variables")
  }
  if (eachName) {
    for (i in 1:length(z)) z[[i]] <- colnames(fCorr)[z[[i]]]
  }
  z
}


# Recursive algorithm ...
functionRecursive <- function(fCorr, ind) {
  drop <- numeric(0)
  x <- vector("list", 0)
  for (i in ind) if (!(i %in% drop)) {
    z <- functionRecursive(fCorr, (1:NCOL(fCorr))[fCorr[i, ] == -1])
    drop <- c(drop, i, z$drop)
    l <- z$l
    for (k in matlabColon(1, length(l))) l[[k]] <- c(i, l[[k]])
    if (!length(l)) 
      l <- list(i)
    x <- c(x, l)
  }
  list(drop = drop, l = x)
}

# Special sorting function
SortNrList <- function(x, index.return = FALSE) {
  m <- matrix(0, length(x), max(sapply(x, length)))
  for (i in seq_len(length(x))) m[i, seq_len(length(x[[i]]))] <- x[[i]]
  ix <- SortRows(m, index.return = TRUE)
  if (index.return) 
    return(ix)
  x[ix]
}

#' Sorting rows
#'
#' @param m 
#' @param cols 
#' @param index.return 
#'
#' @return sorted m
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
#' @examples
#' SortRows(matrix(sample(1:3,15,TRUE),5,3))
SortRows <- function(m, cols = 1:dim(m)[2], index.return = FALSE) {
  ix <- eval(parse(text = paste("order(", paste("m[,", cols, ",drop=TRUE]", sep = "", collapse = ","), 
                                ")")))
  if (index.return) 
    return(ix)
  m[ix, , drop = FALSE]
}


#' Finding table(s) of hierarchical variable groups
#'
#' A single table or two linked tables are found
#'
#' @encoding UTF8
#'
#' @param x Matrix or data frame containing the variables
#' @param findLinked When TRUE, two linked tables can be in output
#' @param mainName When TRUE the groupVarInd ouput is named according to first variable in group.
#' @param fCorr When non-null x is not needed as input.
#' @param CheckHandling Function (warning or stop) to be used in problematic situations.
#'
#' @return Output is a list with items
#'  \item{groupVarInd}{List defining the hierarchical variable groups. First variable has most levels.}
#'  \item{table}{List containing one or two tables. These tables are coded as indices referring to elements of groupVarInd.}
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#'  x <- rep(c('A','B','C'),3)
#'  y <- rep(c(11,22,11),3)
#'  z <- c(1,1,1,2,2,2,3,3,3)
#'  zy <- paste(z,y,sep='')
#'  m <- cbind(x,y,z,zy)
#'  FindTableGroup(m)
#'  FindTableGroup(m,findLinked=TRUE)
FindTableGroup <- function(x = NULL, findLinked = FALSE, mainName = TRUE, fCorr = FactorLevCorr(x), 
                           CheckHandling = warning) {
  hier <- HierarchicalGroups(mainName = mainName, eachName = FALSE, fCorr = fCorr)
  table1 <- UniqueNrList(hier, 1)
  table2 <- UniqueNrList(hier, -1)
  if (identical(table1, table2)) 
    table2 <- NULL
  if (is.null(table2)) {
    if (length(table1) != length(hier)) {
      outside <- seq_len(length(hier))[!(seq_len(length(hier)) %in% table1)]
      table2 <- outside[UniqueNrList(hier[outside])]
    }
  }
  if (!findLinked) {
    # extra check
    uh <- unlist(hier)
    if (length(unique(uh)) == length(uh)) 
      uniqueTable <- TRUE else uniqueTable <- FALSE
      if (uniqueTable & !is.null(table2)) 
        stop("Error detected in unique algorithm")
      table2 <- NULL
      if (!uniqueTable) 
        CheckHandling("Not a single unique table")
  } else {
    if (length(unique(c(table1, table2))) != length(hier)) 
      CheckHandling("All variables could not be used")
  }
  if (is.null(table2)) 
    return(list(groupVarInd = hier, table = list(ind1 = table1)))
  return(list(groupVarInd = hier, table = list(ind1 = table1, ind2 = table2)))
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
  rbind(r1, data.frame(levels = symbols[matlabColon(1, k)], codes = codes[matlabColon(1, 
                                                                                      k)], stringsAsFactors = FALSE))
}




#' Finding commonCells
#'
#' Finding lists defining common cells as needed for the input parameter
#' commonCells to the function protectLinkedTables in package sdcTable.
#' The function handles two tables based on the same main variables
#' but possibly different aggregating variables.
#'
#' @encoding UTF8
#'
#' @param dimList1 As input parameter dimList to the function makeProblem in package sdcTable.
#' @param dimList2 Another dimList with the same names and using the same level names.
#'
#' @return Output is a list according to the specifications in sdcTable.
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#'  x <- rep(c('A','B','C'),3)
#'  y <- rep(c(11,22,11),3)
#'  z <- c(1,1,1,2,2,2,3,3,3)
#'  zy <- paste(z,y,sep='')
#'  m <- cbind(x,y,z,zy)
#'  fg <- FindTableGroup(m,findLinked=TRUE)
#'  dimLists <- FindDimLists(m,fg$groupVarInd)
#'  # Using table1 and table2 in this example cause error,
#'  # but in other cases this may work well
#'  try(FindCommonCells(dimLists[fg$table$table1],dimLists[fg$table$table2]))
#'  FindCommonCells(dimLists[c(1,2)],dimLists[c(1,3)])
FindCommonCells <- function(dimList1, dimList2) {
  okNames <- TRUE
  if (length(unique(names(dimList1))) != length(dimList1)) 
    okNames <- FALSE
  if (length(unique(names(dimList2))) != length(dimList2)) 
    okNames <- FALSE
  if (!okNames) 
    stop("Elements of dimLists must be named uniquely.")
  commonNames <- names(dimList1)[names(dimList1) %in% names(dimList2)]
  niceProblem <- identical(names(dimList1), names(dimList2))
  if (!niceProblem) 
    stop("Only problems where identical(names(dimList1),names(dimList2))=TRUE implemented.")
  
  commonCells <- vector("list", length(commonNames))
  names(commonCells) <- commonNames
  for (i in seq_len(length(commonNames))) {
    okAll <- (niceProblem & identical(dimList1[[i]], dimList2[[i]]))
    if (okAll) 
      commonCells[[i]] <- vector("list", 3) else commonCells[[i]] <- vector("list", 4)
      commonCells[[i]][[1]] <- commonNames[i]
      commonCells[[i]][[2]] <- commonNames[i]
      if (okAll) 
        commonCells[[i]][[3]] <- "All" else {
          #c1 <- dimList1[[which(names(dimList1) == commonNames[i])]]$codes
          #c2 <- dimList2[[which(names(dimList2) == commonNames[i])]]$codes
          #cc <- c1[c1 %in% c2]
          #commonCells[[i]][[3]] <- cc
          #commonCells[[i]][[4]] <- cc
          i1 = which(names(dimList1) == commonNames[i])
          i2 = which(names(dimList2) == commonNames[i])
          c1 <- dimList1[[i1]]$codes
          c2 <- dimList2[[i2]]$codes
          cc <- c1[c1 %in% c2]
          commonCells[[i]][[3]] <- DimListReCode(cc, dimList1[[i1]])
          commonCells[[i]][[4]] <- DimListReCode(cc, dimList2[[i2]])
        }
  }
  commonCells
}




UniqueNrList <- function(x, sort = 0) {
  if (sort == 0) 
    ix <- seq_len(length(x)) else {
      ix <- SortNrList(x, index.return = TRUE)
      if (sort < 0) 
        ix <- rev(ix)
    }
  z <- NULL
  xz <- NULL
  for (i in ix) {
    if (!any((x[[i]] %in% xz))) {
      z <- c(z, i)
      xz <- c(xz, x[[i]])
    }
  }
  sort(z)
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



# Re-coding since “bogus» codes are removed internally in sdcTable.
# See sdcTools/UserSupport/issues/133 at GitHub
DimListReCode <- function(codes, dimList) {
  hi <- DimList2Hierarchy(dimList)
  dupCodes <- unique(hi$mapsTo[duplicated(hi$mapsTo)])
  hi <- hi[!(hi$mapsTo %in% dupCodes), ]
  if (nrow(hi) == 0) 
    return(codes)
  for (i in 0:nrow(hi)) {
    ma <- match(codes, hi$mapsFrom)
    isMatch <- !is.na(ma)
    if (!any(isMatch)) 
      return(codes)
    codes[isMatch] <- hi$mapsTo[ma[isMatch]]
  }
  stop("Something is wrong. Cyclic hierarchy?")
}








