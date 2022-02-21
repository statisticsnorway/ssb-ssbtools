#' Factor level correlation
#'
#' A sort of correlation matrix useful to detect (hierarchical) relationships between the levels of factor variables.
#'
#'
#' @encoding UTF8
#' @noMd
#'
#' @param x Input matrix or data frame containing the variables
#'
#' @return Output is a sort of correlation matrix.
#'
#'         Here we refer to ni as the number of present levels of variable i (the number of unique elements) and we refer to nij as the number
#'         of present levels obtained by crossing variable i and variable j (the number unique rows of x[,c(i,j)]).
#'
#'         The diagonal elements of the output matrix contains the number of present levels of each variable (=ni).
#'
#'         The absolute values of off-diagonal elements:
#'         \item{0}{when nij = ni*nj}
#'         \item{1}{when nij = max(ni,nj)}
#'         \item{Other values}{Computed as (ni*nj-nij)/(ni*nj-max(ni,nj))}
#'
#'         So 0 means that all possible level combinations exist in the data and 1 means that the two variables are
#'         hierarchically related.
#'
#'         The sign of off-diagonal elements:
#'         \item{positive}{when ni<nj}
#'         \item{negative}{when ni>nj}
#'
#'         In cases where ni=nj elements will be positive above the diagonal and negative below.
#'
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
#'  FactorLevCorr(m)
#'
FactorLevCorr <- function(x) {
  if (NROW(x) <= 1) 
    stop(paste(NROW(x), "row in data."))
  x <- unique(x, MARGIN = 1)
  n <- NCOL(x)
  nLevels <- rep(NaN, n)
  for (i in matlabColon(1, n)) nLevels[i] <- Nlevels(x[, i])
  z <- diag(nLevels, nrow = length(nLevels))  # nrow input to allow length one as input
  for (i in matlabColon(1, n)) for (j in matlabColon(i + 1, n)) {
    ni <- nLevels[i]
    nj <- nLevels[j]
    nij <- Nlevels(x[, c(i, j)])
    multij <- ni * nj
    maxij <- max(ni, nj)
    if (ni <= nj) 
      one <- 1 else one <- -1
    if (nij == maxij) 
      z[i, j] <- one else z[i, j] <- one * (multij - nij)/(multij - maxij)
    z[j, i] <- -z[i, j]
  }
  colnames(z) <- colnames(x)
  rownames(z) <- colnames(x)
  z
}


Nlevels = function(x){
  NROW(unique(x,MARGIN=1))
}



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
#' @param total String used to name totals. A vector of length `ncol(x)` is also possible (see examples).  
#'
#' @return Output is a list according to the specifications in sdcTable.
#'         When xReturn is TRUE output has an extra list level and x is the first element.
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
#'  FindDimLists(m)
#'  FindDimLists(m, total = paste0("A", 1:4))
FindDimLists <- function(x, groupVarInd = HierarchicalGroups(x = x), addName = FALSE, 
                         sep = ".", xReturn = FALSE, total = "Total") {
  
  # Generalization to `length(total)>1` could also have been implemented by changing 
  # CheckLevels, DimFromHier and DimFromHier1. Making the change here is easier and safer.
  if (length(total) > 1) {
    if (length(total) != ncol(x)) {
      stop("wrong length of total")
    }
    dimLists <- FindDimLists(x = x, groupVarInd = groupVarInd, addName = addName, sep = sep, xReturn = xReturn, total = "t_O2T_aL83")
    tot <- total[match(names(dimLists), colnames(x))]
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








