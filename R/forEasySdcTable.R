
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








