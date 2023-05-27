
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




