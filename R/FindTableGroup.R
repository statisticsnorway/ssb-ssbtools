
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









