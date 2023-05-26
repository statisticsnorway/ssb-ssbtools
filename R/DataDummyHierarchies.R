


#' Create a (signed) dummy matrix for hierarcical mapping of codes in data
#'
#' @param dataVector A vector of codes in data
#' @param dummyHierarchy Output from \code{\link{DummyHierarchy}}
#'
#' @return  A sparse matrix.
#' Column names are taken from dataVector (if non-NULL) and row names are taken from
#' the row names of dummyHierarchy.
#' @export
#' @author Øyvind Langsrud
#'
DataDummyHierarchy <- function(dataVector, dummyHierarchy) {
  x <- factor(dataVector, levels = colnames(dummyHierarchy))
  m <- dummyHierarchy[, as.integer(x), drop = FALSE]
  colnames(m) <- names(dataVector)
  m
}



#' @rdname DataDummyHierarchy
#' @details `DataDummyHierarchies` is a user-friendly wrapper for the original function `DataDummyHierarchy`.
#'          When `colNamesFromData` is `FALSE` (default), this function returns
#'          `mapply(DataDummyHierarchy,` `data[names(dummyHierarchies)],` `dummyHierarchies)`. 
#'            
#' @param data data
#' @param dummyHierarchies  Output from \code{\link{DummyHierarchies}}
#' @param colNamesFromData  Column names from data when `TRUE` 
#' @export
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")[1:9, ]
#' hi <- FindHierarchies(z[, c("geo", "eu", "age", "year")])
#' dhi <- DummyHierarchies(hi, inputInOutput = TRUE)
#' DataDummyHierarchies(z, dhi, colNamesFromData = TRUE)
DataDummyHierarchies <- function(data, dummyHierarchies,  colNamesFromData = FALSE) {
  
  if(!colNamesFromData)
    return(mapply(DataDummyHierarchy, data[names(dummyHierarchies)], dummyHierarchies))
  
  mapply(function(a, b){names(a) <- a; DataDummyHierarchy(a,b)}, data[names(dummyHierarchies)], dummyHierarchies)
  
}