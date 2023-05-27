
#' Finding hierarchies automatically from data 
#' 
#' \code{\link{FindDimLists}} and \code{AutoHierarchies} wrapped into a single function.
#'
#' @param data Matrix or data frame containing the variables (micro data or cell counts data).
#' @param total String used to name totals. A vector of length `ncol(data)` is also possible (see examples).  
#'
#' @return List of hierarchies
#'
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' dataset <- SSBtoolsData("example1")
#' FindHierarchies(dataset[1:2])
#' FindHierarchies(dataset[2:3])
#' FindHierarchies(dataset[1:4])
#' 
#' FindHierarchies(SSBtoolsData("magnitude1")[1:4], 
#'                 total = c("TOTAL", "unused1", "Europe", "unused2"))
#' 
#' x <- rep(c("A", "B", "C"), 3)
#' y <- rep(c(11, 22, 11), 3)
#' z <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' zy <- paste(z, y, sep = "")
#' m <- cbind(x, y, z, zy)
#' FindHierarchies(m)
#' FindHierarchies(m, total = paste0("A", 1:4))
FindHierarchies <- function(data, total = "Total") {
  AutoHierarchies(FindDimLists(data, total = total))
}