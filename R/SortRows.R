
#' Sorting rows of a matrix or data frame
#'
#' @param m matrix or data frame
#' @param cols Indexes of columns, in the desired order, used for sorting.
#' @param index.return logical indicating if the ordering index vector should be 
#'                     returned instead of sorted input. 
#'
#' @return sorted `m` or a row index vector
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' d <- SSBtoolsData("d2w")
#' SortRows(d[4:7])
#' SortRows(d, cols = 4:7)
#' SortRows(d, cols = c(2, 4))
#' 
#' SortRows(matrix(sample(1:3,15,TRUE),5,3))
SortRows <- function(m, cols = 1:dim(m)[2], index.return = FALSE) {
  ix <- eval(parse(text = paste("order(", paste("m[,", cols, ",drop=TRUE]", sep = "", collapse = ","), 
                                ")")))
  if (index.return) 
    return(ix)
  m[ix, , drop = FALSE]
}
