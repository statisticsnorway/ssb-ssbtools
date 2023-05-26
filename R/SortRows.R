
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
