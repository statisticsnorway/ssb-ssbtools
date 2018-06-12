#' Simulate Matlab's ':'
#' 
#' A function to simulate Matlab's ':' operator
#'
#' @param from numeric. The start value
#' @param to numeric. The end value.
#' 
#' @details matlabColon(a,b) returns a:b (R's version) unless a > b, in which case it returns numeric(0). 
#'
#' @return A numeric vector, possibly empty. 
#' 
#' @export
#' 
#' @author Bjørn-Helge Mevik
#' 
#' @note The function is stolen from own package ffmanova. 
#' 
#' @seealso \code{\link{seq}}
#'
#' @examples
#' identical(3:5, matlabColon(3, 5)) ## => TRUE
#' 3:1 ## => 3 2 1
#' matlabColon(3, 1) ## => numeric(0)
matlabColon <- function (from, to) 
{
  if (from > to)
    numeric(0)
  else from:to
}