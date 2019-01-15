#' Simulate Matlab's ':'
#' 
#' Functions to generate increasing sequences
#'
#' @param from numeric. The start value
#' @param to numeric. The end value.
#' 
#' @details matlabColon(a,b) returns a:b (R's version) unless a > b, in which case it returns integer(0). 
#' SeqInc(a,b) is similar, but results in error when the calculated length of the sequence (1+to-from) is negative.
#'
#' @return A numeric vector, possibly empty. 
#' 
#' @export
#' 
#' @author Bjørn-Helge Mevik (matlabColon) and Øyvind Langsrud (SeqInc)
#' 
#' 
#' @seealso \code{\link{seq}}
#'
#' @examples
#' identical(3:5, matlabColon(3, 5)) ## => TRUE
#' 3:1 ## => 3 2 1
#' matlabColon(3, 1) ## => integer(0)
#' try(SeqInc(3, 1)) ## => Error
#' SeqInc(3, 2)      ## => integer(0)
matlabColon <- function (from, to) 
{
  if (from > to)
    integer(0) # integer since most common  
  else from:to
}



#' @rdname matlabColon
#' @export
SeqInc <- function (from, to)  
{
  if (from > to){
    if(from-to> 1L)
      stop("Length of sequence (1+to-from) must be non-negative")
    integer(0)
  }
  else from:to
}
