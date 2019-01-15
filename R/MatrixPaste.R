#' Combining columns of a matrix
#'
#' @param x  Matrix or vector
#' @param sep String used to combine columns
#' @param forceCharacter When FALSE single column input will keep to original class in output.
#' @param stringEmpty String used when input is empty (can be set to NULL)
#'
#' @return Character vector or possibly same vector as input
#' @details Each row in input will be combined to a single string using sep.
#'
#' @keywords internal
#' @author Øyvind Langsrud
#'
#' @examples
#' \dontrun{
#' MatrixPaste(matrix(1:12,3,4))
#' MatrixPaste(1:5)
#' MatrixPaste(1:5, forceCharacter=TRUE)
#' MatrixPaste(matrix(integer(0),3,0))
#' MatrixPaste(NULL)
#' }
MatrixPaste = function(x, sep="_", forceCharacter=FALSE, stringEmpty = " "){
  if(is.null(x)) return(stringEmpty)
  if(NCOL(x)==0) return(rep(stringEmpty,NROW(x)))
  if(NCOL(x)<=1){
    if(forceCharacter)
      return(as.character(as.vector(x)))
    return(as.vector(x))
  }
  apply(x , 1 , paste , collapse = sep )
}

#' @rdname MatrixPaste
#' @keywords internal
MatrixPaste1 = function(x,stringEmpty = "1") MatrixPaste(x,stringEmpty = stringEmpty)