

#' Round values that are close two whole numbers
#' 
#' @details
#' When \code{digits} is \code{NA}, \code{Inf} or \code{NULL}, input is returned unmodified. 
#' When there is more than one element in \code{digits} or \code{onlyZeros}, 
#' rounding is performed column-wise.     
#' 
#'
#' @param x vector or matrix
#' @param digits parameter to \code{\link{round}}
#' @param onlyZeros Only round values close to zero 
#'
#' @return Modified x
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- c(0.0002, 1.00003, 3.00014)
#' RoundWhole(x)     # No values rounded
#' RoundWhole(x, 4)  # One value rounded
#' RoundWhole(x, 3)  # All values rounded
#' RoundWhole(x, NA) # No values rounded (always)
#' RoundWhole(x, 3, TRUE)  # One value rounded
#' RoundWhole(cbind(x, x, x), digits = c(3, 4, NA))
#' RoundWhole(cbind(x, x), digits = 3, onlyZeros = c(FALSE, TRUE))
RoundWhole <- function(x, digits = 9, onlyZeros = FALSE) {
  if (is.null(digits))
    return(x)
  if (length(digits) > 1 | length(onlyZeros) > 1){
    return(RoundWholeColumns(x, digits, onlyZeros))
  }
  if (!is.finite(digits))
    return(x)
  round_x <- round(x)
  round_x_digits <- round(x, digits = digits)
  if (onlyZeros) 
    toWhole <- round_x_digits == 0 
  else 
    toWhole <- round_x == round_x_digits
  toWhole[is.na(toWhole)] <- FALSE
  x[toWhole] <- round_x[toWhole]
  x
}


RoundWholeColumns <- function(x, digits = 9, onlyZeros = FALSE){
  ncolx  <- ncol(x)
  digits <- rep_len(digits,ncolx)
  onlyZeros <- rep_len(onlyZeros,ncolx)
  for(i in seq_len(ncol(x))){
    x[, i] <- RoundWhole(x[, i], digits[i], onlyZeros[i])
  }
  x
}