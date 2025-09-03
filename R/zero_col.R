

#' Check for empty matrix columns (or rows)
#' 
#' More generally, checks that both row/col sums and sums of absolute
#' values equal a target.  
#' For `value = 0`, this means all entries are zero.  
#' `single_col()` is a wrapper with `value = 1`, often used to check for
#' dummy columns/rows with exactly one element that is `1`.
#' 
#' Memory usage is reduced by applying `abs()` checks only to rows/columns
#' whose total sum is already the target.
#'
#' @param x Numeric matrix. Sparse matrices from the Matrix package are also supported.
#' @param rows Logical; if `TRUE` check rows, else columns.
#' @param value Numeric target (default `0`).
#'
#' @return Logical vector.
#' @export
#' 
#' @examples
#' m <- matrix(c(
#'   0,  0, 0, 0, 
#'   1, -1, 0, 0,
#'   0,  0, 1, 0
#' ), nrow = 3, byrow = TRUE)
#'
#' zero_col(m)
#' zero_col(m, rows = TRUE)
#' single_col(m)
#' single_col(m, rows = TRUE)
#' 
zero_col <- function(x, rows = FALSE, value = 0) {
  if (rows) {
    a <- rowSums(x) == value
    if (any(a)) {
      a[a] <- rowSums(abs(x[a, , drop = FALSE])) == value
    }
    return(a)
  }
  a <- colSums(x) == value
  if (any(a)) {
    a[a] <- colSums(abs(x[, a, drop = FALSE])) == value
  }
  a
}


#' @rdname zero_col
#' @param ... Passed to `zero_col()`.
#' @export
single_col <- function(..., value = 1) {
  zero_col(..., value = value)
}