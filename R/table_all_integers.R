#' Table all integers from 1 to n
#'
#' @param x A vector of integers.
#' @param n The maximum integer value.
#' 
#' @return A 1D array of class `"table"` representing the frequency of each integer from 1 to n.
#'
#' @export
#'
#' @examples
#' table_all_integers(c(2, 3, 5, 3, 5, 3), 7)
#' 
table_all_integers <- function(x, n) {
  table(factor(x, levels = seq_len(n)))
}
