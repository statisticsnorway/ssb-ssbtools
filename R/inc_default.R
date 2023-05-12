#' Default progress indicator function 
#' 
#' @param i i in "i out of n"
#' @param n n in "i out of n"
#' @param steps Number of dots to print
#' @param dot dot 
#'
#' @export
#' @keywords internal
#'
#' @examples
#' for (i in 1:5) inc_default(i, 5)
#' cat("\n")
#' for (i in 1:100) inc_default(i, 100)
#' cat("\n")
#' for (i in 1:1000) inc_default(i, 1000)
#' cat("\n")
#' for (i in 1:1000) inc_default(i, 1000, steps = 10)
#' cat("\n")
#' for (i in 1:10) inc_default()
#' cat("\n")
inc_default <- function(i = 0L, n = 0L, steps = 25L, dot = ".") {
  if (i%%max(1, n%/%steps) == 0L) {
    cat(".")
    flush.console()
  }
}