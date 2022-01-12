
#' An `iFunction` argument to \code{\link{GaussSuppression}}
#' 
#' Use this function as `iFunction` or write your own using the same seven first parameters and also using `...`.
#' 
#' The number of candidates decided (`true` and `false`) may differ from the number of candidates processed (`i`) due to parameter `removeDuplicated`
#' and because the decision for some unprocessed candidates can be found due to empty columns. 
#'
#' @param i Number of candidates processed (columns of `x`)   
#' @param I Total number of candidates to be processed (columns of `x`)
#' @param j Number of eliminated dimensions (rows of `x`)
#' @param J Total number of dimensions (rows of `x`)
#' @param true   Candidates decided to be suppressed
#' @param false  Candidates decided to be not suppressed
#' @param na     Candidates not decided
#' @param filename When non-NULL, the above arguments will be saved to this file. 
#'                 Note that `GaussSuppression` passes this parameter via `...`.
#' @param ...  Extra parameters
#'
#' @return `NULL`
#' @export
#'
GaussIterationFunction <- function(i, I, j, J, true, false, na, filename = NULL, ...) {
  cat("i=", i, " (", round(100 * i/I), "%)  ", 
      "j=", j, " (", round(100 * j/J), "%)  ",
      " -----  ",
      length(true) ," out of ", length(true) + length(false), " are suppressed  ", 
      " -----  ",
      length(na) ," not decided",
      "\n", sep = "")
  flush.console()
  if (!is.null(filename)) {
    save(file = filename, i, I, j, J, true, false, na)
  }
  NULL
}
