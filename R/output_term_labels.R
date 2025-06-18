#' Extract vector of term labels from a data.frame
#' 
#' The data.frame is assumed to be have been constructed with 
#' [ModelMatrix()] functionality using the `formula` parameter.
#' 
#'
#' @param x data.frame
#'
#' @returns vector of term labels
#' @export
#' @seealso [formula_term_labels()], [tables_by_formulas()]
#'
#' @examples
#' 
#' out <- model_aggregate(SSBtoolsData("magnitude1"), 
#'                        formula = ~eu:sector4 + geo * sector2, 
#'                        sum_vars = "value",
#'                        avoid_hierarchical = TRUE)
#' out
#' term_labels <- output_term_labels(out)
#' term_labels
#' cbind(term_labels, out)
#' 
output_term_labels <- function(x) {
  startInd <- c(attr(x, "startRow"), nrow(x) + 1)
  if (is.null(startInd)) {
    stop("startRow attribute not found")
  }
  terms <- character(0)
  for (i in seq_len(length(startInd) - 1)) {
    terms <- c(terms, rep(names(startInd)[i], startInd[i + 1] - startInd[i]))
  }
  terms
}