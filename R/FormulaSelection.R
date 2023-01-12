
#' Limit matrix or data frame to selected model terms 
#' 
#' For use with output from \code{\link{ModelMatrix}} or data frames derived from such output.
#' 
#' The selection is based on `startCol` or `startRow` attribute in input `x`.
#'
#' @param x Model matrix or a data frame
#' @param formula Formula representing the limitation 
#'
#' @return Limited model matrix or a data frame
#' @export
#'
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' x <- ModelMatrix(z, formula = ~age * year)
#' 
#' FormulaSelection(x, ~age)
#' FormulaSelection(x, ~year)
#' FormulaSelection(x, ~year:age)
#' 
#' 
#' a <- ModelMatrix(z, formula = ~age * geo + year, crossTable = TRUE)
#' b <- cbind(as.data.frame(a$crossTable), 
#'            sum = (t(a$modelMatrix) %*% z$ths_per)[, 1], 
#'            max = DummyApply(a$modelMatrix, 
#'            z$ths_per, max))
#' rownames(b) <- NULL
#' attr(b, "startRow") <- attr(a$modelMatrix, "startCol", exact = TRUE)
#' 
#' FormulaSelection(b, ~geo * age)
#' FormulaSelection(b, ~age:geo)
#' FormulaSelection(b, ~year)
FormulaSelection <- function(x, formula) {
  startInd <- c(attr(x, "startCol"), ncol(x) + 1)
  isCol <- length(startInd) > 1
  if (isCol) {
    n <- ncol(x)
  } else {
    startInd <- c(attr(x, "startRow"), nrow(x) + 1)
    n <- nrow(x)
  }
  if (length(startInd) <= 1) {
    stop("startCol or startRow attribute not found")
  }
  terms <- attr(terms(formula), "term.labels")
  selection <- rep(FALSE, n)
  for (i in seq_along(terms)) {
    ma <- match(OrderedVarNames(terms[i]), OrderedVarNames(names(startInd)))
    selection[startInd[ma]:(startInd[ma + 1] - 1)] <- TRUE
  }
  if (isCol) {
    out <- x[, selection, drop = FALSE]
    attr(out, "startCol") <- NULL
  } else {
    out <- x[selection, , drop = FALSE]
    attr(out, "startRow") <- NULL
  }
  out
}

# Function from CalibrateSSB
OrderedVarNames <- function(x, sep = ":") {
  unlist(lapply(strsplit(x, sep), function(x) paste(sort(x), collapse = sep)))
}



#' @rdname FormulaSelection
#' @export
#' @note `formula_selection` and `FormulaSelection` are identical
formula_selection <- FormulaSelection
