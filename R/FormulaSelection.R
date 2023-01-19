
#' Limit matrix or data frame to selected model terms 
#' 
#' For use with output from \code{\link{ModelMatrix}} or data frames derived from such output.
#' 
#' The selection is based on `startCol` or `startRow` attribute in input `x`.
#' 
#' With **formula as character**:
#' * **`~`** is included: 
#'          Input is converted by `as.formula` and default intercept is `TRUE`.
#' * **`~`** is not included:
#'          Internally, input data is converted to a formula by adding `~` and possibly `+`'s when the length is `>1`.
#'          Default intercept is `FALSE` unless `"1"` or `"(Intercept)"` (is changed internally to `"1"`) is included. 
#'
#' @param x Model matrix or a data frame
#' @param formula Formula representing the limitation 
#'                or character string(s) to be converted to a formula (see details) 
#' @param intercept Parameter that specifies whether a possible intercept term (overall total) should be included in the output.
#'                  Default is `TRUE` when a formula is input. Otherwise, see details.
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
#' FormulaSelection(x, "age")
#' FormulaSelection(x, ~year)
#' FormulaSelection(x, ~year:age)
#' 
#' # x1, x2, x3, x4 and x4 are identical
#' x1 <- FormulaSelection(x, ~age)
#' x2 <- FormulaSelection(x, "~age")
#' x3 <- FormulaSelection(x, "age", intercept = TRUE)
#' x4 <- FormulaSelection(x, c("1", "age"))
#' x5 <- FormulaSelection(x, c("(Intercept)", "age"))
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
#' FormulaSelection(b, "age:geo")
#' FormulaSelection(b, ~year - 1)
FormulaSelection <- function(x, formula, intercept = NA) {

  if (is.character(formula)) {
    if (!grepl("~", formula[1])) {
      formula <- trimws(formula)
      formula[formula == "(Intercept)"] <- "1"
      if (is.na(intercept)) {
        intercept <- "1" %in% formula
      }
      formula <- paste0("~", paste(formula, collapse = " + "))
    }
    formula <- as.formula(formula)
  }
  if (!inherits(formula, "formula")) {
    stop("parameter formula must be a single formula")
  }
           
  if (is.na(intercept)) {
    intercept <- TRUE
  }
  
  
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
  if (intercept) {
    if (attr(terms(formula), "intercept")) {
      terms <- c("(Intercept)", terms)
    }
  }
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
