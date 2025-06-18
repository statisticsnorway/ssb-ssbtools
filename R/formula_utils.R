#' Generate model formula by specifying which variables have totals or not.
#'
#' @param nontotal_vars character vector of the variable names without totals
#' @param total_vars character vector of the variable names with totals
#' @param simplify logical value, default TRUE. Determines whether the formula
#' should be simplified before output or not.
#' @param env the environment for the output formula
#'
#' @return model formula
#' @keywords internal
#' @export
#' @importFrom utils combn
#' @author Daniel Lupp
#'
#' @examples
#' formula_from_vars(c("a", "b", "c"), c("a"))
#' formula_from_vars(c("a", "b", "c"), c("a", "c"))
#' formula_from_vars(c("a", "b", "c"), c("a", "b", "c"))
#' formula_from_vars(c("a", "b", "c"), NULL)
#' formula_from_vars(NULL, c("a", "b", "c"))
#' formula_from_vars(c("a", "b"), c("d"))
formula_from_vars <-
  function(nontotal_vars = NULL,
           total_vars = NULL,
           simplify = TRUE,
           env = parent.frame()) {
    nontotal_vars <- unique(nontotal_vars)
    total_vars <- unique(total_vars)
    if (is.null(nontotal_vars)) {
      if (is.null(total_vars))
        return(NULL)
      return(formula(paste0("~", paste(total_vars, collapse = "*")), env = env))
    }
    myterms <- paste(nontotal_vars, collapse = ":")
    if (!is.null(total_vars)) {
      if (all(nontotal_vars %in% total_vars))
        return(formula(paste0("~", paste(total_vars, collapse = "*")), env = env))
      dims <- unique(c(total_vars, nontotal_vars))
      iter <- seq(from = 0, to = length(total_vars))
      for (m in iter) {
        subs <- combn(total_vars, m = m)
        colon_contr <-
          matrix(apply(subs, 2, function(x)
            dims[!(dims %in% x)]), ncol = ncol(subs))
        partial_forms <-
          apply(colon_contr, 2, function(x)
            paste(x, collapse = ":"))
        if (length(partial_forms) != 1)
          myterms <- c(myterms, partial_forms)
        else
          if (partial_forms != "")
            myterms <- c(myterms, partial_forms)
      }
    }
    out <- formula(paste0("~", paste(myterms, collapse = "+")), env = env)
    if (simplify)
      return(formula(terms(out, simplify = TRUE), env = env))
    out
  }


#' Replace variables in formula with sum of other variables
#'
#' @param f A model formula.
#' @param replacements A named list. The names of `replacements` must correspond to variables in `f`.
#'   Each element in `replacements` must be a character vector consisting of 
#'   the variables you wish to replace.
#' @param simplify Logical, default is FALSE. Determines whether the formula 
#'   should be expanded and simplified before output or not.
#' @param env The environment for the output formula.
#'
#' @return model formula
#' @keywords internal
#' @export
#' @author Daniel Lupp and Øyvind Langsrud
#'
#' @examples
#' f <- ~b + a*c  + b:d
#' substitute_formula_vars(f, list(a = c("hello", "world", "b"), 
#'                                 b = c("Q1", "Q2")))
#' 
substitute_formula_vars <-
  function(f, replacements, simplify = FALSE, env = parent.frame()) {
    replace <-list()
    for (v in names(replacements)) {
      replacements_v <- replacements[[v]]
      n <- length(replacements_v)
      if (n > 1) {
        replacements_v[1] <- paste0("(", replacements_v[1])
        replacements_v[n] <- paste0(replacements_v[n], ")")
      }
      replace_v <- formula(paste0("~", paste(replacements_v, collapse = "+")), env = env)
      replace_v <- list(replace_v[[length(replace_v)]])
      names(replace_v) <- v
      replace <- c(replace, replace_v)
    }
    f <- as.formula(do.call(substitute, list(expr = f, env = replace)), env = env)
    if (simplify)
      return(formula(terms(f, simplify = TRUE), env = env))
    f
  }


#' Combine formulas
#' 
#' Combining formulas by `+` or another operator.
#' This is particularly useful for linking tables in the case of table building with formulas.
#'
#' @param lof list or vector of formulas to be linked
#' @param operator `"+"` (default), `"*"`, `":"` or another operator
#' @param simplify logical value, default FALSE. Determines whether the formula
#' should be expanded and simplified before output or not.
#' @param env the environment for the output formula
#'
#' @return model formula
#' @keywords internal
#' @export
#' @author Daniel Lupp and Øyvind Langsrud
#'
#' @examples
#' lof1 <- c(~a+b, ~a:c, ~c*d)
#' combine_formulas(lof1)
#' combine_formulas(lof1, operator = "*")
#' combine_formulas(lof1, simplify = TRUE)
#' 
#' # Intercept is included when needed
#' lof2 <- c(~a+b -1, ~a:c -1, ~c*d)
#' combine_formulas(lof2)
#' combine_formulas(lof2, simplify = TRUE)
#' combine_formulas(lof2[1:2])
#' combine_formulas(lof2[1:2], simplify = TRUE)
combine_formulas <- function(lof, operator = "+", simplify = FALSE, env = parent.frame()) {
  lof_ <- sapply(lof, function(x)
    deparse_here(x[[length(x)]]))
  lof_ <- paste0("(", lof_, ")")
  out <- formula(paste("~", paste(lof_, collapse = operator)), env = env)
  if (operator == "+") {
    if (!has_intercept(out)) {
      if (any(sapply(lof, has_intercept))) {
        out <- formula(paste("~", paste(lof_, collapse = operator), " + 1"), env = env)
      }
    }
  }
  if (simplify)
    return(formula(terms(out, simplify = TRUE), env = env))
  out
}



#' Retrieve term labels from a formula
#'
#' This function extracts the term labels from the right-hand side of a given R formula.
#' If an intercept is to be included (and a name for the intercept is provided), it will be added
#' as the first element of the returned vector.
#'
#' The default intercept value, "(Intercept)", is chosen to be consistent with the intercept label
#' returned by functions such as [stats::lm()], [stats::model.matrix()], and similar modeling functions.
#'
#' @param formula An R formula, e.g. ~ x1 * x2.
#' @param intercept A character string indicating the name for the intercept. The default value is "(Intercept)".
#' If NULL is provided, the intercept will not be included, even if present in the formula.
#'
#' @return A character vector containing the term labels. If an intercept is present and intercept is not NULL,
#' the intercept is returned first, followed by the remaining terms.
#' 
#' @keywords internal
#' @export
#' 
#' @note This function is documented by ChatGPT after some discussion. 
#'
#' @examples
#' # With intercept:
#' formula_term_labels(~ x1 * x2)
#'
#' # Without intercept:
#' formula_term_labels(~ x1 * x2, intercept = NULL)
#'
formula_term_labels <- function(formula, intercept = "(Intercept)") {
  terms_obj <- terms(formula)
  terms <- attr(terms_obj, "term.labels")
  if (!is.null(intercept)) {
    if (attr(terms_obj, "intercept")) {
      terms <- c(intercept, terms)
    }
  }
  terms
}





#' Functions for formula manipulation
#' 
#' @details
#' 
#' *  \code{\link{combine_formulas}}: Combine formulas
#' *  \code{\link{formula_from_vars}}:  Generate model formula by specifying which variables have totals or not
#' *  \code{\link{substitute_formula_vars}}: Replace variables in formula with sum of other variables
#' *  \code{\link{formula_term_labels}}: Retrieve term labels from a formula
#'
#' @docType data
#' @name formula_utils
#'
NULL


# copy of base::deparse1 for old R versions without deparse1
deparse1_copy <- function (expr, collapse = " ", width.cutoff = 500L, ...) 
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)


deparse_here <- get0("deparse1", envir=baseenv(), ifnotfound = deparse1_copy)



has_intercept <- function(formula) {
  terms_obj <- terms(formula)
  attr(terms_obj, "intercept") == 1
}

