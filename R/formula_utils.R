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
#' @param f model formula
#' @param hier_vars named list. the names of `hier_vars` must correspond to variables in `f`.
#' Each element in `hier_vars` must be a character vector consisting of those
#' variables you wish to replace
#' @param simplify logical value, default TRUE. Determines whether the formula
#' should be simplified before output or not.
#' @param env the environment for the output formula
#'
#' @return model formula
#' @keywords internal
#' @export
#' @author Daniel Lupp and Øyvind Langsrud
#'
#' @examples
#' f <- ~b + a*c  + b:d
#' substitute_formula_terms(f, list(a = c("hello", "world", "b"), 
#'                                  b = c("Q1", "Q2")))
#' 
substitute_formula_terms <-
  function(f, hier_vars, simplify = TRUE, env = parent.frame()) {
    replace <-list()
    for (v in names(hier_vars)) {
      replace_v <- formula(paste0("~", paste(hier_vars[[v]], collapse = "+")), env = env)
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
combine_formulas <- function(lof, operator = "+", simplify = FALSE, env = parent.frame()) {
  lof <- sapply(lof, function(x)
    deparse_here(x[[length(x)]]))
  lof <- paste0("(", lof, ")")
  out <- formula(paste("~", paste(lof, collapse = operator)), env = env)
  if (simplify)
    return(formula(terms(out, simplify = TRUE), env = env))
  out
}



#' Functions for formula manipulation
#' 
#' @details
#' 
#' *  \code{\link{combine_formulas}}: Combine formulas
#' *  \code{\link{formula_from_vars}}:  Generate model formula by specifying which variables have totals or not
#' *  \code{\link{substitute_formula_terms}}: Replace variables in formula with sum of other variables
#'
#' @docType data
#' @name formula_utils
#'
NULL


# copy of base::deparse1 for old R versions without deparse1
deparse1_copy <- function (expr, collapse = " ", width.cutoff = 500L, ...) 
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)


deparse_here <- get0("deparse1", envir=baseenv(), ifnotfound = deparse1_copy)





