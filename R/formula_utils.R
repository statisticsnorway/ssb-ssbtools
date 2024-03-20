#' Generate model formula by specifying which variables have totals or not.
#'
#' @param nontotal_vars character vector of the variable names without totals
#' @param total_vars character vector of the variable names with totals
#' @param simplify logical value, default TRUE. Determines whether the formula
#' should be simplified before output or not.
#' @param env the environment for the output formula
#'
#' @return model formula
#' @export
#'
#' @examples
#' table_formula_from_vars(c("a", "b", "c"), c("a"))
#' table_formula_from_vars(c("a", "b", "c"), c("a", "c"))
#' table_formula_from_vars(c("a", "b", "c"), c("a", "b", "c"))
#' table_formula_from_vars(c("a", "b", "c"), NULL)
#' table_formula_from_vars(NULL, c("a", "b", "c"))
#' table_formula_from_vars(c("a", "b"), c("d"))
table_formula_from_vars <-
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
#' @export
#'
#' @examples
#' f2 <- table_formula_from_vars(c("a", "b", "c"), c("a", "c"))
#' formula_include_hierarchies(f2, list(a = c("hello", "world")),
#' simplify = FALSE)
formula_include_hierarchies <-
  function(f, hier_vars, simplify = TRUE, env = parent.frame()) {
    for (v in names(hier_vars)) {
      replace <- formula(paste0("~", paste(hier_vars[[v]], collapse = "+")), env = env)
      replace <- list(replace[[length(replace)]])
      names(replace) <- v
      f <- do.call(substitute, list(expr = f, env = replace))
      environment(f) <- env
    }
    if (simplify)
      return(formula(terms(f, simplify = TRUE), env = env))
    f
  }


#' Function for linking tables defined using model formulas
#'
#' @param lof list or vector of formulas to be linked
#' @param simplify logical value, default TRUE. Determines whether the formula
#' should be simplified before output or not.
#' @param env the environment for the output formula
#'
#' @return model formula
#' @export
#'
#' @examples
#' lof1 <- c(~a+b, ~a:c, ~c*d)
#' combine_table_formulas(lof1, simplify = TRUE)
#' combine_table_formulas(lof1, simplify = FALSE)
combine_table_formulas <- function(lof, simplify = TRUE, env = parent.frame()) {
  lof <- sapply(lof, function(x)
    deparse(x[[length(x)]]))
  out <- formula(paste("~", paste(lof, collapse = "+")), env = env)
  if (simplify)
    return(formula(terms(out, simplify = TRUE), env = env))
  out
}
