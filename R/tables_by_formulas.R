

#' tables_by_formulas
#'
#' @param data data 
#' @param ...  dots
#' @param table_fun table_fun 
#' @param table_formulas table_formulas 
#' @param substitute_vars substitute_vars 
#' @param collapse_vars collapse_vars 
#'
#' @return A data frame 
#' @export
#'
#' @examples
#' tables_by_formulas(SSBtoolsData("magnitude1"),
#'                    table_fun = model_aggregate, 
#'                    table_formulas = list(table_1 = ~region * sector2, 
#'                                          table_2 = ~region1:sector4 - 1, 
#'                                          table_3 = ~region + sector4 - 1), 
#'                    substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
#'                    collapse_vars = list(sector = c("sector2", "sector4")), 
#'                    sum_vars = "value")
tables_by_formulas <- function(data, 
                               ..., 
                               table_fun, 
                               table_formulas, 
                               substitute_vars = NULL, 
                               collapse_vars = NULL) {
  
  if (length(substitute_vars)) {
    for (i in seq_along(table_formulas)) {
      table_formulas[[i]] <- substitute_formula_vars(table_formulas[[i]], substitute_vars)
    }
    substitute_vars_removed <- remove_included_substitute_elements(substitute_vars) 
  }
  
  formula <- combine_formulas(table_formulas)
  
  a <- table_fun(data, ..., formula = formula, avoid_hierarchical = TRUE, avoidHierarchical = TRUE)
  
  startRow <- attr(a, "startRow", exact = TRUE)
  
  table_indicators <- as.data.frame(matrix(NA, nrow(a), length(table_formulas)))
  names(table_indicators) <- names(table_formulas)
  
  for (i in seq_along(table_formulas)) {
    table_indicators[[i]] <- formula_selection(a, table_formulas[[i]], logical = TRUE)
  }
  
  if (length(substitute_vars)) {
    a <- total_collapse_allow_missing(a, substitute_vars_removed) 
  }
  
  if (length(collapse_vars)) {
    a <- total_collapse_allow_missing(a, collapse_vars) 
  }
  
  a <- cbind(a, table_indicators)

  if (!is.null(startRow)) {
    attr(a, "startRow") <- startRow
  }
  a
  
}

# remove_included_substitute_elements(list(region = c("geo", "eu"), region1 = "eu"))
## list(region = c("geo", "eu"))
# remove_included_substitute_elements(list(region = c("geo", "eu"), region1 = c("geo", "geo2")))
## Error .... Problematic substitute_vars
remove_included_substitute_elements <- function(x) {
  
  x <- x[order(sapply(x, length), decreasing = TRUE)]
  y <- x
  
  for (i in seq_along(y)) {
    y[[i]] <- rep(names(y)[i], length(y[[i]]))
  }
  
  y <- unlist(y)
  d <- duplicated(unlist(x))
  
  if (nrow(unique(cbind(y, d))) > length(x)) {
    stop("Problematic substitute_vars")
  }
  
  keep <- unique(y[!d])
  
  x[keep]
} 

# Allow variables not found in data
total_collapse_allow_missing <- function(data, variables, ...) {
  for (i in seq_along(variables)) {
    variables[[i]] <- variables[[i]][variables[[i]] %in% names(data)]
  }
  variables <- variables[sapply(variables, length) > 0]
  if (!length(variables)) {
    return(data)
  }
  total_collapse(data, variables, ...)
}

