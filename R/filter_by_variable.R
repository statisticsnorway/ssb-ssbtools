#' Filter a List of Items or Retrieve Names by a Variable
#'
#' Filters a list of items, retaining only those associated with a specific variable, 
#' or retrieves the names of items associated with the variable. The association between 
#' items and variables is provided via a named list, where each element contains a 
#' vector of variables corresponding to an item in `items`.
#' 
#' `filter_by_variable()` returns the filtered list of items, whereas 
#' `names_by_variable()` is a simpler function that just returns the names of the items.
#'
#' @param variable A character string. The variable to filter the items by.
#' @param items A named list of elements. These can be any type of objects (e.g., formulas, data, etc.).
#' @param variable_mapping A named list. Each element is a character vector of variables associated 
#'   with the corresponding item in `items`. The names of the list in `variable_mapping` should match 
#'   the names of the list in `items`.
#'   
#' @return 
#' - `filter_by_variable()`: A named list containing a subset of `items` where each element is 
#'   associated with the specified `variable`. If no matches are found, an empty list is returned.
#' - `names_by_variable()`: A character vector of names from `variable_mapping` that are associated 
#'   with the specified `variable`. If no matches are found, an empty character vector is returned.
#'
#' 
#' @note This function is written and documented by ChatGPT after some discussion. 
#'       The examples have been chosen to be relevant in connection with the 
#'        \code{\link{tables_by_formulas}} function. 
#' 
#' @examples
#' 
#' items <- list(
#'   table_1 = ~region * sector2, 
#'   table_2 = ~region1:sector4 - 1, 
#'   table_3 = ~region + sector4 - 1
#' )
#'
#' variable_mapping <- list(
#'   table_3 = c("z", "y"), 
#'   table_1 = c("value", "x"), 
#'   table_2 = c("value", "x", "y")
#' )
#'
#' filter_by_variable("value", items, variable_mapping)
#' filter_by_variable("y", items, variable_mapping)
#' filter_by_variable("nonexistent", items, variable_mapping)
#'
#' names_by_variable("value", variable_mapping)
#' names_by_variable("y", variable_mapping)
#' names_by_variable("nonexistent", variable_mapping)
#'
#' @export
filter_by_variable <- function(variable, items, variable_mapping) {
  # Ensure the order of `items` and `variable_mapping` is aligned by their names
  matching_names <- intersect(names(items), names(variable_mapping))
  
  # Subset items and variable_mapping by matching names
  aligned_items <- items[matching_names]
  aligned_variable_mapping <- variable_mapping[matching_names]
  
  # Filter the aligned items based on whether the variable is in the variable mapping
  filtered_items <- aligned_items[sapply(aligned_variable_mapping, function(vars) variable %in% vars)]
  
  # Return the filtered list
  return(filtered_items)
}



#' @rdname filter_by_variable
#' @export
names_by_variable <- function(variable, variable_mapping) {
  matches <- sapply(variable_mapping, function(vars) variable %in% vars)
  names(matches)[matches]
}




