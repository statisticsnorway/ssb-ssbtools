#' Get column names from a data.frame, tibble, or data.table
#'
#' This helper function returns column names based on either column
#' indices (numeric) or column names (character). It works consistently
#' across `data.frame`, `tibble`, and `data.table` objects.
#'
#' By default, `cols = NULL` returns `character(0)`, matching the
#' behavior of `names(data[1, NULL, drop = FALSE])`.  
#' If `preserve_NULL = TRUE`, the function instead returns `NULL`.
#'
#' @param data A data frame, tibble, or data.table.
#' @param cols Column selection, either as numeric indices, character names,
#'   or `NULL`.
#' @param preserve_duplicates Logical, default `FALSE`.  
#' If `TRUE`, duplicates and order in `cols` are preserved.  
#' If `FALSE`, duplicates are removed while preserving order of first appearance.
#' @param preserve_NULL Logical, default `FALSE`.  
#' If `TRUE`, returns `NULL` when `cols = NULL`.  
#' If `FALSE`, returns `character(0)` when `cols = NULL`.
#'
#' @return A character vector of column names, or `NULL` if `cols = NULL`
#'   and `preserve_NULL = TRUE`.
#'   
#' @note This function is written and documented by ChatGPT after some discussion.
#'
#' @examples
#' df <- data.frame(a = 1, b = 2, c = 3)
#'
#' # NULL input handling
#' get_colnames(df, NULL)
#' get_colnames(df, NULL, preserve_NULL = TRUE)
#'
#' # Numeric input
#' get_colnames(df, c(2, 2, 1))
#' get_colnames(df, c(2, 2, 1), preserve_duplicates = TRUE)
#'
#' # Character input
#' get_colnames(df, c("c", "a", "c"))
#' get_colnames(df, c("c", "a", "c"), preserve_duplicates = TRUE)
#'
#' @export
get_colnames <- function(data, cols, preserve_duplicates = FALSE, preserve_NULL = FALSE) {
  # Handle NULL explicitly
  if (is.null(cols)) {
    if (preserve_NULL) {
      return(NULL)
    } else {
      return(character(0))
    }
  }
  
  # Extract all column names from the object
  nms <- names(data)
  
  if (is.character(cols)) {
    if (preserve_duplicates) {
      return(cols)
    } else {
      return(unique(cols))
    }
    
  } else if (is.numeric(cols)) {
    if (preserve_duplicates) {
      return(nms[cols])
    } else {
      return(unique(nms[cols]))
    }
    
  } else {
    stop("cols must be either character, numeric, or NULL")
  }
}


