
#' Add variables to dataset based on hierarchies
#' 
#' Uses \code{\link{hierarchies_as_vars}} to transform hierarchies, followed by mapping to the dataset.
#'
#' @param data A data frame containing variables with names matching the names of the hierarchies.
#' @inheritParams hierarchies_as_vars
#' @param when_overwritten A function to be called when existing column(s) are overwritten.
#'                         Supply `stop` to invoke an error, `warning` for a warning (default),
#'                         `message` to display an informational message, or `NULL` to do nothing.
#'                         
#' @param ... Further parameters sent to \code{\link{hierarchies_as_vars}} 
#'
#' @return Input `data` with extra Variables
#' @export
#'
#' @examples
#' 
#' # Examples similar those from hierarchies_as_vars
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' year_formula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
#' geo_dim_list <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' age_hierarchy <- SSBtoolsData("sprt_emp_ageHier")
#' 
#' map_hierarchies_to_data(z, list(age = age_hierarchy, geo = geo_dim_list, 
#'                                 year = year_formula))
#' 
#' map_hierarchies_to_data(data.frame(f = c("A", "B", "C", "D", "E", "A")), list(f = 
#'        c("AB = A + B", "AC = A + C", "CD = C + D", "ABCD = AB + CD")))
#'        
map_hierarchies_to_data <- function(data, hierarchies, when_overwritten = warning, ...){
  a <- hierarchies_as_vars(hierarchies, ...)
  for(i in seq_along(a)){
    a[[i]] = map_var_hierarchy(a[[i]], data[[names(a[i])]])
  }
  names(a) <- NULL
  a_names <- unlist(lapply(a, names))
  if (any(duplicated(a_names))) {
    stop("Duplicate generated variable names not allowed")
  }
  a_names_in_data <- which(names(data) %in% a_names)
  if (length(a_names_in_data)) {
    if (!is.null(when_overwritten)) {
      message_text <- paste("Overwritten columns:", 
          paste(names(data)[a_names_in_data], collapse = ", "))
      when_overwritten(message_text)
    }
    data <- data[-a_names_in_data]
  }
  do.call(cbind, c(list(data), a))
}


map_var_hierarchy <- function(var_hierarchy, y){
  z <- var_hierarchy[match(y, var_hierarchy[[1]]), -1,drop = FALSE]
  rownames(z) <- NULL
  z
}

