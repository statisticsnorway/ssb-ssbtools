#' Aggregate by base R or data.table
#'
#' This function aggregates data by specified grouping variables, using either base R or `data.table`.
#'
#' @param data A data frame
#' @param by A character vector specifying the column names to group by.
#' @param var A character vector specifying the column names of the variables to be aggregated.
#' @param pkg A character string indicating which package to use for aggregation. 
#' Must be either `"base"` for base R or `"data.table"` for `data.table`. Default is `"base"`.
#' @param include_na A logical value indicating whether `NA` values in the grouping variables should be included in the aggregation. Default is `FALSE`.
#' @param fun The function to be applied for aggregation. Default is `sum`.
#' @param base_order A logical value indicating whether to return the results in the same order as base R when using `data.table`. Default is `TRUE`.
#'
#' @return A data.frame containing the aggregated results.
#'
#' @export
#' 
#' @examples
#' library(data.table)
#' d <- SSBtoolsData("d2")[1:20, ]
#' d[[2]] <- as.numeric(d[[2]])
#' d$y <- as.numeric(1:20)
#' d$y[2] <- NA
#' d$county[8:9] <- NA
#' d$main_income[11:12] <- NA
#' d$k_group[19:20] <- NA
#' by <- c("main_income", "county", "k_group")
#' a1 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"))
#' a2 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), pkg = "data.table")
#' a3 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), 
#'                        include_na = TRUE)
#' a4 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), pkg = "data.table", 
#'                        include_na = TRUE)
#' a5 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), 
#'                        include_na = TRUE, fun = function(x) list(x))
#' a6 <- aggregate_by_pkg(d, by = by, var = c("y", "freq"), pkg = "data.table", 
#'                        include_na = TRUE, fun = function(x) list(x))
#'                        
#' identical(a1, a2)
#' identical(a3, a4)
#' identical(a5, a6)
#'                         
aggregate_by_pkg <- function(data, by, var, pkg = "base", include_na = FALSE, fun = sum, base_order = TRUE) {
  if (pkg == "base") {
    
    na_included <- rep(FALSE, length(by))
    
    if (include_na) {
      for (i in seq_along(by)) {
        if (anyNA(data[[by[i]]])) {
          na_included[i] <- TRUE
          if (is.integer(data[[by[i]]])) {
            # Replace NA values in integer columns with a very large integer value
            # This value is set close to the maximum integer value that R can handle.
            data[[by[i]]][is.na(data[[by[i]]])] <- .Machine$integer.max - 3L
          } else if (is.numeric(data[[by[i]]])) {
            # Replace NA values in numeric columns with a very large numeric value
            # This value is set close to the maximum numeric value that R can handle.
            data[[by[i]]][is.na(data[[by[i]]])] <- 1.789e+308
          } else {
            if (is.factor(data[[by[i]]]) | is.logical(data[[by[i]]])) {
              # Convert factor or logical columns to character type to handle NA replacement
              data[[by[i]]] <- as.character(data[[by[i]]])
              warning(paste(names(data)[i], "changed from", class(data[[by[i]]]), "to character"))
            }
            # Replace NA values in character (or converted factor/logical) columns with a string
            # This string is chosen so that it will likely be sorted at the end.
            data[[by[i]]][is.na(data[[by[i]]])] <- "~~~~~~M"
          }
        }
      }
    }
    
    result <- aggregate(data[var], data[by], fun)
    
    if (any(na_included)) {
      for (i in seq_along(by)) {
        if (na_included[i]) {
          if (is.integer(result[[by[i]]])) {
            # Revert the large integer value back to NA after aggregation
            result[[by[i]]][result[[by[i]]] == (.Machine$integer.max - 3L)] <- NA
          } else if (is.numeric(result[[by[i]]])) {
            # Revert the large numeric value back to NA after aggregation
            result[[by[i]]][result[[by[i]]] == 1.789e+308] <- NA
          } else {
            # Revert the placeholder string back to NA after aggregation
            result[[by[i]]][result[[by[i]]] == "~~~~~~M"] <- NA
          }
        }
      }
    }
    
    return(result)
  }
  
  if (pkg == "data.table") {
    dt <- as.data.table(data)
    if (!include_na) {
      # Remove rows where any of the by-columns have NA if include_na is FALSE
      dt <- dt[complete.cases(dt[, ..by]), ]
    }
    # Perform aggregation using data.table, grouping by the specified columns
    dt <- dt[, lapply(.SD, fun), by = by, .SDcols = var]
    
    if (base_order) {
      # Optional sorting to mimic base R order
      sort_order <- do.call(order, dt[, rev(by), with = FALSE])
      dt <- dt[sort_order]
    }
    
    return(as.data.frame(dt))
  }
  
  stop('pkg must be "base" or "data.table"')
}



