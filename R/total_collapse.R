
#' Collapse variables to single representation
#'
#' Simplify a data frame by collapsing specified variables, 
#' according to the location of total codes, 
#' into a single vector or by consolidating groups of variables into new columns.
#'
#' @param data A data frame containing the variables to be collapsed.
#' @param variables A vector of variable names or a named list of variable names.
#'  *  If `variables` is a vector, the specified variables in `data` are collapsed 
#'        into a single character vector.
#'  *  If `variables` is a named list, each element in the list defines a group of 
#'        variables to consolidate into a new column. 
#'        Each list name will be used as the new column name in the resulting data frame.
#' @param total A total code or vector of total codes to use in the result. 
#'  *  If `variables` is a vector, `total` specifies the code to represent collapsed values. 
#'  *  If `variables` is a named list, `total` may contain one code per group.
#' @param include_names A character string or `NULL` (default). 
#'  *  If `variables` is a vector, whether the resulting output vector is named depends 
#'       on whether `include_names` is `NULL` or not. The actual value of `include_names` 
#'        is ignored in this case. 
#'  * If `variables` is a named list, `include_names` specifies a suffix to append to 
#'        each group name, creating one additional column per group. 
#'        If `NULL`, no additional columns with variable names are included in the result.
#'
#' @return A character vector (if `variables` is a vector) or a modified data frame (if `variables` is a named list).
#' 
#' @export
#'
#' @examples
#' 
#' # Creates data that can act as input
#' magnitude1 <- SSBtoolsData("magnitude1")
#' a <- model_aggregate(magnitude1, 
#'                      formula = ~geo + eu + sector2 + sector4, 
#'                      sum_vars = "value", 
#'                      avoid_hierarchical = TRUE)
#' a
#' 
#' b <- total_collapse(a, list(GEO = c("geo", "eu"), SECTOR = c("sector2", "sector4")))
#' b
#' 
#' total_collapse(a, c("geo", "eu"))
#' total_collapse(a, c("sector2", "sector4"))                                 
#' 
#' 
#' # Similar examples with both `total` and `include_names` parameters
#' aa <- a
#' aa[1:2][aa[1:2] == "Total"] <- "Europe"
#' aa[3:4][aa[3:4] == "Total"] <- ""
#' aa
#' 
#' bb <- total_collapse(data = aa, 
#'                      variables = list(GEO = c("geo", "eu"), 
#'                                       SECTOR = c("sector2", "sector4")), 
#'                      total = c("Europe", ""),
#'                      include_names = "_Vars")
#' bb
#' 
#' total_collapse(aa, c("geo", "eu"), total = "Europe", include_names = "_Vars")
#' total_collapse(aa, c("sector2", "sector4"), total = "", include_names = "_Vars") 
#' 
#' 
#' # All four variables can be collapsed
#' total_collapse(a, 
#'                list(ALL = c("geo", "eu", "sector2", "sector4")), 
#'                include_names = "_Vars")
#' 
total_collapse <- function(data, variables, total = "Total", include_names = NULL) {
  if(!is.list(variables)) {
    return(total_collapse_var(data[variables], total = total, include_names = include_names))
  }
  if(!(length(total) %in% c(1, length(variables)))) {
    stop("wrong length of the total parameter")
  }
  total <- rep_len(total, length(variables))
  startRow <- attr(data, "startRow")
  for (i in seq_along(variables)) {
    data <- total_collapse_1(data, variables[[i]], names(variables)[i], total = total[i], include_names = include_names)
  }
  attr(data, "startRow") <- startRow
  data
}

total_collapse_1 <- function(data, var_names, new_name, total, include_names) {
  var_ind <- match(var_names, names(data))
  new_var <- total_collapse_var(data[var_names], total = total, include_names = include_names)
  data[[var_ind[1]]] <- new_var
  names(data)[var_ind[1]] <- new_name
  one_or_two <- 1 + as.integer(!is.null(include_names))
  if (length(var_ind) > one_or_two) {
    data <- data[-(var_ind[-seq_len(one_or_two)])]
  }
  if (!is.null(include_names)) {
    if (length(var_names) == 1) {
      idx <- ncol(data) + 1
    } else {
      idx <- var_ind[2]
    }
    data[[idx]] <- names(new_var)
    names(data)[idx] <- paste0(new_name, include_names)
  }
  data
}

total_collapse_var <- function(data, total, include_names) {
  q <- data != total
  if (max(rowSums(q)) > 1) {
    stop("Not single non-total-variable")
  }
  z <- data[, 1]
  q[!q] <- NA
  w <- WhereFirst(q)
  rows <- which(!is.na(w))
  z[rows] <- data[cbind(rows, w[rows])]
  if(!is.null(include_names)) {
    z_names <- rep(names(data)[1], nrow(data))
    z_names[rows] <- names(data)[w[rows]]
    names(z) <- z_names
  }
  z
}

# Copy from https://github.com/statisticsnorway/ssb-kostra
MinPos = function(x){
  z=min(c(x[x>0],Inf))
  if(!is.finite(z)) z=NA
  z
}

# Copy from https://github.com/statisticsnorway/ssb-kostra
WhereFirst =  function(x){
  x = as.matrix(x)
  apply(col(x)* (is.finite(x)),1,MinPos)
}