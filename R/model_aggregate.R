

#' Hierarchical aggregation via model specification
#' 
#' Internally a dummy/model matrix is created according to the model specification. 
#' This model matrix is used in the aggregation process via matrix multiplication and/or the function \code{\link{aggregate_multiple_fun}}.
#' 
#'
#' @param data A data frame containing data to be aggregated 
#' @param sum_vars Variables to be summed. This will be done via matrix multiplication. 
#' @param fun_vars Variables to be aggregated by supplied functions.  
#'      This will be done via \code{\link{aggregate_multiple_fun}} and \code{\link{dummy_aggregate}} and 
#'      `fun_vars` is specified as the parameter `vars`. 
#' @param fun         The `fun`         parameter to \code{\link{aggregate_multiple_fun}} 
#' @param hierarchies The `hierarchies` parameter to \code{\link{ModelMatrix}}
#' @param formula     The `formula`     parameter to \code{\link{ModelMatrix}} 
#' @param dim_var     The `dimVar`      parameter to \code{\link{ModelMatrix}}
#' @param char_var    Extra variables to be used as grouping elements in the pre-aggregate step 
#' @param pre_aggregate Whether to pre-aggregate data to reduce the dimension of the model matrix. 
#'                    Note that all original `fun_vars` observations are retained in the aggregated dataset and `pre_aggregate` does not affect the final result.
#' @param list_return Whether to return a list of separate components including the model matrix `x`.
#' @param pre_return Whether to return the pre-aggregate data as a two-component list. 
#' @param verbose     Whether to print information during calculations. 
#' @param ... Further arguments passed to `dummy_aggregate`.
#'
#' @return A data frame or a list. 
#' @export
#' @importFrom Matrix crossprod
#' @importFrom utils flush.console
#'
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' names(z)[names(z) == "ths_per"] <- "ths"
#' z$y <- 1:18
#' 
#' my_range <- function(x) c(min = min(x), max = max(x))
#' 
#' out <- model_aggregate(z, 
#'    formula = ~age:year + geo, 
#'    sum_vars = c("y", "ths"), 
#'    fun_vars = c(sum = "ths", mean = "y", med = "y", ra = "ths"), 
#'    fun = c(sum = sum, mean = mean, med = median, ra = my_range))
#' 
#' out
model_aggregate = function(
  data,
  sum_vars,
  fun_vars, 
  fun = sum, 
  hierarchies = NULL,
  formula = NULL,
  dim_var = NULL,
  char_var = NULL,
  pre_aggregate = TRUE,
  list_return = FALSE,
  pre_return = FALSE,
  verbose = TRUE, ...) {
  
  
  sum_vars <- var_names(sum_vars, data)
  if (is.null(names(sum_vars))) {
    names(sum_vars) <- ""
  }
  names(sum_vars)[is.na(names(sum_vars))] <- ""
  sum_vars_noname <- sum_vars[names(sum_vars) == ""]
  
  vars <- fix_vars_amf(fun_vars, ..., names_data = names(data))
  fun_names <- sapply(vars, function(x) x[[2]] )
  vars_3 <- sapply(vars, function(x) x[[3]] )
  vars_length <-  sapply(vars, length)
  fun_vars_noname <- vars_3[vars_length == 3 & fun_names == ""] 
  
  vars <- lapply(vars, function(x) x[-(1:2)] )
  unique_fun_vars <- unique(unlist(vars)) 
  
  dim_var <- var_names(dim_var, data)
  char_var <- var_names(char_var, data)
  d_var <- unique(NamesFromModelMatrixInput(hierarchies = hierarchies, formula = formula, dimVar = dim_var))
  if (!length(d_var)) {
    stop("hierarchies, formula, or dim_var needed ")
  }
  
  if (anyDuplicated(c(sum_vars_noname, fun_vars_noname))) { 
    stop("Any duplicates in (sum_vars, fun_vars) must be uniquely named (name can be omitted for one element).")
  }
  
  
  if (pre_aggregate) {
    if (verbose) {
      cat("[pre_aggregate ", dim(data)[1], "*", dim(data)[2], sep = "")
      flush.console()
    }
    sum_data <- data  # input_data 
    if (verbose) {
      cat("-")
      flush.console()
    }
    data <- aggregate(data[unique_fun_vars], data[unique(c(d_var, char_var))], function(x) x, simplify = FALSE)
    if (verbose) {
      cat(">")
      flush.console()
    }
    sum_data <- aggregate(sum_data[unique(sum_vars)], sum_data[unique(c(d_var, char_var))], sum, simplify = TRUE)
    if (verbose) {
      cat(dim(data)[1])
      flush.console()
    }
    if (!identical(data[unique(c(d_var, char_var))], sum_data[unique(c(d_var, char_var))])) {
      stop("Check failed")
    }
    if (verbose) {
      cat("*")
      flush.console()
    }
    sum_data <- sum_data[unique(sum_vars)]
    if (verbose) {
      cat(dim(data)[2] + dim(sum_data)[2], "] ", sep = "")
      flush.console()
    }
  } else {
    sum_data <- NULL
  }
  
  if (pre_return) {
    if (verbose) {
      cat("]\n")
      flush.console()
    }
    return(list(data=data, sum_data = sum_data))
  }
  
  
  if (verbose) {
    cat("[ModelMatrix")
    flush.console()
  }
  mm <- ModelMatrix(data, hierarchies = hierarchies, formula = formula, dimVar = dim_var, crossTable = TRUE)
  if (verbose) {
    cat("] ")
    flush.console()
  }
  
  
  if (verbose) {
    cat("[crossprod")
    flush.console()
  }
  
  
  if (pre_aggregate) {
    sum_data <- as.data.frame(as.matrix(crossprod(mm$modelMatrix, as.matrix(sum_data))))
  } else {
    sum_data <- as.data.frame(as.matrix(crossprod(mm$modelMatrix, as.matrix(data[unique(sum_vars)]))))
  }
  if (verbose) {
    cat("] ")
    flush.console()
  }
  
  if (verbose) {
    cat("[dummy_aggregate")
    flush.console()
  }
  
  z <- dummy_aggregate(data = data, x = mm$modelMatrix, fun = fun, vars = fun_vars, ...)
  if (verbose) {
    cat("] ")
    flush.console()
  }
  
  if (list_return) {
    if (verbose) {
      cat("]\n")
      flush.console()
    }
    return(list(cross_table = as.data.frame(mm$crossTable), sum_data = sum_data, fun_data = z, x = mm$modelMatrix))
  }
  
  if (verbose) {
    cat("[cbind")
    flush.console()
  }
  z <- cbind(as.data.frame(mm$crossTable), sum_data, z)
  rownames(z) <- NULL
  if (verbose) {
    cat("]\n")
    flush.console()
  }
  z
}


var_names <- function(vars, data) {
  if (is.list(vars)) {
    return(vars)
  }
  names_vars <- names(vars)
  unique_vars <- unique(vars)
  vars <- names(data[1, unique_vars, drop = FALSE])[match(vars, unique_vars)]
  names(vars) <- names_vars
  vars
}


  
  