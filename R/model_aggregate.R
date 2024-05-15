

#' Hierarchical aggregation via model specification
#' 
#' Internally a dummy/model matrix is created according to the model specification. 
#' This model matrix is used in the aggregation process via matrix multiplication and/or the function \code{\link{aggregate_multiple_fun}}.
#' 
#' With formula input, limited output can be achieved by \code{\link{formula_selection}} (see example). 
#' An attribute called `startCol` has been added to the output data frame to make this functionality work.
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
#' @param remove_empty  When non-NULL, the `removeEmpty` parameter to \code{\link{ModelMatrix}}.
#'                    Thus, the actual default value is `TRUE` with formula input without hierarchy and 
#'                    otherwise `FALSE` (see \code{\link{ModelMatrix}}).
#' @param preagg_var  Extra variables to be used as grouping elements in the pre-aggregate step
#' @param dummy       The `dummy`       parameter to \code{\link{dummy_aggregate}}.
#'                    When `TRUE`, only 0s and 1s are assumed in the generated model matrix. 
#'                    When `FALSE`, non-0s in this matrix are passed as an additional first input parameter to the `fun` functions. 
#' @param pre_aggregate Whether to pre-aggregate data to reduce the dimension of the model matrix. 
#'                    Note that all original `fun_vars` observations are retained in the aggregated dataset and `pre_aggregate` does not affect the final result.
#'                    However, `pre_aggregate` must be set to `FALSE` when the `dummy_aggregate` parameter `dummy` is set to `FALSE` 
#'                    since then \code{\link{unlist}} will not be run. 
#'                    An exception to this is if the `fun` functions are written to handle list data. 
#' @param list_return Whether to return a list of separate components including the model matrix `x`.
#' @param pre_return  Whether to return the pre-aggregate data as a two-component list. Can also be combined with `list_return` (see examples).
#' 
#' @param verbose     Whether to print information during calculations. 
#' @param mm_args     List of further arguments passed to `ModelMatrix`.
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
#' 
#' # Limited output can be achieved by formula_selection
#' formula_selection(out, ~geo)
#' 
#' 
#' # Using the single unnamed variable feature.
#' model_aggregate(z, formula = ~age, fun_vars = "y", 
#'                 fun = c(sum = sum, mean = mean, med = median, n = length))
#' 
#' 
#' # To illustrate list_return and pre_return 
#' for (pre_return in c(FALSE, TRUE)) for (list_return in c(FALSE, TRUE)) {
#'   cat("\n=======================================\n")
#'   cat("list_return =", list_return, ", pre_return =", pre_return, "\n\n")
#'   out <- model_aggregate(z, formula = ~age:year, 
#'                          sum_vars = c("ths", "y"), 
#'                          fun_vars = c(mean = "y", ra = "y"), 
#'                          fun = c(mean = mean, ra = my_range), 
#'                          list_return = list_return,
#'                          pre_return = pre_return)
#'   cat("\n")
#'   print(out)
#' }
#'
#'
#' # To illustrate preagg_var 
#' model_aggregate(z, formula = ~age:year, 
#' sum_vars = c("ths", "y"), 
#' fun_vars = c(mean = "y", ra = "y"), 
#' fun = c(mean = mean, ra = my_range), 
#' preagg_var = "eu",
#' pre_return = TRUE)[["pre_data"]]
#' 
#' 
#' # To illustrate hierarchies 
#' geo_hier <- SSBtoolsData("sprt_emp_geoHier")
#' model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier), 
#'                 sum_vars = "y", 
#'                 fun_vars = c(sum = "y"))
#' 
#' ####  Special non-dummy cases illustrated below  ####
#' 
#' # Extend the hierarchy to make non-dummy model matrix  
#' geo_hier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), 
#'                               mapsTo = "EUandSpain", sign = 1), geo_hier[, -4])
#' 
#' # Warning since non-dummy
#' # y and y_sum are different 
#' model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier2), 
#'                 sum_vars = "y", 
#'                 fun_vars = c(sum = "y"))
#' 
#' # No warning since dummy since unionComplement = TRUE (see ?HierarchyCompute)
#' # y and y_sum are equal   
#' model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier2), 
#'                 sum_vars = "y", 
#'                 fun_vars = c(sum = "y"),
#'                 mm_args = list(unionComplement = TRUE))
#' 
#' # Non-dummy again, but no warning since dummy = FALSE
#' # Then pre_aggregate is by default set to FALSE (error when TRUE) 
#' # fun with extra argument needed (see ?dummy_aggregate)
#' # y and y_sum2 are equal
#' model_aggregate(z, hierarchies = list(age = "All", geo = geo_hier2), 
#'                 sum_vars = "y", 
#'                 fun_vars = c(sum2 = "y"),
#'                 fun = c(sum2 = function(x, y) sum(x * y)),
#'                 dummy = FALSE) 
#'                 
model_aggregate = function(
  data,
  sum_vars = NULL,
  fun_vars = NULL, 
  fun = NULL, 
  hierarchies = NULL,
  formula = NULL,
  dim_var = NULL,
  remove_empty = NULL,
  preagg_var = NULL,
  dummy = TRUE,
  pre_aggregate = dummy,
  list_return = FALSE,
  pre_return = FALSE,
  verbose = TRUE,
  mm_args = NULL, ...) {
  
  if (!length(sum_vars)) {
    sum_vars <- NULL
  }
  if (!is.null(sum_vars)) {
    sum_vars <- var_names(sum_vars, data)
  }
  
  if (!length(fun_vars)) {
    fun_vars <- NULL
  }
  
  if (is.null(fun_vars) & is.null(sum_vars)) {
    stop("sum_vars and/or fun_vars must be specified")
  }
  
  if (!is.null(fun_vars)) {
    vars <- fix_vars_amf(fun_vars, ..., names_data = names(data))
    fun_names <- sapply(vars, function(x) x[[2]] )
    vars_3 <- sapply(vars, function(x) x[[3]] )
    vars_length <-  sapply(vars, length)
    fun_vars_noname <- vars_3[vars_length == 3 & fun_names == ""] 
    vars <- lapply(vars, function(x) x[-(1:2)] )
    unique_fun_vars <- unique(unlist(vars)) 
  } else {
    fun_vars_noname <- NULL
  }
  
  dim_var <- var_names(dim_var, data)
  preagg_var <- var_names(preagg_var, data)
  d_var <- unique(NamesFromModelMatrixInput(hierarchies = hierarchies, formula = formula, dimVar = dim_var))
  if (!length(d_var)) {
    stop("hierarchies, formula, or dim_var needed ")
  }
  
  if (anyDuplicated(c(sum_vars, fun_vars_noname))) { 
    stop("Unnamed fun_vars also found in sum_vars (name can be omitted for one element not in sum_vars).")
  }
  
  
  if (pre_aggregate) {
    if (verbose) {
      cat("[pre_aggregate ", dim(data)[1], "*", dim(data)[2], sep = "")
      flush.console()
    }
    if (!is.null(sum_vars)) {
      sum_data <- data  # input_data
    } else {
      sum_data <- NULL
    }
    if (verbose) {
      cat("-")
      flush.console()
    }
    if (!is.null(fun_vars)) {
      data <- aggregate(data[unique_fun_vars], data[unique(c(d_var, preagg_var))], function(x) x, simplify = FALSE)
    }
    if (verbose) {
      cat(">")
      flush.console()
    }
    if (!is.null(sum_vars)) {
      sum_data <- aggregate(sum_data[unique(sum_vars)], sum_data[unique(c(d_var, preagg_var))], sum, simplify = TRUE)
    }
    if (is.null(fun_vars)) {
      data <- sum_data[unique(c(d_var, preagg_var))]
    }
    if (verbose) {
      cat(dim(data)[1])
      flush.console()
    }
    if (!is.null(sum_vars) & !is.null(fun_vars)) {
      if (!identical(data[unique(c(d_var, preagg_var))], sum_data[unique(c(d_var, preagg_var))])) {
        stop("Check failed")
      }
    }
    if (verbose) {
      cat("*")
      flush.console()
    }
    sum_data <- sum_data[unique(sum_vars)]
    if (verbose) {
      cat(dim(data)[2] + c(dim(sum_data)[2], 0)[1], "] ", sep = "")   # trick for 0 when NULL
      flush.console()
    }
  } else {
    sum_data <- NULL
  }
  
  if (pre_return) {
    pre_sum <- sum_data
    if (!list_return) {
      if (verbose) {
        cat("\n")
        flush.console()
      }
      return(list(pre_data=data, pre_sum = pre_sum))
    }
  }
  
  
  if (verbose) {
    cat("[ModelMatrix")
    flush.console()
  }
  if (!is.null(remove_empty)) {
    mm_args <- c(mm_args, list(removeEmpty = remove_empty))
  }
  if (is.null(mm_args)) {
    mm <- ModelMatrix(data, hierarchies = hierarchies, formula = formula, dimVar = dim_var, crossTable = TRUE)
  } else {
    mm <- do.call(ModelMatrix, c(list(data = data, hierarchies = hierarchies, formula = formula, dimVar = dim_var, crossTable = TRUE), mm_args))
  }
  if (verbose) {
    cat("] ")
    flush.console()
  }
  
  
  if (!is.null(sum_vars)) {
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
  }
  
  
  if (!is.null(fun_vars)) {
    if (verbose) {
      if (hasArg("inc_progress")) {
        cat("\n")
      }
      cat("[dummy_aggregate")
      flush.console()
    }
    z <- dummy_aggregate(data = data, x = mm$modelMatrix, vars = fun_vars, 
                         fun = fun, dummy = dummy, keep_names = FALSE, ...)
    if (verbose) {
      cat("] ")
      flush.console()
    }
  } else {
    z <- NULL
  }
  
  if (list_return) {
    if (verbose) {
      cat("\n")
      flush.console()
    }
    out <- list(cross_table = as.data.frame(mm$crossTable), sum_data = sum_data, fun_data = z, x = mm$modelMatrix)
    if (pre_return) {
      out <- c(list(pre_data=data, pre_sum = pre_sum), out)
    }
    return(out)
  }
  
  if (verbose) {
    cat("[cbind")
    flush.console()
  }
  if (!is.null(sum_vars)) {
    if (!is.null(fun_vars)) {
      z <- cbind(as.data.frame(mm$crossTable), sum_data, z)
    } else {
      z <- cbind(as.data.frame(mm$crossTable), sum_data)
    }
  } else {
    z <- cbind(as.data.frame(mm$crossTable), z)
  }
  rownames(z) <- NULL
  startCol <- attr(mm$modelMatrix, "startCol", exact = TRUE)
  if (!is.null(startCol)) {
    attr(z, "startRow") <- startCol
  }
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


  
  