

#' model_aggregate
#'
#' @param data data
#' @param sum_vars sum_vars 
#' @param fun_vars fun_vars 
#' @param fun fun
#' @param hierarchies hierarchies 
#' @param formula formula 
#' @param dimVar dimVar 
#' @param charVar charVar 
#' @param pre_aggregate pre_aggregate 
#' @param verbose verbose 
#' @param ... dots
#'
#' @return data frame
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
  dimVar = NULL,
  charVar = NULL,
  pre_aggregate = TRUE,
  verbose = TRUE, ...) {
  
  sum_vars <- var_names(sum_vars, data)
  fun_vars <- var_names(fun_vars, data)  ####### This is preliminary  ############
  dimVar <- var_names(dimVar, data)
  charVar <- var_names(charVar, data)
  dVar <- unique(NamesFromModelMatrixInput(hierarchies = hierarchies, formula = formula, dimVar = dimVar))
  if (!length(dVar)) {
    stop("hierarchies, formula, or dimVar needed ")
  }
  
  if (is.null(names(sum_vars))) {
    names(sum_vars) <- ""
  }
  names(sum_vars)[is.na(names(sum_vars))] <- ""
  if (is.null(names(fun_vars))) {
    names(fun_vars) <- ""
  }
  names(fun_vars)[is.na(names(fun_vars))] <- ""
  
  if (anyDuplicated(c(sum_vars, fun_vars)[names(c(sum_vars, fun_vars)) == ""])) { ####### This is preliminary ############
    stop("Any duplicates in c(sum_vars, fun_vars) must be uniquely named (name can be omitted for one element).")
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
    data <- aggregate(data[unique(unlist(fun_vars))], data[unique(c(dVar, charVar))], function(x) x, simplify = FALSE)
    if (verbose) {
      cat(">")
      flush.console()
    }
    sum_data <- aggregate(sum_data[unique(sum_vars)], sum_data[unique(c(dVar, charVar))], sum, simplify = TRUE)
    if (verbose) {
      cat(dim(data)[1])
      flush.console()
    }
    if (!identical(data[unique(c(dVar, charVar))], sum_data[unique(c(dVar, charVar))])) {
      stop("Check failed")
    }
    if (verbose) {
      cat("*")
      flush.console()
    }
    sum_data <- as.matrix(sum_data[unique(sum_vars)])
    if (verbose) {
      cat(dim(data)[2], "] ", sep = "")
      flush.console()
    }
  }
  
  
  if (verbose) {
    cat("[ModelMatrix")
    flush.console()
  }
  mm <- ModelMatrix(data, hierarchies = hierarchies, formula = formula, dimVar = dimVar, crossTable = TRUE)
  if (verbose) {
    cat("] ")
    flush.console()
  }
  
  
  if (verbose) {
    cat("[crossprod")
    flush.console()
  }
  
  
  if (pre_aggregate) {
    sum_data <- as.data.frame(as.matrix(crossprod(mm$modelMatrix, sum_data)))
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


  
  