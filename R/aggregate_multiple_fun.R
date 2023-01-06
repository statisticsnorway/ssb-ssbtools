                       
#' Wrapper to `aggregate` 
#' 
#' Wrapper to \code{\link{aggregate}} that allows multiple functions and functions of several variables 
#' 
#' A limitation is that the `...` parameters are not forwarded to the supplied functions.
#' When extra parameters are needed, supply instead wrapper functions where those parameters are fixed.
#'
#' @param data A data frame containing data to be aggregated 
#' @param by A data frame defining grouping
#' @param fun A named list of functions. These names will be used as suffixes in output variable names. Name can be omitted for one function. 
#'            A vector of function as strings is also possible. When unnamed, these function names will be used directly. 
#'            See the examples of \code{\link{fix_fun_amf}}, which is the function used to convert `fun`.
#' @param vars  A named vector or list of variable names in `data`. The elements are named by the names of `fun`.
#'              All the pairs of variable names and function names thus define all the result variables to be generated.
#'              Parameter `vars` will converted to an internal standard by the function \code{\link{fix_vars_amf}}. 
#'              Thus, function names and also output variable names can be coded in different ways.
#'              Multiple output variable names can be coded using `multi_sep`. 
#'              See examples and examples in \code{\link{fix_vars_amf}}.
#'              Indices instead of variable names are allowed. 
#'              
#' @param ind When non-NULL, a data frame of indices. 
#'            When NULL, this variable will be generated internally as `data.frame(ind = seq_len(nrow(data)))`. 
#'            The parameter is useful for advanced use involving model/dummy matrices.  
#'            
#'                
#' @param ... 	Further arguments passed to `aggregate`
#' @param name_sep  A character string used when output variable names are generated. 
#' @param seve_sep  A character string used when output variable names are generated from functions of several variables. 
#' @param multi_sep A character string used when multiple output variable names are sent as input. 
#'
#' @return A data frame
#' @export
#' @importFrom stats aggregate
#' 
#' @note Note to developers: If `...` is to be handled (see details), this is probably best done by wrapper functions being generated at the start 
#'       and not by `...` being sent all the way through. This leads to many issues that must be dealt with, 
#'       there can be time-consuming overhead in the calculations and `R.utils::doCall` is no solution.
#'
#' @examples
#' z2 <- SSBtoolsData("z2")
#' set.seed(12)
#' z2$y <- round(rnorm(nrow(z2)), 2)
#' z <- z2[sample.int(nrow(z2), size = 20), ]
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    fun = c(sum, median = median, d1 = function(x) x[1]),    
#'    vars = c("ant", "y", median = "ant", median = "y", d1 = "ant")
#' )
#' 
#' # With functions as strings 
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    fun = c("sum", "median"),    
#'    vars = c(sum = "y", median = "ant", median = "y")
#' )
#' 
#' # with multiple outputs (function my_range)
#' # and with function of two variables (weighted.mean(y, ant))
#' my_range <- function(x) c(min = min(x), max = max(x))
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    fun = c(sum, ra = my_range, wmean = weighted.mean),    
#'    vars = list("ant", "y", ra = "ant", wmean  = c("y", "ant"))
#' )
#' 
#' # with specified output variable names
#' my_range <- function(x) c(min = min(x), max = max(x))
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    fun = c(sum, ra = my_range, wmean = weighted.mean),    
#'    vars = list("ant", "y", 
#'                `antmin,antmax` = list(ra = "ant"), 
#'                 yWmean  = list(wmean  = c("y", "ant")))
#' )
#' 
#' 
#' 
#'  
aggregate_multiple_fun <- function(data, by, fun, vars, ind = NULL, ..., name_sep = "_", seve_sep = ":", multi_sep = ",") {
  
  if(is.null(ind)){
    ind = data.frame(ind = seq_len(nrow(data)))
  }
  
  names(ind) = "i7N9Qd3"
  
  
  fun <- fix_fun_amf(fun)

  vars <- fix_vars_amf(vars, name_sep = name_sep,  seve_sep = seve_sep, multi_sep = multi_sep, names_data = names(data))
  
  
  output_names <- sapply(vars, function(x) x[[1]] )
  fun_names <- sapply(vars, function(x) x[[2]] )
  vars <- lapply(vars, function(x) x[-(1:2)] )
  
  if (is.null(names(fun))) {
    names(fun) <- ""
  }
  
  names(fun)[is.na(names(fun))] <- ""
  if (anyDuplicated(names(fun))) {
    stop("fun must be uniquely named")
  }
  
  
  if (!all(unlist(vars) %in% names(data))) {
    stop("All vars must be in names(x)")
  }
  if (!all(fun_names %in% names(fun))) {
    stop("All fun names in vars must be in names(fun)")
  }
  if (!all(names(fun) %in% fun_names)) {
    warning("Not all fun elements will be used")
    fun <- fun[names(fun) %in% fun_names]
  }
  
  
  data1 = data[1, ]
  for(i in seq_len(ncol(data1))){
    d1 = unlist(data1[1,i])[1]
    if(length(d1)==1)
      data1[,i] = d1
  }  
  
  
  fun_all <- function(ind, fun_input, data, vars, output_names, fun_names, data1 = NULL, fun_all_0 = NULL){
    if(length(ind)==1)
      if(ind==0)
        if(!is.null(fun_all_0)){
          return(fun_all_0)
        } else {
          data = data1
        }
        
    out <- vector("list", length(vars))
    for(i in seq_along(out)){
      j <- match(fun_names[i], names(fun_input)) 
      if(length(vars[[i]]) == 1){
        out[[i]] <- fun_input[[j]](unlist(data[[vars[[i]]]][ind]))
        if (names(fun)[j] == "") {
          names(out)[i] <- vars[[i]]
        } else {
          names(out)[i] <- paste(vars[[i]], names(fun)[j], sep = name_sep)
        }
      } else {
        if(length(vars[[i]]) == 2)   out[[i]] <- fun_input[[j]]( unlist(data[[vars[[i]][1]]][ind]), unlist(data[[vars[[i]][2]]][ind]))
        if(length(vars[[i]]) == 3)   out[[i]] <- fun_input[[j]]( unlist(data[[vars[[i]][1]]][ind]), unlist(data[[vars[[i]][2]]][ind]), unlist(data[[vars[[i]][3]]][ind]))
        if(length(vars[[i]]) == 4)   out[[i]] <- fun_input[[j]]( unlist(data[[vars[[i]][1]]][ind]), unlist(data[[vars[[i]][2]]][ind]), unlist(data[[vars[[i]][3]]][ind]), unlist(data[[vars[[i]][4]]][ind]))
        if(length(vars[[i]]) > 4){
          stop("Not implemented: length(vars[[i]]) > 4")
        }
      }
        
        easy_name = FALSE
        
        if(is.null(multi_sep)){
          easy_name = TRUE
        } else {
          if(!grepl(multi_sep, output_names[i])){
            easy_name = TRUE
          } else {
            split_names= strsplit(output_names[i], multi_sep, fixed = TRUE)[[1]]
            if(length(split_names) != length(out[[i]])){
              easy_name = TRUE
              warning("Wrong number of strings after multi_sep splitting")
            } else {
              names(out[[i]]) = split_names
              names(out)[i] = ""
            }
          }
        }
        if(easy_name){
          names(out)[i] = output_names[i]
        }
      
    }
    unlist(out) 
  }
  

  if(min(ind[[1]]) == 0){
    fun_all_0 = fun_all(ind = 0L, vars = vars, output_names = output_names, fun_names = fun_names, fun_input = fun, data = data, data1 = data1) 
  } else {
    fun_all_0 = NULL # not needed 
  }
  
  z <- aggregate(x = ind, by = by, FUN = fun_all, vars = vars, output_names = output_names, fun_names = fun_names, fun_input = fun, data = data, fun_all_0 = fun_all_0, ...)
  
  #Transform  embedded matrix
  z <- unmatrix(z, sep = name_sep)
  names(z) <- sub(paste0(names(ind), name_sep), "", colnames(z))
  
  # Fix name when not embedded matrix
  grepind <- grep(names(ind), names(z))
  if(length(grepind)==1){
    if(length(fun)==1 & length(vars)==1){
      names(z)[grepind]  =  output_names[1]
    } else {
      names(z)[grepind] <- "output_from_fun"
      warning("Unusual output")  
    }
  }
  if(length(grepind)>1){
    warning("Output is strange")
  }
  
  z

}




#' Fix `vars` parameter to `aggregate_multiple_fun`
#'
#' @param vars vars
#' @inheritParams aggregate_multiple_fun
#' @param names_data `names(data)` to convert numeric input (indices)
#'
#' @return vars
#' @export
#' 
#' @keywords internal
#'
#' @examples
#' f <- fix_vars_amf
#' 
#' f(c("ant", "y", median = "ant", median = "y", d1 = "ant"))
#' 
#' v1 <- list(sum = "a", sum = "w", q = c("a", "w"), snitt = c("b", "w"))
#' v2 <- list(c(fun = "sum", "a"), c(fun = "sum", "w"), c(fun = "q", "a", "w"), 
#'            c(fun = "snitt", "b", "w"))
#' v3 <- list(sum = "a", sum = "w", q = c(name = "a:w_q", "a", "w"), 
#'            `b:w_snitt` = list(snitt = c("b", "w")))
#' v4 <- list(c(name = "a_sum", fun = "sum", "a"), 
#'            c(name = "w_sum", fun = "sum", "w"), 
#'            c(name = "a:w_q", fun = "q", "a", "w"), 
#'            c(name = "b:w_snitt", fun = "snitt", "b", "w"))
#' v5 <- list(a_sum = c(fun = "sum", "a"), 
#'            w_sum = c(fun = "sum", "w"), 
#'            `a:w_q` = c(fun = "q", "a", "w"), 
#'            `b:w_snitt` = c(fun = "snitt", "b", "w"))
#' 
#' identical(f(v1), f(v2))
#' identical(f(v1), f(v3))
#' identical(f(v1), f(v4))
#' identical(f(v1), f(v5))
#' 
#' identical(f(v1), f(f(v1)))
#' identical(f(v1), v4)
fix_vars_amf  = function(vars, name_sep = "_", seve_sep = ":", multi_sep = ",", names_data = NULL){
  if (is.null(vars)) {
    stop("non-NULL vars needed")
  }
  vars <- as.list(vars)
  for(i in seq_along(vars)){
    vars[[i]] = fi_amf(vars[i], name_sep = name_sep,  seve_sep = seve_sep, multi_sep = multi_sep,  names_data = names_data)
  }
  names(vars) <- NULL
  vars
}


fi_amf = function(vars_i, name_sep, seve_sep, multi_sep, names_data){
  names_i  = c(names(vars_i), "")[1]
  if(is.na(names_i)){
    names_i <- ""
  }
  vars_i = vars_i[[1]]
  if(is.list(vars_i)){
    if(length(vars_i) > 1){
      stop("inner list must be of length 1")
    }
    names_i2  = c(names(vars_i), "")[1]
    if(is.na(names_i)){
      names_i <- ""
    }
    if(names_i == ""){
      stop("name needed when list in list")
    }
    vars_i = vars_i[[1]]
    if (is.numeric(vars_i)) {
      vars_i <- names_data[vars_i]
    }
    vars_i = c(name = names_i, fun = names_i2, vars_i) 
  } else {
    if (is.numeric(vars_i)) {
      vars_i <- names_data[vars_i]
    }
    addname = TRUE
    if("fun" %in%  names(vars_i)){
      if(names_i != ""){
        vars_i = c(name = names_i, vars_i)
        addname = FALSE
      }
    } else {
      vars_i = c(fun = names_i, vars_i)
    }
    if("name" %in%  names(vars_i)){
      addname = FALSE
    }
    if(addname){
      fun_name = vars_i[["fun"]]
      vars_i_ = vars_i[names(vars_i) != "fun"]
      if (fun_name == "") {
        name <- paste(vars_i_, collapse = seve_sep)
      } else {
        name <- paste(paste(vars_i_, collapse = seve_sep), fun_name, sep = name_sep)
      }
      vars_i = c(name = name, vars_i)
    }
  }
  n_fun  = sum(names(vars_i) == "fun")
  n_name = sum(names(vars_i) == "name")
  if(n_fun > 1){
    stop("Multiple function names found")
  }
  if(n_fun < 1){
    stop("function names: something is wrong")
  }
  if(n_name > 1){
    stop("Multiple output names found")
  }
  if(n_name < 1){
    stop("Output names: something is wrong")
  }
  c(name = trim(vars_i[["name"]], multi_sep), fun = vars_i[["fun"]],  vars_i[!(names(vars_i) %in% c("fun", "name"))])
}  

# Remove leading/trailing whitespace
trim <- function(name, multi_sep){
  paste(trimws(strsplit(name, multi_sep, fixed = TRUE)[[1]]), collapse = multi_sep)
}




#' Transform data frame with embedded matrices 
#'
#' @param data data frame 
#' @param sep A character string used when variable names are generated.
#'
#' @return data frame 
#' @export
#' 
#' @keywords internal
#'
unmatrix <- function(data, sep = "_") {
  if (!is.data.frame(data)) {
    stop("data must be data.frame")
  }
  j <- match(TRUE, sapply(data, is.matrix))
  if (is.na(j)) {
    return(data)
  }
  name_j <- names(data)[j]
  data_j <- as.data.frame(data[[j]])
  if (is.null(colnames(data[[j]]))) {
    n_j <- as.character(seq_len(ncol(data_j)))
  } else {
    n_j <- names(data_j)
  }
  names(data_j) <- paste(name_j, n_j, sep = sep)
  
  data <- cbind(data[SeqInc(1, j - 1)], data_j, data[SeqInc(j + 1, ncol(data))])
  unmatrix(data, sep = sep)
}



#' Fix `fun` parameter to `aggregate_multiple_fun`
#'
#' @param fun fun
#'
#' @return fun
#' @export
#' 
#' @keywords internal
#'
#' @examples
#' identical(fix_fun_amf("median"), c(median = median))
#' 
#' identical(fix_fun_amf(c("sum", "median")), c(sum = sum, median = median))
#' 
#' ff <- c("sum", "median", "cor")
#' names(ff) <- c("", NA, "Correlation")
#' identical(fix_fun_amf(ff), c(sum, median = median, Correlation = cor))
#' 
#' identical(fix_fun_amf(structure("median", names = "")), fix_fun_amf(median))
fix_fun_amf <- function(fun) {
  if (is.function(fun)) {
    fun <- c(fun)  # This is a list
    names(fun) <- ""
  }
  if (is.character(fun)) {
    fun <- as.list(fun)
  }
  if (is.null(names(fun))) {
    names(fun) <- NA
  }
  for (i in seq_along(fun)) {
    if (is.character(fun[[i]])) {
      fun_i <- fun[[i]]
      fun[[i]] <- get(fun[[i]])
      if (is.na(names(fun)[i])) {
        names(fun)[i] <- fun_i
      }
    } else {
      if (is.na(names(fun)[i])) {
        names(fun)[i] <- ""
      }
    }
  }
  
  fun
}


















