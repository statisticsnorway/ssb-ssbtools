                       
#' Wrapper to `aggregate` 
#' 
#' Wrapper to \code{\link{aggregate}} that allows multiple functions and functions of several variables 
#'
#' @param data A data frame containing data to be aggregated 
#' @param by A data frame defining grouping
#' @param FUN A named list of functions. These names will be used as suffixes in output variable names. Name can be omitted for one function. 
#' @param vars  A named vector or list of variable names in `x`. The elements are named by the names of `FUN`.
#'              All the pairs of variable names and function names thus define all the result variables to be generated.
#'              Parameter `vars` will converted to an internal standard by the function \code{\link{fix_vars_amf}}. 
#'              Thus, function names and also output variable names can be coded in different ways.
#'              Multiple output variable names can be coded using `multi_sep`. 
#'              See examples and examples in \code{\link{fix_vars_amf}}.
#'              
#' @param ind When non-NULL, a data frame of indices. 
#'            When NULL, this variable will be generated internally as `data.frame(ind = seq_len(nrow(data)))`. 
#'            The parameter is useful for advanced use involving model/dummy matrices.  
#'            
#'                
#' @param ... 	Further arguments passed to `aggregate`
#' @param name_sep A character string used when output variable names are generated. 
#' @param multi_sep A character string used when output variable names are sent as input. 
#' @param print_inc Printing "..." to console when `TRUE` 
#'
#' @return A data frame
#' @export
#' @importFrom stats aggregate
#'
#' @examples
#' z2 <- SSBtoolsData("z2")
#' set.seed(12)
#' z2$y <- round(rnorm(nrow(z2)), 2)
#' z <- z2[sample.int(nrow(z2), size = 20), ]
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    FUN = c(sum, median = median, d1 = function(x) x[1]),    
#'    vars = c("ant", "y", median = "ant", median = "y", d1 = "ant")
#' )
#' 
#' # with multiple outputs (function my_range)
#' # and with function of two variables (weighted.mean(y, ant))
#' my_range <- function(x) c(min = min(x), max = max(x))
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    FUN = c(sum, ra = my_range, wmean = weighted.mean),    
#'    vars = list("ant", "y", ra = "ant", wmean  = c("y", "ant"))
#' )
#' 
#' # with specified output variable names
#' my_range <- function(x) c(min = min(x), max = max(x))
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    FUN = c(sum, ra = my_range, wmean = weighted.mean),    
#'    vars = list("ant", "y", 
#'                `antmin,antmax` = list(ra = "ant"), 
#'                 yWmean  = list(wmean  = c("y", "ant")))
#' )
#' 
#' 
#' 
#'  
aggregate_multiple_fun <- function(data, by, FUN, vars, ind = NULL, ..., name_sep = "_", multi_sep = ",", print_inc = TRUE) {
  
  if(is.null(ind)){
    ind = data.frame(ind = seq_len(nrow(data)))
  }
  
  names(ind) = "i7N9Qd3"
  
  if (is.function(FUN)) {
    FUN <- c(FUN)
    names(FUN) <- ""
  }
  if (is.null(names(FUN))) {
    names(FUN) <- ""
  }
  
  
  vars <- fix_vars_amf(vars)
  
  
  output_names <- sapply(vars, function(x) x[[1]] )
  FUN_names <- sapply(vars, function(x) x[[2]] )
  vars <- lapply(vars, function(x) x[-(1:2)] )
  
  if (is.null(names(FUN))) {
    names(FUN) <- ""
  }
  
  names(FUN)[is.na(names(FUN))] <- ""
  if (anyDuplicated(names(FUN))) {
    stop("FUN must be uniquely named")
  }
  
  
  if (!all(unlist(vars) %in% names(data))) {
    stop("All vars must be in names(x)")
  }
  if (!all(FUN_names %in% names(FUN))) {
    stop("All FUN names in vars must be in names(FUN)")
  }
  if (!all(names(FUN) %in% FUN_names)) {
    warning("Not all FUN elements will be used")
    FUN <- FUN[names(FUN) %in% FUN_names]
  }
  
  
  data1 = data[1, ]
  for(i in seq_len(ncol(data1))){
    d1 = unlist(data1[1,i])[1]
    if(length(d1)==1)
      data1[,i] = d1
  }  
  
  
  FUN_all <- function(ind, FUN_input, data, vars, output_names, FUN_names, data1 = NULL, fun_all_0 = NULL){
    if(length(ind)==1)
      if(ind==0)
        if(!is.null(fun_all_0)){
          return(fun_all_0)
        } else {
          data = data1
        }
        
    out <- vector("list", length(vars))
    for(i in seq_along(out)){
      j <- match(FUN_names[i], names(FUN_input)) 
      if(length(vars[[i]]) == 1){
        out[[i]] <- FUN_input[[j]](unlist(data[[vars[[i]]]][ind]))
        if (names(FUN)[j] == "") {
          names(out)[i] <- vars[[i]]
        } else {
          names(out)[i] <- paste(vars[[i]], names(FUN)[j], sep = "_")
        }
      } else {
        if(length(vars[[i]]) == 2)   out[[i]] <- FUN_input[[j]]( unlist(data[[vars[[i]][1]]][ind]), unlist(data[[vars[[i]][2]]][ind]))
        if(length(vars[[i]]) == 3)   out[[i]] <- FUN_input[[j]]( unlist(data[[vars[[i]][1]]][ind]), unlist(data[[vars[[i]][2]]][ind]), unlist(data[[vars[[i]][3]]][ind]))
        if(length(vars[[i]]) == 4)   out[[i]] <- FUN_input[[j]]( unlist(data[[vars[[i]][1]]][ind]), unlist(data[[vars[[i]][2]]][ind]), unlist(data[[vars[[i]][3]]][ind]), unlist(data[[vars[[i]][4]]][ind]))
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
            split_names= strsplit(output_names[i], multi_sep)[[1]]
            if(length(split_names) != length(out[[i]])){
              easy_name = TRUE
              warning("warning")
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
    fun_all_0 = FUN_all(ind = 0L, vars = vars, output_names = output_names, FUN_names = FUN_names, FUN_input = FUN, data = data, data1 = data1) 
  } else {
    fun_all_0 = NULL # not needed 
  }
  
  z <- aggregate(x = ind, by = by, FUN = FUN_all, vars = vars, output_names = output_names, FUN_names = FUN_names, FUN_input = FUN, data = data, fun_all_0 = fun_all_0, ...)
  
  #Transform  embedded matrix
  z <- unmatrix(z)
  names(z) <- sub(paste0(names(ind), name_sep), "", colnames(z))
  
  # Fix name when not embedded matrix
  grepind <- grep(names(ind), names(z))
  if(length(grepind)==1){
    if(length(FUN)==1 & length(vars)==1){
      names(z)[grepind]  =  output_names[1]
    } else {
      names(z)[grepind] <- "output_from_FUN"
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
#'
#' @return
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
#' v2 <- list(c(FUN = "sum", "a"), c(FUN = "sum", "w"), c(FUN = "q", "a", "w"), 
#'            c(FUN = "snitt", "b", "w"))
#' v3 <- list(sum = "a", sum = "w", q = c(name = "a:w_q", "a", "w"), 
#'            `b:w_snitt` = list(snitt = c("b", "w")))
#' v4 <- list(c(name = "a_sum", FUN = "sum", "a"), 
#'            c(name = "w_sum", FUN = "sum", "w"), 
#'            c(name = "a:w_q", FUN = "q", "a", "w"), 
#'            c(name = "b:w_snitt", FUN = "snitt", "b", "w"))
#' v5 <- list(a_sum = c(FUN = "sum", "a"), 
#'            w_sum = c(FUN = "sum", "w"), 
#'            `a:w_q` = c(FUN = "q", "a", "w"), 
#'            `b:w_snitt` = c(FUN = "snitt", "b", "w"))
#' 
#' identical(f(v1), f(v2))
#' identical(f(v1), f(v3))
#' identical(f(v1), f(v4))
#' identical(f(v1), f(v5))
#' 
#' identical(f(v1), f(f(v1)))
#' identical(f(v1), v4)
fix_vars_amf  = function(vars){
  if (is.null(vars)) {
    stop("non-NULL vars needed")
  }
  vars <- as.list(vars)
  for(i in seq_along(vars)){
    vars[[i]] = fi_amf(vars[i])
  }
  names(vars) <- NULL
  vars
}


fi_amf = function(vars_i){
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
    vars_i = c(name = names_i, FUN = names_i2, vars_i) 
  } else {
    addname = TRUE
    if("FUN" %in%  names(vars_i)){
      if(names_i != ""){
        vars_i = c(name = names_i, vars_i)
        addname = FALSE
      }
    } else {
      vars_i = c(FUN = names_i, vars_i)
    }
    if("name" %in%  names(vars_i)){
      addname = FALSE
    }
    if(addname){
      FUN_name = vars_i[["FUN"]]
      vars_i_ = vars_i[names(vars_i) != "FUN"]
      if (FUN_name == "") {
        name <- paste(vars_i_, collapse = ":")
      } else {
        name <- paste(paste(vars_i_, collapse = ":"), FUN_name, sep = "_")
      }
      vars_i = c(name = name, vars_i)
    }
  }
  n_FUN  = sum(names(vars_i) == "FUN")
  n_name = sum(names(vars_i) == "name")
  if(n_FUN > 1){
    stop("Multiple function names found")
  }
  if(n_FUN < 1){
    stop("Function names: something is wrong")
  }
  if(n_name > 1){
    stop("Multiple output names found")
  }
  if(n_name < 1){
    stop("Output names: something is wrong")
  }
  c(name = vars_i[["name"]], FUN = vars_i[["FUN"]],  vars_i[!(names(vars_i) %in% c("FUN", "name"))])    # gjøre enklere med indekser 
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
  unmatrix(data)
}





















