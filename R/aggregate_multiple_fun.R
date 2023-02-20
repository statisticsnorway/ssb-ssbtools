                       
#' Wrapper to `aggregate` 
#' 
#' Wrapper to \code{\link{aggregate}} that allows multiple functions and functions of several variables 
#' 
#' One intention of `aggregate_multiple_fun` is to be a true generalization of `aggregate`. 
#' However, when many functions are involved, passing extra parameters can easily lead to errors. 
#' Therefore `forward_dots` and `dots2dots` are set to `FALSE` by default.
#' When `forward_dots = TRUE` and `dots2dots = FALSE`, parameters will be forwarded, 
#' but only parameters that are explicitly defined in the specific `fun` function.
#' For the `sum` function, this means that a possible `na.rm` parameter is forwarded but not others.
#' When `forward_dots = TRUE` and `dots2dots = TRUE`, other parameters will also be forwarded to `fun` functions where `...` is included. 
#' For the `sum` function, this means that such extra parameters will, probably erroneously, be included in the summation (see examples).
#' 
#' For the function to work with \code{\link{dummy_aggregate}}, 
#' the data is subject to \code{\link{unlist}} before the `fun` functions are called.
#' This does not apply in the special case where `ind` is a two-column data frame.
#' Then, in the case of list data, the `fun` functions have to handle this themselves.
#' 
#' A limitation when default output, when `do_unlist = TRUE`, is that variables in output are forced to have the same class. 
#' This is caused by the \code{\link{unlist}} function being run on the output. This means, for example, 
#' that all the variables will become numeric when they should have been both integer and numeric.
#' 
#' 
#' @param data A data frame containing data to be aggregated 
#' @param by A data frame defining grouping
#'    
#' @param vars  A named vector or list of variable names in `data`. The elements are named by the names of `fun`.
#'              All the pairs of variable names and function names thus define all the result variables to be generated.
#' * Parameter `vars` will converted to an internal standard by the function \code{\link{fix_vars_amf}}. 
#'              Thus, function names and also output variable names can be coded in different ways.
#'              Multiple output variable names can be coded using `multi_sep`. 
#'              See examples and examples in \code{\link{fix_vars_amf}}. Indices instead of variable names are allowed. 
#' * Omission of (some) names is possible since names can be omitted for one function (see `fun` below).
#' * A special possible feature is the combination of a single unnamed variable and all functions named. 
#'              In this case, all functions are run and output variable names will be identical to the function names.
#'            
#' @param fun A named list of functions. These names will be used as suffixes in output variable names. Name can be omitted for one function. 
#'            A vector of function as strings is also possible. When unnamed, these function names will be used directly. 
#'            See the examples of \code{\link{fix_fun_amf}}, which is the function used to convert `fun`.
#'            Without specifying `fun`, the functions, as strings, are taken from the function names coded in `vars`.
#'              
#' @param ind When non-NULL, a data frame of indices. 
#'            When NULL, this variable will be generated internally as `data.frame(ind = seq_len(nrow(data)))`. 
#'            The parameter is useful for advanced use involving model/dummy matrices.
#'            For special use (`dummy = FALSE` in \code{\link{dummy_aggregate}}) `ind` can also be a two-column data frame.   
#'            
#'                
#' @param ... 	Further arguments passed to `aggregate` and, 
#'              depending on `forward_dots`/`dots2dots`, forwarded to the functions in `fun` (see details).
#' @param name_sep  A character string used when output variable names are generated. 
#' @param seve_sep  A character string used when output variable names are generated from functions of several variables. 
#' @param multi_sep A character string used when multiple output variable names are sent as input. 
#' @param forward_dots Logical vector (possibly recycled) for each element of `fun` that determines whether `...` should be forwarded (see details). 
#' @param dots2dots  Logical vector (possibly recycled) specifying the behavior when `forward_dots = TRUE` (see details).
#' @param do_unmatrix By default (`TRUE`), the implementation uses \code{\link{unmatrix}} before returning output. 
#'                    For special use this can be omitted (`FALSE`).
#' @param do_unlist   By default (`TRUE`), the implementation uses \code{\link{unlist}} to combine output from multiple functions. 
#'                    For special use this can be omitted (`FALSE`).
#'
#' @return A data frame
#' @export
#' @importFrom stats aggregate
#' 
#'
#' @examples
#' z2 <- SSBtoolsData("z2")
#' set.seed(12)
#' z2$y <- round(rnorm(nrow(z2)), 2)
#' z <- z2[sample.int(nrow(z2), size = 20), ]
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    vars = c("ant", "y", median = "ant", median = "y", d1 = "ant"),
#'    fun = c(sum, median = median, d1 = function(x) x[1])  
#' )
#' 
#' # With functions as named strings 
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    vars = c(sum = "y", med = "ant", med = "y"),
#'    fun = c(sum = "sum", med = "median")
#' )
#' 
#' # Without specifying functions 
#' # - equivalent to `fun = c("sum", "median")` 
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    vars = c(sum = "y", median = "ant", median = "y")
#' )
#' 
#' # The single unnamed variable feature. Also functions as strings. 
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    vars = "y",
#'    fun = c("sum", "median", "min", "max")
#' ) 
#' 
#' # with multiple outputs (function my_range)
#' # and with function of two variables (weighted.mean(y, ant))
#' my_range <- function(x) c(min = min(x), max = max(x))
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    vars = list("ant", "y", ra = "ant", wmean  = c("y", "ant")),
#'    fun = c(sum, ra = my_range, wmean = weighted.mean)
#' )
#' 
#' # with specified output variable names
#' my_range <- function(x) c(min = min(x), max = max(x))
#' aggregate_multiple_fun(
#'    data = z, 
#'    by = z[c("kostragr", "hovedint")], 
#'    vars = list("ant", "y", 
#'                `antmin,antmax` = list(ra = "ant"), 
#'                 yWmean  = list(wmean  = c("y", "ant"))),
#'    fun = c(sum, ra = my_range, wmean = weighted.mean)
#' )
#' 
#' 
#' # To illustrate forward_dots and dots2dots
#' q <- z[1, ]
#' q$w <- 100 * rnorm(1)
#' for (dots2dots in c(FALSE, TRUE)) for (forward_dots in c(FALSE, TRUE)) {
#'   cat("\n=======================================\n")
#'   cat("forward_dots =", forward_dots, ", dots2dots =", dots2dots)
#'   out <- aggregate_multiple_fun(
#'     data = q, by = q["kostragr"], 
#'     vars = c(sum = "ant", round = "w"), fun = c("sum", "round"),  
#'     digits = 3, forward_dots = forward_dots, dots2dots = dots2dots)
#'   cat("\n")
#'   print(out)
#' }
#' # In last case digits forwarded to sum (as ...) 
#' # and wrongly included in the summation
#'  
aggregate_multiple_fun <- function(data, by, vars, fun = NULL, ind = NULL, ..., 
       name_sep = "_", seve_sep = ":", multi_sep = ",", forward_dots = FALSE, 
       dots2dots = FALSE, do_unmatrix = TRUE, do_unlist = TRUE) {
  
  if (any(forward_dots)) {
    match_call <- match.call()
    is_dot <- !(names(match_call)[-1] %in% names(formals(aggregate_multiple_fun)))
  } else {
    is_dot <- FALSE
  }
  
  x_r <- NULL
  x_x <- NULL 
  if (!is.null(ind)) {
    if (ncol(ind) == 2) {
      x_r <- ind[[1]]  # matrix row   (x@i + 1L), see SSBtools::As_TsparseMatrix
      x_x <- ind[[2]]  # matrix value (x@x)      
      ind <- data.frame(ind = seq_len(length(x_r)))
      ind[x_r == 0L, ] <- 0L   # for quick fun_all_0 return
    }
  } else {
    ind = data.frame(ind = seq_len(nrow(data)))
  }
  
  names(ind) = "i7N9Qd3"
  

  vars <- fix_vars_amf(vars, name_sep = name_sep,  seve_sep = seve_sep, multi_sep = multi_sep, names_data = names(data))
  
  
  output_names <- sapply(vars, function(x) x[[1]] )
  fun_names <- sapply(vars, function(x) x[[2]] )
  vars <- lapply(vars, function(x) x[-(1:2)] )
  
  if (!length(fun)) {
    fun <- unique(fun_names)
  }
  fun <- fix_fun_amf(fun)
  
  if (anyDuplicated(names(fun))) {
    stop("fun must be uniquely named")
  }
  
  
  if (!all(unlist(vars) %in% names(data))) {
    stop("All vars must be in names(data)")
  }
  if (!all(fun_names %in% names(fun))) {
    if (identical(fun_names, "") & length(vars[[1]]) == 1) { # then length(vars) is 1
      output_names <- names(fun)
      fun_names <- names(fun)
      vars <- rep(vars, length(fun))
    } else {
      stop("All fun names in vars must be in names(fun)")
    }
  }
  if (!all(names(fun) %in% fun_names)) {
    warning("Not all fun elements will be used")
    fun <- fun[names(fun) %in% fun_names]
  }

  if (any(is_dot)) {
    forward_dots <- rep_len(forward_dots, length(fun))
    dots2dots <- rep_len(dots2dots, length(fun))
    dots <- list(...)[names(match_call)[-1][is_dot]]  
    # dots <- as.list(match_call)[-1][is_dot] # not working in all cases when generations back, depends on parameter order. Interpretation of e.g.: ..3 and ..4 
    # dots <- lapply(dots, eval)  # not working since need to go all generations back 
    fun_input <- fun
    dots_ind <- vector("list", length(fun))
    for (i in which(forward_dots)) {
      ma_fun_names <- fun_names %in% names(fun)[i]
      n_vars_fun_i <- unique(sapply(vars[ma_fun_names], length)) + as.integer(!is.null(x_r))
      if (any(ma_fun_names)) {
        if (length(n_vars_fun_i) > 1) {
          stop("NOT IMPLEMENTED: forward_dots combined with different number of variables for the same function")
        }
        if (is.primitive(fun[[i]])) {
          names_i <- names(formals(args(fun[[i]])))
        } else {
          names_i <- names(formals(fun[[i]]))
        }
        if ("..." %in% names_i) {
          if (dots2dots[i]) {
            dots_ind[[i]] <- seq_len(length(dots))
          } else {
            dots_ind[[i]] <- which(names(dots) %in% names_i)
          }
        } else {
          if (length(names_i) > n_vars_fun_i) {
            dots_ind[[i]] <- which(names(dots) %in% names_i)
          }
        }
        if (length(dots_ind[[i]])) {
          do_call_args <- paste("c(list(", paste0("x", seq_len(n_vars_fun_i), collapse = ", "), "),", "dots[dots_ind[[", i, "]]])")
          do_call_what <- paste0("fun_input[[", i, "]]")
          do_call_string <- paste("do.call(", do_call_what, ",", do_call_args, ")")
          fun_i_args <- paste0("x", seq_len(n_vars_fun_i), collapse = ", ")
          eval(parse(text = paste0("fun[[", i, "]] <- function(", fun_i_args, ") ", do_call_string)))
        }
      }
    }
  }

  data1 = data[1, ,drop=FALSE]
  for(i in seq_len(ncol(data1))){
    d1 = unlist(data1[1,i])[1]
    if(length(d1)==1)
      data1[,i] = d1
  }  
  
  
  fun_all <- function(ind, fun_input, data, vars, output_names, fun_names, 
                      x_r, x_x, do_unlist, data1 = NULL, fun_all_0 = NULL, ...){
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
      if (is.null(x_r)) {
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
          if(length(vars[[i]]) > 4){   # 2,3,4 implemented directly due to speed
            out[[i]] <- eval(parse(text = paste("fun_input[[j]](", paste("unlist(data[[vars[[i]][", seq_len(length(vars[[i]])), "]]][ind])", sep = "", collapse = ","),")")))
          }
        }
      } else {   # Copy of code above + unlist removed  + x_x[ind] included as extra parameter
        if(length(vars[[i]]) == 1){
          out[[i]] <- fun_input[[j]](x_x[ind], data[[vars[[i]]]][x_r[ind]])
          if (names(fun)[j] == "") {
            names(out)[i] <- vars[[i]]
          } else {
            names(out)[i] <- paste(vars[[i]], names(fun)[j], sep = name_sep)
          }
        } else {
          if(length(vars[[i]]) == 2)   out[[i]] <- fun_input[[j]](x_x[ind], data[[vars[[i]][1]]][x_r[ind]], data[[vars[[i]][2]]][x_r[ind]])
          if(length(vars[[i]]) == 3)   out[[i]] <- fun_input[[j]](x_x[ind], data[[vars[[i]][1]]][x_r[ind]], data[[vars[[i]][2]]][x_r[ind]], data[[vars[[i]][3]]][x_r[ind]])
          if(length(vars[[i]]) == 4)   out[[i]] <- fun_input[[j]](x_x[ind], data[[vars[[i]][1]]][x_r[ind]], data[[vars[[i]][2]]][x_r[ind]], data[[vars[[i]][3]]][x_r[ind]], data[[vars[[i]][4]]][x_r[ind]])
          if(length(vars[[i]]) > 4){   # 2,3,4 implemented directly due to speed
            out[[i]] <- eval(parse(text = paste("fun_input[[j]]( x_x[ind], ", paste("data[[vars[[i]][", seq_len(length(vars[[i]])), "]]][x_r[ind]]", sep = "", collapse = ","),")")))
          }
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
          if (!is.null(names(out[[i]]))) {
            if (length(out[[i]]) == 1) {
              names(out[[i]]) <- NULL
            }
          }
          names(out)[i] = output_names[i]
        }
      
    }
    if(!do_unlist){
      return(out)
    }
    unlist(out) 
  }
  
  
  if (min(ind[[1]]) == 0) {
    fun_all_0 <- fun_all(ind = 0L, fun_input = fun, data = data, vars = vars, 
                         output_names = output_names, fun_names = fun_names, 
                         x_r = x_r, x_x = x_x, do_unlist = do_unlist, data1 = data1)
  } else {
    fun_all_0 <- NULL  # not needed 
  } 

  
  z <- aggregate(x = ind, by = by, FUN = fun_all, fun_input = fun, data = data, 
                 vars = vars, output_names = output_names, fun_names = fun_names, 
                 x_r = x_r, x_x = x_x, do_unlist = do_unlist, fun_all_0 = fun_all_0, ...)
  
  
  if(do_unmatrix){
    #Transform  embedded matrix
    z <- unmatrix(z, sep = name_sep)
    names(z) <- sub(paste0(names(ind), name_sep), "", colnames(z))
  } else {
    names(z)[sapply(z, is.matrix)] <- ""
  }
  
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
#' @param ... unused parameters
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
fix_vars_amf  = function(vars, name_sep = "_", seve_sep = ":", multi_sep = ",", names_data = NULL, ...){
  if (!length(vars)) {
    stop("non-empty vars needed")
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
#' @examples
#' a <- aggregate(1:6, list(rep(1:3, 2)), range)
#' b <- unmatrix(a)
#' 
#' a
#' b
#' 
#' dim(a)
#' dim(b)
#' 
#' names(a)
#' names(b)
#' 
#' class(a[, 2])
#' class(b[, 2])
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
  if (!length(fun)) {
    stop("non-empty fun needed")
  }
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


















