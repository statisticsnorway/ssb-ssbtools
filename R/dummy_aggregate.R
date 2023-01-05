



#' `aggregate_multiple_fun` using a dummy matrix 
#' 
#' Wrapper to \code{\link{aggregate_multiple_fun}} 
#' that uses a dummy matrix instead of the `by` parameter
#' 
#' Internally this function make use of the `ind` parameter to `aggregate_multiple_fun`  
#'
#' @param x A (sparse) dummy matrix
#' @inheritParams aggregate_multiple_fun
#' @param ... Further arguments passed to `aggregate_multiple_fun`
#'
#' @return data frame
#' @export
#' @importFrom Matrix uniqTsparse
#' 
#' @seealso \code{\link{aggregate_multiple_fun}} 
#'
#' @examples
#' 
#' # Code that generates output similar to the 
#' # last example in aggregate_multiple_fun
#' 
#' z2 <- SSBtoolsData("z2")
#' set.seed(12)
#' z2$y <- round(rnorm(nrow(z2)), 2)
#' z <- z2[sample.int(nrow(z2), size = 20), ]
#' 
#' x <- ModelMatrix(z, formula = ~hovedint:kostragr - 1)
#' 
#' # with specified output variable names
#' my_range <- function(x) c(min = min(x), max = max(x))
#' dummy_aggregate(
#'    data = z, 
#'    x = x, 
#'    fun = c(sum, ra = my_range, wmean = weighted.mean),    
#'    vars = list("ant", "y", 
#'                `antmin,antmax` = list(ra = "ant"), 
#'                 yWmean  = list(wmean  = c("y", "ant")))
#' )
#' 
dummy_aggregate <- function(data, x, fun, vars = NULL, ...) {
  if (is.function(fun)) {
    fun <- c(fun)
    names(fun) <- ""
  }
  if (is.null(names(fun))) {
    names(fun) <- ""
  }
  if (is.null(vars)) {
    vars <- rep("", ncol(data))
  }
  if (is.null(names(vars))) {
    names(vars) <- ""
  }
  
  
  x <- uniqTsparse(As_TsparseMatrix(x))
  seq_len_ncol_x <- seq_len(ncol(x))
  
  x_i_1L <- x@i + 1L
  x_j_1L <- x@j + 1L
  
  is_na_j1 <- which(!(seq_len_ncol_x %in% x_j_1L))
  
  x_j_1L <- c(x_j_1L, is_na_j1)
  x_i_1L <- c(x_i_1L, rep(0L, length(is_na_j1)))
  
  x_j_1L <- data.frame(A = x_j_1L)
  x_i_1L <- data.frame(B = x_i_1L)
  
  
  aggregate_multiple_fun(data = data, ind = x_i_1L, by = x_j_1L, fun = fun, vars = vars, ...)[-1]
  
}