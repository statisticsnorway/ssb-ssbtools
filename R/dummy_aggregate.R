



#' `aggregate_multiple_fun` using a dummy matrix 
#' 
#' Wrapper to \code{\link{aggregate_multiple_fun}} 
#' that uses a dummy matrix instead of the `by` parameter.
#' Functionality for non-dummy  matrices as well.
#' 
#' Internally this function make use of the `ind` parameter to `aggregate_multiple_fun`  
#'
#' @param x A (sparse) dummy matrix
#' @inheritParams aggregate_multiple_fun
#' @param dummy When `TRUE`, only 0s and 1s are assumed in `x`.
#'              When `FALSE`, non-0s in `x` are passed as an additional first input parameter to the `fun` functions.
#'              Thus, the same result as matrix multiplication is achieved with `fun = function(x, y) sum(x * y)`.
#'              In this case, the data will not be subjected to `unlist`. See \code{\link{aggregate_multiple_fun}}. 
#' @param when_non_dummy Function to be called when `dummy` is `TRUE` and when `x` is non-dummy.  Supply `NULL` to do nothing. 
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
#'    vars = list("ant", "y", 
#'                `antmin,antmax` = list(ra = "ant"), 
#'                 yWmean  = list(wmean  = c("y", "ant"))),
#'    fun = c(sum, ra = my_range, wmean = weighted.mean))
#' 
#' 
#' # Make a non-dummy matrix 
#' x2 <- x
#' x2[17, 2:5] <- c(-1, 3, 0, 10)
#' x2[, 4] <- 0
#' 
#' # Now warning 
#' # Result is not same as t(x2) %*% z[["ant"]]
#' dummy_aggregate(data = z, x = x2, vars = "ant", fun = sum)
#' 
#' # Now same as t(x2) %*% z[["ant"]]
#' dummy_aggregate(data = z, x = x2, 
#'                 vars = "ant", dummy = FALSE,
#'                 fun = function(x, y) sum(x * y))
#' 
#' 
#' # Same as t(x2) %*% z[["ant"]]  + t(x2^2) %*% z[["y"]] 
#' dummy_aggregate(data = z, x = x2, 
#'                 vars = list(c("ant", "y")), dummy = FALSE,
#'                 fun = function(x, y1, y2) {sum(x * y1) + sum(x^2 * y2)})
#'                 
dummy_aggregate <- function(data, x, vars, fun = NULL, dummy = TRUE, 
                            when_non_dummy = warning, ...) {

  x <- uniqTsparse(As_TsparseMatrix(x))
  seq_len_ncol_x <- seq_len(ncol(x))
  
  x_i_1L <- x@i + 1L
  x_j_1L <- x@j + 1L
  
  is_na_j1 <- which(!(seq_len_ncol_x %in% x_j_1L))
  
  x_j_1L <- c(x_j_1L, is_na_j1)
  x_i_1L <- c(x_i_1L, rep(0L, length(is_na_j1)))
  
  x_j_1L <- data.frame(A = x_j_1L)
  x_i_1L <- data.frame(B = x_i_1L)
  
  
  if (dummy) {
    if (!is.null(when_non_dummy)) {
      if (min(x@x) < 1 | max(x@x) > 1) {
        when_non_dummy("All non-0s in x are treated as 1s. Use dummy = FALSE?")
      }
    }
    
  } else {
    x_i_1L <- cbind(x_i_1L, x = c(x@x, rep(NA, length(is_na_j1))))
  }
  
  aggregate_multiple_fun(data = data, ind = x_i_1L, by = x_j_1L, vars = vars, fun = fun, ...)[-1]
  
}