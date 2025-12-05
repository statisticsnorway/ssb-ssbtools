



#' `aggregate_multiple_fun` using a dummy matrix 
#' 
#' Wrapper to \code{\link{aggregate_multiple_fun}} 
#' that uses a dummy matrix instead of the `by` parameter.
#' Functionality for non-dummy  matrices as well.
#' 
#' Internally this function make use of the `ind` parameter to `aggregate_multiple_fun`  
#' 
#' If duplicate column names are present in `data`, only the last occurrence is
#' used. This choice is consistent with the structure of the data after
#' pre-aggregation with [aggregate_by_pkg()], where overlaps between the `by`
#' and `var` parameters may produce columns with identical names, and the last
#' column represents the intended value.
#'
#' @param x A (sparse) dummy matrix
#' @inheritParams aggregate_multiple_fun
#' @param dummy When `TRUE`, only 0s and 1s are assumed in `x`.
#'              When `FALSE`, non-0s in `x` are passed as an additional first input parameter to the `fun` functions.
#'              Thus, the same result as matrix multiplication is achieved with `fun = function(x, y) sum(x * y)`.
#'              In this case, the data will not be subjected to `unlist`. See \code{\link{aggregate_multiple_fun}}. 
#' @param when_non_dummy Function to be called when `dummy` is `TRUE` and when `x` is non-dummy.  Supply `NULL` to do nothing. 
#' @param keep_names When `TRUE`, output row names are inherited from column names in `x`. 
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
#' d2 <- SSBtoolsData("d2")
#' set.seed(12)
#' d2$y <- round(rnorm(nrow(d2)), 2)
#' d <- d2[sample.int(nrow(d2), size = 20), ]
#' 
#' x <- ModelMatrix(d, formula = ~main_income:k_group - 1)
#' 
#' # with specified output variable names
#' my_range <- function(x) c(min = min(x), max = max(x))
#' dummy_aggregate(
#'    data = d, 
#'    x = x, 
#'    vars = list("freq", "y", 
#'                `freqmin,freqmax` = list(ra = "freq"), 
#'                 yWmean  = list(wmean  = c("y", "freq"))),
#'    fun = c(sum, ra = my_range, wmean = weighted.mean))
#' 
#' 
#' # Make a non-dummy matrix 
#' x2 <- x
#' x2[17, 2:5] <- c(-1, 3, 0, 10)
#' x2[, 4] <- 0
#' 
#' # Now warning 
#' # Result is not same as t(x2) %*% d[["freq"]]
#' dummy_aggregate(data = d, x = x2, vars = "freq", fun = sum)
#' 
#' # Now same as t(x2) %*% d[["freq"]]
#' dummy_aggregate(data = d, x = x2, 
#'                 vars = "freq", dummy = FALSE,
#'                 fun = function(x, y) sum(x * y))
#' 
#' 
#' # Same as t(x2) %*% d[["freq"]]  + t(x2^2) %*% d[["y"]] 
#' dummy_aggregate(data = d, x = x2, 
#'                 vars = list(c("freq", "y")), dummy = FALSE,
#'                 fun = function(x, y1, y2) {sum(x * y1) + sum(x^2 * y2)})
#'                 
dummy_aggregate <- function(data, x, vars, fun = NULL, dummy = TRUE, 
                            when_non_dummy = warning, keep_names = TRUE, ...) {
  
  if (anyDuplicated(names(data))) {
    data <- data[, !duplicated(names(data), fromLast = TRUE)]
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
  
  
  if (dummy) {
    if (!is.null(when_non_dummy)) {
      if (min(x@x) < 1 | max(x@x) > 1) {
        when_non_dummy("All non-0s in x are treated as 1s. Use dummy = FALSE?")
      }
    }
    
  } else {
    x_i_1L <- cbind(x_i_1L, x = c(x@x, rep(NA, length(is_na_j1))))
  }
  
  out <- aggregate_multiple_fun(data = data, ind = x_i_1L, by = x_j_1L, vars = vars, fun = fun, ...)[-1]
  if (keep_names & !is.null(colnames(x))) {
    rownames(out) <- colnames(x)
  }
  out
}




