
#' Apply a function to subsets defined by a dummy matrix
#' 
#' For each column, `i`,  of the matrix `x` of zeros and ones, the output value is equivalent to `FUN(y[x[, i] != 0])`. 
#' 
#' With a dummy `x` and `FUN = sum`, output is equivalent to `z = t(x) %*% y`.
#'
#' @param x A (sparse) dummy matrix
#' @param y Vector of input values
#' @param FUN A function
#' @param simplify Parameter to \code{\link{aggregate}}. When `FALSE`, list output is ensured.
#' 
#' @return Vector of output values or a matrix when multiple outputs from `FUN`  (see examples). 
#'         List output is also possible (ensured when `simplify = FALSE`).  
#' @importFrom methods as
#' @importFrom Matrix uniqTsparse drop0
#' @export
#' 
#' @examples
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' a <- ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), 
#'       sum1 = (t(a$modelMatrix) %*% z$ths_per)[,1],
#'       sum2 = DummyApply(a$modelMatrix, z$ths_per, sum),
#'        max = DummyApply(a$modelMatrix, z$ths_per, max))
#'        
#' DummyApply(a$modelMatrix, z$ths_per, range)
#' DummyApply(a$modelMatrix, z$ths_per, range, simplify = FALSE)  
#' 
#' a$modelMatrix[, c(3, 5)] <- 0   # Introduce two empty columns. 
#' DummyApply(a$modelMatrix, z$ths_per, function(x){ 
#'   c(min = min(x), 
#'     max = max(x), 
#'     mean = mean(x), 
#'     median = median(x), 
#'     n = length(x))})   
#'     
#' DummyApply(a$modelMatrix, z$ths_per, function(x) x, simplify = FALSE)          
#' 
DummyApply <- function(x, y, FUN = sum, simplify = TRUE) {
  FUNind <- function(ind) FUN(y[ind])
  x <- uniqTsparse(As_TsparseMatrix(x)) # x <- uniqTsparse(as(drop0(x), "dgTMatrix"))
  seq_len_ncol_x <- seq_len(ncol(x))
  colf <- list(factor(x@j + 1L, levels = seq_len_ncol_x))
  # Fix for aggregate in old R versions (< 3.5.0)
  z <- seq_len_ncol_x + NA
  agg <- aggregate(x@i + 1L, by = colf, FUNind, drop = FALSE, simplify = simplify)
  if (is.matrix(agg[[2]])) {
    z <- matrix(z, nrow = length(z), ncol = ncol(agg[[2]]))
    colnames(z) <- colnames(agg[[2]])
    z[agg[[1]], ] <- agg[[2]]
  } else {
    z[agg[[1]]] <- agg[[2]]
  }
  # end Fix
  # z <- aggregate(x@i + 1L, by = colf, FUNind, drop = FALSE)[[2]] (without Fix)
  is_na_z <- !(seq_len_ncol_x %in% (x@j + 1L))     # Better than z[is.na(z)] <- ..
  if (any(is_na_z)) {   # Test to avoid warning. e.g In FUN(y[integer(0)]) : no non-missing arguments to max; returning -Inf
    if (is.list(z)) {
      z[is_na_z] <- list(FUN(y[integer(0)]))
    } else {
      if (is.matrix(agg[[2]])) {
        z[is_na_z, ] <- rep(FUN(y[integer(0)]), each = sum(is_na_z))
      } else {
        z[is_na_z] <- FUN(y[integer(0)])
      }
    }
  }
  z
}
