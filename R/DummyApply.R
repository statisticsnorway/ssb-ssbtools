
#' Apply a function to subsets defined by a dummy matrix
#' 
#' For each column, `i`,  of the matrix `x` of zeros and ones, the output value is equivalent to `FUN(y[x[, i] != 0])`. 
#' 
#' With a dummy `x` and `FUN = sum`, output is equivalent to `z = t(x) %*% y`.
#'
#' @param x A (sparse) dummy matrix
#' @param y Vector of input values
#' @param FUN A function
#' 
#' @return Vector of output values  
#' @importFrom methods as
#' @importFrom Matrix uniqTsparse drop0
#' @export
#' 
#' @examples
#' 
#' z <- SSBtools::SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' a <- SSBtools::ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), 
#'       sum1 = (t(a$modelMatrix) %*% z$ths_per)[,1],
#'       sum2 = DummyApply(a$modelMatrix, z$ths_per, sum),
#'        max = DummyApply(a$modelMatrix, z$ths_per, max))
#' 
DummyApply <- function(x, y, FUN = sum) {
  FUNind <- function(ind) FUN(y[ind])
  x <- uniqTsparse(as(drop0(x), "dgTMatrix"))
  seq_len_ncol_x <- seq_len(ncol(x))
  colf <- list(factor(x@j + 1L, levels = seq_len_ncol_x))
  z <- aggregate(x@i + 1L, by = colf, FUNind, drop = FALSE)[[2]]
  z[!(seq_len_ncol_x %in% (x@j + 1L))] <- FUN(y[integer(0)])  # Better than z[is.na(z)] <- .. 
  z
}
