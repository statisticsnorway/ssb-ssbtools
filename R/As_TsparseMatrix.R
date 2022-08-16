


#' Transform to TsparseMatrix/dgTMatrix
#' 
#' To implement adaption needed after Matrix ver. 1.4-2 since
#'  `as(from, "dgTMatrix")` no longer allowed.
#'  
#' 
#' @details
#' This function is made to replace `as(from, "dgTMatrix")` and `as(drop0(from), "dgTMatrix")` in `SSBtools` and related packages.   
#'
#' @param from      A matrix
#' @param do_drop0  whether to run `drop0`
#'
#' @return A matrix. Virtual class is `TsparseMatrix`. Class `dgTMatrix` expected.
#' @importFrom utils compareVersion packageVersion
#' @export
#' 
#' @note `Matrix:::.as.via.virtual` in development version of package `Matrix` (date 2022-08-13) used to generate code.
#'
As_TsparseMatrix <- function(from, do_drop0 = TRUE) {
  
  if (do_drop0) {
    from <- drop0(from)
  }
  
  if (inherits(from, "dgTMatrix")) {
    return(from)
  }
  
  if (inherits(from, c("dgCMatrix", "dgeMatrix"))) {
    return(as(from, "TsparseMatrix"))  # Matrix:::.as.via.virtual('dgCMatrix', 'dgTMatrix'); Matrix:::.as.via.virtual('dgeMatrix', 'dgTMatrix')
  }
  
  if (inherits(from, "matrix")) {  # But "matrix" is not class after drop0
    if (compareVersion(as.character(packageVersion("Matrix")), "1.4.2") < 0) {   # Since code fails in earlier versions of Matrix.
      return(as(from, "dgTMatrix"))
    }
    return(as(as(as(from, "dMatrix"), "generalMatrix"), "TsparseMatrix"))  # Matrix:::.as.via.virtual('matrix', 'dgTMatrix')
  }
  
  as(as(from, "generalMatrix"), "TsparseMatrix")  # Matrix:::.as.via.virtual('dtrMatrix', 'dgTMatrix'); Matrix:::.as.via.virtual('ddiMatrix', 'dgTMatrix'); Matrix:::.as.via.virtual('dsCMatrix', 'dgTMatrix')
}



# Matrix:::.as.via.virtual in development version of Matrix (date 2022-08-13)
if(FALSE){
  .as.via.virtual <- function (Class1, Class2, from = quote(from)) 
  {
    if (!isClassDef(Class1)) 
      Class1 <- getClassDef(Class1)
    if (!isClassDef(Class2)) 
      Class2 <- getClassDef(Class2)
    if (!grepl("^[dln](di|ge|tr|sy|tp|sp|[gts][CRT])Matrix$", 
               Class2@className)) 
      stop("invalid 'Class2'")
    contains1 <- names(Class1@contains)
    contains2 <- names(Class2@contains)
    virtual <- list(c("dMatrix", "lMatrix", "nMatrix"), 
                    c("generalMatrix", "triangularMatrix", "symmetricMatrix"), 
                    c("CsparseMatrix", "RsparseMatrix", "TsparseMatrix", 
                      "diagonalMatrix", "unpackedMatrix", "packedMatrix"))
    to <- from
    for (v in virtual) {
      if (any(m <- match(v, contains2, 0L) > 0L)) {
        v1 <- v[m][1L]
        if (match(v1, contains1, 0L) == 0L) 
          to <- call("as", to, v1)
      }
    }
    to
  }
}