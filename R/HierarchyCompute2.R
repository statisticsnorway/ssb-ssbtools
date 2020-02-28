#' Extended Hierarchical Computations
#' 
#' Extended variant of \code{\link{HierarchyCompute}} with several column variables (not just \code{"colFactor"}).
#' Parameter colVar splits the hierarchy variables in two groups and this variable overrides the difference between \code{"rowFactor"} and \code{"colFactor"}.
#' 
#' Within this function, \code{HierarchyCompute} is called two times. 
#' By specifying output as \code{"matrixComponents"}, 
#' output from the two runs are retuned as a list with elements \code{hcRow} and \code{hcCol}.
#' The matrix multiplication in HierarchyCompute is extended to 
#' \code{outputMatrix} \code{=} \code{hcRow$dataDummyHierarchy} 
#' \code{\%*\%} \code{hcRow$valueMatrix}
#' \code{\%*\%} \code{t(hcCol$dataDummyHierarchy)}.
#' This is modified in cases with more than a single \code{valueVar}.
#' 
#' @note There is no need to call \code{HierarchyCompute2} directly. 
#'       The main function \code{\link{HierarchyCompute}}  can be used instead.
#'
#' @param data The input data frame
#' @param hierarchies A named list with hierarchies 
#' @param colVar Name of the column variable(s)
#' @param valueVar Name of the variable(s) to be aggregated
#' @param rowSelect Data frame specifying variable combinations for output
#' @param colSelect Data frame specifying variable combinations for output
#' @param select Data frame specifying variable combinations for output
#' @param output One of "data.frame" (default), "outputMatrix", "matrixComponents".
#' @param ...  Further parameters sent to \code{\link{HierarchyCompute}} 
#'
#' @return As specified by the parameter \code{output}
#' @seealso \code{\link{Hierarchies2ModelMatrix}}, \code{\link{AutoHierarchies}}.
#' @export
#'
#' @author Øyvind Langsrud
#' 
#' @examples
#' x <- SSBtoolsData("sprt_emp")
#' geoHier <- SSBtoolsData("sprt_emp_geoHier")
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#'
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
#'                  colVar = c("age", "year"))
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per",
#'                  colVar = c("age", "geo"))
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
#'                  colVar = c("age", "year"), output = "matrixComponents")
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
#'                  colVar = c("age", "geo"), output = "matrixComponents")
#'
HierarchyCompute2 <- function(data, hierarchies, valueVar, colVar, 
                               rowSelect = NULL, colSelect = NULL, 
                               select = NULL, output = "data.frame", ...){
  HierarchyCompute2_(data=data, hierarchies=hierarchies, valueVar=valueVar, colVar=colVar,
                     rowSelect=rowSelect, colSelect=colSelect, select=select, output=output, ...)
}

HierarchyCompute2_ <- function(data, hierarchies, valueVar, colVar, rowSelect = NULL, colSelect = NULL, select = NULL, inputInOutput = FALSE, output = "data.frame", autoLevel = TRUE, 
                               unionComplement = FALSE, constantsInOutput = NULL, handleDuplicated = "sum", asInput = FALSE, verbose = FALSE, makeRownames = NULL, ...) {
  
  if (!length(valueVar)) 
    stop("valueVar needed")
  
  if (!is.null(select)) {
    if ((!is.null(rowSelect)) | (!is.null(colSelect))) 
      stop("With non-NULL")
    
    colTRUE <- names(select) %in% colVar
    
    colSelect <- RowGroups(select[, colTRUE, drop = FALSE], returnGroups = TRUE)$groups
    rowSelect <- RowGroups(select[, !colTRUE, drop = FALSE], returnGroups = TRUE)$groups
  }
  
  nHier <- length(hierarchies)
  inputInOutput <- rep_len(inputInOutput, nHier)
  autoLevel <- rep_len(autoLevel, nHier)
  unionComplement <- rep_len(unionComplement, nHier)
  
  if (!(output %in% c("data.frame", "outputMatrix", "matrixComponents"))) {
    stop("output must be one of \"data.frame\",\"outputMatrix\", \"matrixComponents\"")
  }
  
  doubleOutput <- !(output %in% c("data.frame", "outputMatrix"))
  
  if (is.null(makeRownames)) {
    makeRownames <- doubleOutput
  }
  
  colRowGroups <- RowGroups(data[, colVar, drop = FALSE], returnGroups = TRUE)
  
  isColVar <- names(hierarchies) %in% colVar
  
  hierarchiesCol <- Col2rowFactor(hierarchies[isColVar])
  hierarchiesRow <- Col2rowFactor(hierarchies[!isColVar])
  
  names_hierarchiesRow <- names(hierarchiesRow)
  
  hierarchiesRow$colRowGroups_idx <- "colFactor"
  
  hcCol <- HierarchyCompute(colRowGroups$groups, hierarchiesCol, rowSelect = colSelect, output = "dataDummyHierarchyWithCodeFrame", 
                            inputInOutput = inputInOutput[isColVar], autoLevel = autoLevel[isColVar], 
                            unionComplement = unionComplement[isColVar], asInput = TRUE, verbose = verbose, makeRownames = makeRownames, ...)
  
  colSelect_idx <- as.character(seq_len(nrow(colRowGroups$groups)))
  
  dataRow <- data[, c(names_hierarchiesRow, valueVar), drop = FALSE]
  
  dataRow$colRowGroups_idx <- colRowGroups$idx
  
  hcRow <- HierarchyCompute(dataRow, hierarchiesRow, valueVar = valueVar, colSelect = colSelect_idx, rowSelect = rowSelect, output = "matrixComponents", 
                            inputInOutput = inputInOutput[!isColVar], 
                            autoLevel = autoLevel[!isColVar], unionComplement = unionComplement[!isColVar], 
                            handleDuplicated = handleDuplicated, asInput = asInput, verbose = verbose, ...)
  
  rm(dataRow)
  
  if (length(colSelect_idx) > 0) {
    if (length(valueVar) > 1) {
      colnamesOK <- !any(!(colSelect_idx == colnames(hcRow$valueMatrix)[seq_len(length(colSelect_idx))]))
    } else {
      colnamesOK <- !any(!(colSelect_idx == colnames(hcRow$valueMatrix)))
    }
  } else {
    colnamesOK <- ncol(hcRow$valueMatrix) == 0
  }
  
  if (!colnamesOK) 
    stop("Somthing is wrong. Intermediate data not sorted  as expected.")
  
  if (doubleOutput) 
    return(list(hcRow = hcRow, hcCol = hcCol))
  
  nValueVar <- length(valueVar)
  
  noColVar <- FALSE
  nCol <- length(colSelect_idx)
  
  if (length(valueVar) > 1 & nCol > 0) {
    outputMatrix <- NewNcol(NewNcol(hcRow$dataDummyHierarchy %*% hcRow$valueMatrix, nCol) %*% t(hcCol$dataDummyHierarchy), nrow(hcCol$dataDummyHierarchy) * nValueVar)
  } else {
    outputMatrix <- hcRow$dataDummyHierarchy %*% hcRow$valueMatrix %*% t(hcCol$dataDummyHierarchy)
  }
  
  if (length(valueVar) > 1 & nCol == 0) {
    colnames_outputMatrix <- colnames(outputMatrix)
    outputMatrix <- matrix(outputMatrix, nrow(outputMatrix), nValueVar * ncol(outputMatrix))
    colnames(outputMatrix) <- rep_len(colnames_outputMatrix, ncol(outputMatrix))
  }
  
  if (output == "outputMatrix") {
    return(outputMatrix)
  }
  
  if (verbose) {
    cat(" [ output='data.frame'...")
    flush.console()
  }
  
  x <- as.matrix(outputMatrix)
  colnamesX <- colnames(x)
  dimX1 <- dim(x)[1]
  
  if (nValueVar > 1) {
    if (!noColVar) {
      colnamesX <- colnamesX[seq_len(nCol)]
      x <- matrix(x, ncol = nValueVar)
      colnames(x) <- valueVar
      z <- as.data.frame(x)
    } else {
      z <- as.data.frame(x)
    }
  } else {
    z <- data.frame(a = as.vector(x), stringsAsFactors = FALSE)
    names(z) <- valueVar
  }
  
  xCrossCode <- hcRow$toCrossCode
  colDataSelected <- hcCol$codeFrame[rep(seq(nrow(hcCol$codeFrame)), times = 1, each = dimX1), , drop = FALSE]
  
  if (!is.null(constantsInOutput)) {
    w <- cbind(constantsInOutput, colDataSelected, xCrossCode, z)
  } else {
    w <- cbind(colDataSelected, xCrossCode, z)
  }
  
  if (!is.null(select)) {
    ma <- Match(select, w[, names(select), drop = FALSE])
    w <- w[ma, , drop = FALSE]
  }
  
  if (verbose) {
    cat("]\n")
    flush.console()
  }
  
  rownames(w) <- NULL
  
  w
}



Col2rowFactor <- function(x) {
  for (i in seq_along(x)) {
    if (is.character(x[[i]])) 
      if (x[[i]] == "colFactor") 
        x[[i]] <- "rowFactor"
  }
  x
}

NewNcol <- function(x, nCol) {
  dimX <- dim(x)
  if (nCol == dimX[2]) 
    return(x)
  if (nCol > dimX[2]) {
    z <- x
    dim(z) <- c(prod(dimX)/nCol, nCol)
    z <- z[, order(order(rep_len(seq_len(dimX[2]), nCol))), drop = FALSE]
  } else {
    z <- x[, order(rep_len(seq_len(nCol), dimX[2])), drop = FALSE]
    dim(z) <- c(prod(dimX)/nCol, nCol)
  }
  if (!is.null(colnames(x))) 
    colnames(z) <- rep_len(colnames(x), nCol)
  z
}



