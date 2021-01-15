#' Sums (aggregates) and/or sparse model matrix with possible cross table
#'
#' By default this function return sums if the formula contains a response part and a model matrix otherwise
#'
#' The model matrix is constructed by calling fac2sparse() repeatedly. The sums are computed by calling aggregate() repeatedly.
#' Hierarchical variables handled when constructing cross table.
#' Column names constructed from the cross table.
#'
#' @param data data frame
#' @param formula A model formula
#' @param makeNames Column/row names made when TRUE
#' @param crossTable Cross table in output when TRUE
#' @param total String used to name totals
#' @param printInc  Printing "..." to console when TRUE
#' @param dropResponse When TRUE response part of formula ignored.
#' @param makeModelMatrix Make model matrix when TRUE. NULL means automatic.
#' @param sep String to separate when creating column names
#' @param sepCross String to separate when creating column names involving crossing
#'
#' @return
#'   A matrix of sums, a sparse model matrix or a list of two or three elements (model matrix and cross table and sums when relevant).
#'   
#' @importFrom stats aggregate as.formula delete.response terms
#' @importFrom Matrix fac2sparse
#' @importFrom utils flush.console
#'
#' @export
#' @author Øyvind Langsrud
#'
#'
#' @examples
#' x <- SSBtoolsData("sprt_emp_withEU")
#' 
#' FormulaSums(x, ths_per ~ year*geo + year*eu)
#' FormulaSums(x, ~ year*age*eu)
#' FormulaSums(x, ths_per ~ year*age*geo + year*age*eu, crossTable = TRUE, makeModelMatrix = TRUE)
#' FormulaSums(x, ths_per ~ year:age:geo -1)
FormulaSums <- function(data, formula, makeNames = TRUE, crossTable = FALSE, total = "Total", printInc = FALSE, 
                        dropResponse = FALSE, makeModelMatrix = NULL, sep = "-", sepCross = ":") {
  
  hg <- NULL  # Possible input in a future version
  
  termsFormula <- terms(as.formula(formula))
  
  intercept <- attr(termsFormula, "intercept") != 0
  
  if (dropResponse) 
    response <- FALSE 
  else 
    response <- attr(termsFormula, "response") != 0
  
  if (is.null(makeModelMatrix)) 
    makeModelMatrix <- !response
  
  fac <- attr(delete.response(termsFormula), "factors") != 0
  faccol <- match(rownames(fac), colnames(data))
  
  if (is.null(hg)) 
    hg <- HierarchicalGroups3(data[, faccol, drop = FALSE])
  
  hgid <- match(names(hg), colnames(data))
  
  nFactors <- length(faccol)
  
  hgcol <- rep(0, nFactors)
  for (i in seq_len(length(hg))) hgcol[hg[[i]]] <- hgid[i]
  
  hgcoli <- rep(0, NCOL(data))
  for (i in seq_len(length(hg))) 
    hgcoli[faccol[hg[[i]]]] <- i
  
  firstROW <- CharacterDataFrame(data[1, hgid, drop = FALSE])
  firstROW <- as.matrix(firstROW)
  firstROW[, ] <- total
  rownames(firstROW) <- NULL
  allRows <- firstROW
  
  if (intercept) 
    allRows <- firstROW 
  else 
    allRows <- firstROW[integer(0), , drop = FALSE]
  
  if (makeModelMatrix) {
    m <- fac2sparse(rep(1, NROW(data)))
    if (!intercept) 
      m <- m[integer(0), , drop = FALSE]
  }
  
  if (response) {
    aggFormula <- stats::update(as.formula(formula), ".~rg1RowGroups735345")
    attr(aggFormula, ".Environment") <- attr(as.formula(".~rg"), ".Environment")
    
    rg1RowGroups735345 <- rep(1, NROW(data))
    allSums <- as.matrix(aggregate(aggFormula, data, sum)[, -1, drop = FALSE])
    
    if (!intercept) {
      allSums <- allSums[integer(0), , drop = FALSE]
    }
  }
  
  nFac <- NCOL(fac)
  
  for (k in seq_len(nFac)) {
    if (printInc) 
      if (k%%max(1, round(nFac/10)) == 0) {
        cat(".")
        flush.console()
      }
    ck <- faccol[fac[, k]]
    
    if (makeNames | crossTable | response) 
      rg <- RowGroups(data[, ck, drop = FALSE], returnGroups = TRUE)
    
    if (response) {
      rg1RowGroups735345 <- rg[[1]]
      allSums <- rbind(allSums, as.matrix(aggregate(aggFormula, data, sum)[, -1, drop = FALSE]))
    }
    
    if (makeNames | crossTable) {
      ur <- rg[[2]]
      if (makeModelMatrix) 
        m <- rbind(m, fac2sparse(rg[[1]]))  # rBind 
      ur <- CharacterDataFrame(ur)
      ur <- as.matrix(ur)
      fr <- firstROW[rep(1, NROW(ur)), , drop = FALSE]
      hgcolick <- hgcoli[ck]
      if (!any(duplicated(hgcolick))) 
        fr[, hgcoli[ck]] <- ur else {
          for (ick in unique(hgcolick)) fr[, ick] <- MatrixPaste(ur[, hgcolick == ick, drop = FALSE], sep = sepCross)
        }
      allRows <- rbind(allRows, fr)
    } else 
      if (makeModelMatrix) 
        m <- rbind(m, fac2sparse(RowGroups(data[, ck, drop = FALSE], returnGroups = FALSE)))
  }
  if (makeNames) {
    rowNames <- MatrixPaste(allRows, sep = sep)
    if (makeModelMatrix) 
      rownames(m) <- rowNames
    if (response) 
      rownames(allSums) <- rowNames
  }
  
  if ((makeModelMatrix) & (!crossTable) & (!response)) 
    return(Matrix::t(m))
  
  if ((!makeModelMatrix) & (!crossTable) & (response)) 
    return(allSums)
  
  if (!crossTable) 
    allRows <- NULL
  
  if (makeModelMatrix) 
    m <- Matrix::t(m) else m <- NULL
  
  if (!response) 
    return(list(modelMatrix = m, crossTable = allRows)) #allSums <- NULL
  
  list(modelMatrix = m, crossTable = allRows, allSums = allSums)
}


#' @rdname FormulaSums
#' @param ... Further arguments to be passed to \code{FormulaSums}
#' @export
Formula2ModelMatrix <-  function(data, formula, dropResponse = TRUE, ...){
  FormulaSums(data=data, formula=formula, dropResponse=dropResponse, ...)
}
