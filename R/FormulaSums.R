#' Sums (aggregates) and/or sparse model matrix with possible cross table
#'
#' By default this function return sums if the formula contains a response part and a model matrix otherwise
#'
#' The model matrix is constructed by calling fac2sparse() repeatedly. The sums are computed by calling aggregate() repeatedly.
#' Hierarchical variables handled when constructing cross table.
#' Column names constructed from the cross table.
#' The returned model matrix includes the attribute \code{startCol} (see last example line).
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
#' @param avoidHierarchical Whether to avoid treating of hierarchical variables. Instead of logical, variables can be specified.  
#' @param includeEmpty  When `TRUE`, empty columns of the model matrix (only zeros) are included. 
#'                      This is not implemented when a response term is included in the formula and `dropResponse = FALSE` (error will be produced).  
#' @param NAomit When `TRUE`, NAs in the grouping variables are omitted in output and not included as a separate category. 
#'               Technically, this parameter is utilized through the function \code{\link{RowGroups}}.
#' @param rowGroupsPackage Parameter `pkg` to the function \code{\link{RowGroups}}.          
#' @param ... Extra unused parameters
#'
#' @return
#'   A matrix of sums, a sparse model matrix or a list of two or three elements (model matrix and cross table and sums when relevant).
#'   
#' @importFrom stats aggregate as.formula delete.response terms
#' @importFrom Matrix fac2sparse
#' @importFrom utils flush.console
#' 
#' @seealso \code{\link{ModelMatrix}}
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
#' m <- Formula2ModelMatrix(x, ~ year*geo + year*eu)
#' print(m[1:3, ], col.names = TRUE)
#' attr(m, "startCol")
FormulaSums <- function(data, formula, makeNames = TRUE, crossTable = FALSE, total = "Total", printInc = FALSE, 
                        dropResponse = FALSE, makeModelMatrix = NULL, sep = "-", sepCross = ":", 
                        avoidHierarchical = FALSE, 
                        includeEmpty = FALSE, 
                        NAomit = TRUE,
                        rowGroupsPackage = "data.table",
                        ...) {
  
  hg <- NULL  # Possible input in a future version
 
  if (is.logical(avoidHierarchical)) {
    if (avoidHierarchical) {
      avoidHierarchical <- seq_len(ncol(data))
    } else {
      avoidHierarchical <- NULL
    }
  }
  
  termsFormula <- terms(as.formula(formula))
  
  intercept <- attr(termsFormula, "intercept") != 0
  
  if (dropResponse) 
    response <- FALSE 
  else 
    response <- attr(termsFormula, "response") != 0
  
  if(response & includeEmpty){
    stop("'includeEmpty = TRUE' with response is not implemented")
  }
  
  
  if (is.null(makeModelMatrix)) 
    makeModelMatrix <- !response
  
  attr_startCol <- makeModelMatrix
  
  fac <- attr(delete.response(termsFormula), "factors") != 0
  faccol <- match(rownames(fac), colnames(data))
  
  if (is.null(hg)){
    if (is.null(avoidHierarchical)){
      hg <- HierarchicalGroups3(data[, faccol, drop = FALSE])
    } else {
      avoidvar <- colnames(data[1, avoidHierarchical, drop=FALSE])
      avoidcol <- match(avoidvar, colnames(data))
      faccol_a <- faccol[!(faccol %in% avoidcol)]
      if(length((faccol_a))){
        hg_a <- HierarchicalGroups3(data[, faccol_a, drop = FALSE])
        faccol_aa <- which(!(faccol %in% avoidcol))
        for(i in seq_along(hg_a)){
          hg_a[[i]] <- faccol_aa[hg_a[[i]]] 
        }
      } else {
        hg_a <- NULL
      }
      avoidcol_b <- match(avoidcol, faccol)
      hg_b <- as.list(avoidcol_b[!is.na(avoidcol_b)])
      names(hg_b) <- avoidvar[!is.na(avoidcol_b)]
      hg <- SortNrList(c(hg_a, hg_b))
    }
  }
  
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
  
  if (attr_startCol) {
    # Copy from HierarchiesAndFormula2ModelMatrix
    if (intercept) {
      termNames <- c("(Intercept)", colnames(fac))
      startCol <- 1L
    } else {
      termNames <- colnames(fac)
      startCol <- integer(0)
    }
  }
  
  nFac <- NCOL(fac)
  
  entries <- rep(nrow(data), nFac + as.integer(intercept))
  if (NAomit) {
    faccolNA <- rep(NA, length(faccol))
    for (i in seq_along(faccolNA)) {
      faccolNA[i] <- anyNA(data[, faccol[i]])
    }
    for (k in seq_len(nFac)) {
      if (any(faccolNA[fac[, k]])) {
        rowNA <- FALSE
        for (i in faccol[fac[, k]][faccolNA[fac[, k]]]) {
          rowNA <- rowNA | is.na(data[, i])
        }
        entries[k] <- entries[k] - sum(rowNA)
      }
    }
  }
  entries <- sum(entries)
  if (entries > .Machine$integer.max) {
    stop(paste("A matrix of", entries, "nonzero entries cannot be created. Limit is 2^31-1."))
  }
  
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
  
  for (k in seq_len(nFac)) {
    if (attr_startCol) {
      startCol <- c(startCol, nrow(m) + 1L)
    }
                  
    if (printInc) 
      if (k%%max(1, round(nFac/10)) == 0) {
        cat(".")
        flush.console()
      }
    ck <- faccol[fac[, k]]
    
    if (makeNames | crossTable | response) 
      rg <- RowGroups(data[, ck, drop = FALSE], 
                      returnGroups = TRUE, 
                      NAomit = NAomit, 
                      pkg = rowGroupsPackage)
    
    if (response) {
      rg1RowGroups735345 <- rg[[1]]
      allSums <- rbind(allSums, as.matrix(aggregate(aggFormula, data, sum)[, -1, drop = FALSE]))
    }
    
    if (makeNames | crossTable) {
      rg1 <- rg[[1]]
      ur <- rg[[2]]
      ur <- CharacterDataFrame(ur)
      hgcolick <- hgcoli[ck]
      if(includeEmpty){
        varGroups <- NULL
        if (any(duplicated(hgcolick))){
          for (ick in unique(hgcolick)){ 
            varGroups <- c(varGroups, list(names(ur)[hgcolick == ick]))
          }
        } 
        ur <- Extend0(ur, freqName = "fRe_Q_u_r", hierarchical = FALSE, varGroups = varGroups)[, names(ur), drop = FALSE]
        uridx <- SortRows(ur, index.return = TRUE)
        ur <- ur[uridx, , drop = FALSE]
        rg1 <- factor(rg1, levels = uridx)
      }
      ur <- as.matrix(ur)
      fr <- firstROW[rep(1, NROW(ur)), , drop = FALSE]
      if (!any(duplicated(hgcolick))) 
        fr[, hgcoli[ck]] <- ur else {
          for (ick in unique(hgcolick)) fr[, ick] <- MatrixPaste(ur[, hgcolick == ick, drop = FALSE], sep = sepCross)
        }
      allRows <- rbind(allRows, fr)
      if (makeModelMatrix) {
        #m <- rbind(m, fac2sparse(rg[[1]])) 
        m <- rbind(m, fac2sparse(rg1, drop.unused.levels = FALSE)) 
      }
    } else 
      if (makeModelMatrix) 
        m <- rbind(m, fac2sparse(RowGroups(data[, ck, drop = FALSE], 
                                           returnGroups = FALSE, 
                                           NAomit = NAomit, 
                                           pkg = rowGroupsPackage)))
  }
  
  
  if (makeNames) {
    rowNames <- MatrixPaste(allRows, sep = sep)
    if (makeModelMatrix) 
      rownames(m) <- rowNames
    if (response) 
      rownames(allSums) <- rowNames
  }
  
  if (attr_startCol) {
    names(startCol) <- termNames
  }
  
  # Possible to check entries calculation
  if (FALSE) if (makeModelMatrix) {
    if (entries != sum(m != 0)) {
      stop("Wrong entries calculation")
    }
  }
  
  if ((makeModelMatrix) & (!crossTable) & (!response)) {
    if (attr_startCol) {
      m <- Matrix::t(m)
      attr(m, "startCol") <- startCol
      return(m)
    }
    return(Matrix::t(m))
  }
  
  if ((!makeModelMatrix) & (!crossTable) & (response)) 
    return(allSums)
  
  if (!crossTable) 
    allRows <- NULL
  
  if (makeModelMatrix) {
    m <- Matrix::t(m)
    if (attr_startCol) {
      attr(m, "startCol") <- startCol
    }
  } else {
    m <- NULL
  }
  
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
