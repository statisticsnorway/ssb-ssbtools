#' Model matrix representing crossed hierarchies according to a formula
#' 
#' How to cross the hierarchies are defined by a formula. The formula is automatically simplified when totals are involved.
#' @param data Matrix or data frame with data containing codes of relevant variables
#' @param hierarchies List of hierarchies, which can be converted by \code{\link{AutoHierarchies}}.
#' Thus, the variables can also be coded by \code{"rowFactor"} or \code{""}, which correspond to using the categories in the data.
#' @param formula A model formula
#' @param inputInOutput Logical vector (possibly recycled) for each element of hierarchies.
#'         TRUE means that codes from input are included in output. Values corresponding to \code{"rowFactor"} or \code{""} are ignored.
#' @param makeColNames Colnames included when TRUE (default).
#' @param crossTable Cross table in output when TRUE
#' @param total Vector of total codes (possibly recycled) used when running \code{\link{Hrc2DimList}} 
#' @param simplify When TRUE (default) the model can be simplified when total codes are found in the hierarchies (see examples).
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}
#' @param unionComplement Logical vector (possibly recycled) for each element of hierarchies.
#'        When TRUE, sign means union and complement instead of addition or subtraction. 
#'        Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored. 
#' @param reOrder When TRUE (default) output codes are ordered in a way similar to a usual model matrix ordering. 
#' @param sep String to separate when creating column names
#'
#' @return A sparse model matrix or a list of two elements (model matrix and cross table)
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # Create some input
#' z <- SSBtoolsData("sprt_emp_withEU")
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#' geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' 
#' # Shorter function name
#' H <- HierarchiesAndFormula2ModelMatrix
#' 
#' # Small dataset example. Two dimensions.
#' s <- z[z$geo == "Spain", ]
#' geoYear <- list(geo = geoDimList, year = "")
#' m <- H(s, geoYear, ~geo * year, inputInOutput = c(FALSE, TRUE))
#' print(m, col.names = TRUE)
#' attr(m, "total")     # Total code 'Europe' is found
#' attr(m, "startCol")  # Two model terms needed
#' 
#' # Another model and with crossTable in output
#' H(s, geoYear, ~geo + year, crossTable = TRUE)
#' 
#' # Three dimensions
#' ageGeoYear <- list(age = ageHier, geo = geoDimList, year = "allYears")
#' m <- H(z, ageGeoYear, ~age * geo + geo * year)
#' head(colnames(m))
#' attr(m, "total")
#' attr(m, "startCol")
#' 
#' # With simplify = FALSE
#' m <- H(z, ageGeoYear, ~age * geo + geo * year, simplify = FALSE)
#' head(colnames(m))
#' attr(m, "total")
#' attr(m, "startCol")
#' 
#' # Compute aggregates
#' m <- H(z, ageGeoYear, ~geo * age, inputInOutput = c(TRUE, FALSE, TRUE))
#' t(m) %*% z$ths_per
#' 
#' # Without hierarchies. Only factors.
#' ageGeoYearFactor <- list(age = "", geo = "", year = "")
#' t(H(z, ageGeoYearFactor, ~geo * age + year:geo))
HierarchiesAndFormula2ModelMatrix <- function(data, hierarchies, formula, inputInOutput = TRUE, makeColNames = TRUE, 
                                              crossTable = FALSE, total = "Total", simplify = TRUE, 
                                              hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                                              unionComplement = FALSE, reOrder = TRUE, sep = "-") {
  nHier <- length(hierarchies)
  nam <- names(hierarchies)
  n <- NROW(data)
  
  if (any(duplicated(nam))) 
    stop("Duplicated hierarchy names. Try to run AutoHierarchies first.")
  
  allVars <- row.names(attr(delete.response(terms(as.formula(formula))), "factors"))
  
  ma <- match(allVars, names(hierarchies))
  if (any(is.na(ma))) 
    stop("Var in formula not in hi")
  
  total <- rep_len(total, nHier)[ma]
  unionComplement <- rep_len(unionComplement, nHier)[ma]
  inputInOutput <- rep_len(inputInOutput, nHier)[ma]
  
  
  hierarchies <- AutoHierarchies(hierarchies = hierarchies[ma], data = data, total = total, hierarchyVarNames = hierarchyVarNames)
  
  nHier <- length(hierarchies)
  
  names(total) <- names(hierarchies) 
  
  if (simplify) {
    totCode <- FindTotCode(hierarchies, data)  #, hierarchyVarNames=hierarchyVarNames) 
    formula <- ReduceFormula(formula, nam[sapply(totCode, length) > 0])
    for (i in 1:nHier) {
      li <- length(totCode[[i]])
      if (li) {
        if (li > 1) 
          warning("More than one total codes. First used")
        total[i] <- totCode[[i]][1]
      }
    }
  }
  termsFormula <- terms(as.formula(formula))
  fac <- attr(delete.response(termsFormula), "factors") != 0
  intercept <- attr(termsFormula, "intercept") != 0
  
  if (crossTable | makeColNames) {
    firstROW <- CharacterDataFrame(data[1, allVars, drop = FALSE])
    firstROW[, ] <- total
    rownames(firstROW) <- NULL
    allRows <- firstROW
    if (intercept) 
      allRows <- firstROW else allRows <- firstROW[integer(0), , drop = FALSE]
  }
  
  if (intercept) 
    m <- Matrix(1, n, 1) else m <- Matrix(0, n, 0)
  
  
  
  if (intercept){ 
    termNames <- c("(Intercept)", colnames(fac)) 
    startCol <- 1L
  } else  {
    termNames <- colnames(fac)
    startCol <- integer(0)
  }
  
  
  
  nFac <- NCOL(fac)
  
  for (k in seq_len(nFac)) {
    startCol <- c(startCol, ncol(m) + 1L)
    fack <- fac[, k]
    hcd <- HierarchyComputeDummy(data = data, hierarchies = hierarchies[fack], inputInOutput = inputInOutput[fack], 
                                 crossTable = (crossTable | makeColNames), unionComplement = unionComplement[fack], 
                                 reOrder = reOrder, makeRownames = FALSE)
    if (crossTable | makeColNames) {
      
      fr <- firstROW[rep(1, NROW(hcd$crossTable)), , drop = FALSE]
      rownames(fr) <- NULL
      fr[, allVars[fack]] <- hcd$crossTable[, allVars[fack], drop = FALSE]
      allRows <- rbind(allRows, fr)
      m <- cbind(m, hcd$modelMatrix)
    } else {
      m <- cbind(m, hcd)
    }
  }
  
  names(startCol) <- termNames 
  
  if (crossTable | makeColNames) {
    rownames(allRows) <- NULL
  }
  
  if (makeColNames) {
    colnames(m) <- apply(allRows, 1, paste, collapse = sep)
  }
  
  attr(m, "total") = total
  attr(m, "startCol") = startCol
  
  
  if (!crossTable) {
    return(m)
  }
  
  list(modelMatrix = m, crossTable = allRows)
}


FindTotCode <- function(hi, data = NULL) {
  hi <- AddMapsInput(hi, data)
  n <- length(hi)
  z <- vector("list", n)
  names(z) <- names(hi)
  for (i in seq_len(n)) {
    if (is.list(hi[[i]])) {
      z[[i]] <- names(FindTotRow(DummyHierarchy(hi[[i]]$mapsFrom, hi[[i]]$mapsTo, 
                hi[[i]]$sign, hi[[i]]$level, mapsInput = attr(hi[[i]], "mapsInput"))))
    } else z[[i]] <- character(0)
  }
  z
}




DummyHi <- function(hi) {
  n <- length(hi)
  z <- vector("list", n)
  for (i in seq_len(n)) {
    if (is.list(hi[[i]])) 
      z[[i]] <- DummyHierarchy(hi[[i]]$mapsFrom, hi[[i]]$mapsTo, hi[[i]]$sign, hi[[i]]$level, 
                               mapsInput = attr(hi[[i]], "mapsInput")) else z[[i]] <- Matrix(0, 0, 0)
  }
  z
}


FindTotRow <- function(x) {
  nc <- ncol(x)
  w1 <- which(rowSums(x) == nc)
  if (length(w1)) {
    z <- x[w1, , drop = FALSE]^2
    w2 <- which(rowSums(z) == nc)
    if (length(w2)) {
      return(w1[w2])
    }
  }
  w1
}


ReduceFormula <- function(formula, tot) {
  termsFormula <- terms(as.formula(formula))
  fac <- attr(delete.response(termsFormula), "factors") != 0
  intercept <- attr(termsFormula, "intercept") != 0
  if (intercept) 
    fac <- cbind(`(Intercept)` = FALSE, fac)
  totHi <- tot
  isTot <- rownames(fac) %in% totHi
  ok <- rep(TRUE, ncol(fac))
  or <- rep(TRUE, ncol(fac))
  for (i in order(colSums(fac), decreasing = TRUE)) {
    or[i] <- FALSE
    if (ok[i]) {
      r <- !(fac[, i] & isTot)
      ork <- which(or & ok)
      if (length(ork)) {
        fack <- fac[r, ork, drop = FALSE]
        faci <- matrix(fac[r, i], nrow(fack), ncol(fack))
        ok[ork[colSums(faci == fack) == sum(r)]] <- FALSE
      }
    }
  }
  termOk <- colnames(fac)[ok]
  ma <- match("(Intercept)", termOk)
  if (is.na(ma)) {
    noi <- "- 1"
  } else {
    termOk <- termOk[-ma]
    noi <- ""
  }
  paste("~", paste(termOk, collapse = " + "), noi)
}







