

#' Ensure standardized coding of hierarchies
#' 
#' Automatic convert list of hierarchies coded in different ways to standardized to-from coding
#' 
#' Input can be to-from coded hierarchies, hierarchies/dimList as in sdcTable, TauArgus coded hierarchies or formulas. 
#' Automatic coding from data is also supported. Output is on a from ready for input to \code{\link{HierarchyCompute}}.
#' \code{FindHierarchies} wraps \code{\link{FindDimLists}} and \code{AutoHierarchies} into a single function.
#' A single string as hierarchy input is assumed to be a total code. 
#' Then, the hierarchy is created as a simple hierarchy where all codes in data sum up to this total.
#' For consistence with \code{HierarchyCompute}, 
#' the codes \code{"rowFactor"} and \code{"colFactor"} are unchanged. 
#' An empty string is recoded to  \code{"rowFactor"}.
#' 
#' 
#'
#' @param hierarchies List of hierarchies
#' @param data Matrix or data frame with data containing codes of relevant variables
#' @param total Within \code{AutoHierarchies}: Vector of total codes (possibly recycled) used when running \code{\link{Hrc2DimList}}.  
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}
#' @param combineHierarchies Whether to combine several hierarchies for same variable into a single hierarchy
#' @param unionComplement Logical vector as in \code{\link{Hierarchies2ModelMatrix}}. The parameter is only in use when hierarchies are combined. 
#' 
#' @seealso \code{\link{DimList2Hierarchy}}, \code{\link{Hierarchy2Formula}}.
#'
#' @return List of hierarchies
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # First, create different types of input
#' z <- SSBtoolsData("sprt_emp_withEU")
#' yearFormula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
#' yearHier <- Formula2Hierarchy(yearFormula)
#' geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' geoDimList2 <- FindDimLists(z[, c("geo", "eu")])[[1]]
#' geoHrc <- DimList2Hrc(geoDimList)
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#' 
#' h1 <- AutoHierarchies(list(age = ageHier, geo = geoDimList, year = yearFormula))
#' h2 <- AutoHierarchies(list(age = "Y15-64", geo = geoHrc, year = yearHier), data = z, 
#'                       total = "Europe")
#' h3 <- AutoHierarchies(list(age = "Total", geo = geoDimList2, year = "Total"), data = z)
#' h4 <- FindHierarchies(z[, c(1, 2, 3, 5)])
#' h5 <- AutoHierarchies(list(age = "Total", geo = "", year = "colFactor"), data = z)
#' identical(h1, h2)
#' identical(h3, h4)
#' 
#' FindHierarchies(z[, c("geo", "eu", "age")])
AutoHierarchies <- function(hierarchies, data = NULL, total = "Total", 
                            hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"),
                            combineHierarchies = TRUE, unionComplement = FALSE) {
  total <- rep_len(total, length(hierarchies))
  namesHierarchies <- names(hierarchies)
  if (is.null(namesHierarchies)) 
    stop("hierarchies must be a named list")
  for (i in 1:length(hierarchies)) {
    hierarchies[[i]] <- AutoHierarchies1(hierarchies[[i]], data = data, total = total[i], hierarchyVarNames = hierarchyVarNames, varName = namesHierarchies[i])
  }
  if (combineHierarchies) {
    dph <- duplicated(names(hierarchies))
    if (any(dph)) {
      hi <- hierarchies
      unionComplement <- rep_len(unionComplement, length(hi))
      hierarchies <- hi[!dph]
      for (i in seq_len(length(hierarchies))) {
        nam <- names(hi) == names(hierarchies)[i]
        if (sum(nam) > 1) 
          hierarchies[[i]] <- CombineHierarchies(hi[nam], unionComplement = unionComplement)
      }
    }
  }
  hierarchies
}

#' @rdname AutoHierarchies
#' @export
FindHierarchies <- function(data, total = "Total") {
  AutoHierarchies(FindDimLists(data, total = total))
}


AutoHierarchies1 <- function(hi, data, total, hierarchyVarNames, varName) {
  if (is.character(hi)) 
    if (length(hi) == 1) {
      if (hi == "") 
        hi <- "rowFactor"
      if (hi %in% c("rowFactor", "colFactor")) 
        return(hi)
      fd <- FindDimLists(data[, varName, drop = FALSE], total = hi)
      # return(list(fd,varName))
      hi <- fd[[match(varName, names(fd))]]
    }
  if (is.character(hi)) 
    if (!any(!grepl("\\=", hi))) 
      hi <- Formula2Hierarchy(hi)
    if (is.character(hi)) 
      hi <- Hrc2DimList(hi, total = total)
    if (!is.data.frame(hi)) 
      stop("Something is wrong")
    if (NCOL(hi) == 2) 
      hi <- DimList2Hierarchy(hi) 
    else 
      hi <- HierarchyFix(hi, hierarchyVarNames)
    hi
}





#' DimList2Hierarchy
#' 
#' From hierarchy/dimList as in sdcTable to to-from coded hierarchy
#'
#' @param x An element of a dimList as in sdcTable
#'
#' @return Data frame with to-from coded hierarchy
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # First generate a dimList element 
#' x <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu")], , total = "Europe")[[1]]
#' 
#' DimList2Hierarchy(x)
#' 
DimList2Hierarchy <- function(x) {
  x <- FixDimListNames(x)
  n <- NROW(x) - 1
  levels <- nchar(x$levels)
  dLevels <- diff(levels)
  z <- data.frame(mapsFrom = rep("", n), mapsTo = rep("", n), sign = rep(1L, n), level = max(levels) - levels[-1] + 1, stringsAsFactors = FALSE)
  
  codes <- x$codes
  parentCodes <- character(0)
  for (i in seq_len(n)) {
    if (dLevels[i] > 0) {
      parentCodes <- c(rep(codes[i], dLevels[i]), parentCodes)
    }
    if (dLevels[i] < 0) {
      parentCodes <- parentCodes[-seq_len(abs(dLevels[i]))]
    }
    
    z$mapsFrom[i] <- codes[i + 1]
    z$mapsTo[i] <- parentCodes[1]
  }
  z
}


FixDimListNames <- function(x) {
  if (!any(!(c("levels", "codes") %in% names(x)))) 
    return(x)
  a <- unique(c(pmatch(c("lev", "cod", "nam"), names(x)), 1:2))
  a <- a[!is.na(a)][1:2]
  names(x)[a] <- c("levels", "codes")
  x
}





#' DimList2Hrc/Hrc2DimList
#' 
#' Conversion between hierarchies/dimList as in sdcTable and TauArgus coded hierarchies
#'
#' @param dimList List of data frames according to the specifications in sdcTable 
#' @param hrc List of character vectors
#' @param total	String used to name totals.
#'
#' @return See Arguments
#' @export 
#' @author Øyvind Langsrud  
#'
#' @examples
#' # First generate dimList
#' dimList <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")])
#' hrc <- DimList2Hrc(dimList)
#' dimList2 <- Hrc2DimList(hrc)
#' identical(dimList, dimList2)
DimList2Hrc <- function(dimList) {
  if (!is.data.frame(dimList[[1]])) 
    return(DimList2Hrc(list(dimList))[[1]])
  RowMake <- function(x) paste(substring(x[1], 3), x[2], sep = "")
  for (i in 1:length(dimList)) dimList[[i]] <- apply(dimList[[i]], 1, RowMake)[-1]
  dimList
}


#' @rdname DimList2Hrc
#' @export
Hrc2DimList <- function(hrc, total = "Total") {
  if (!is.list(hrc)) 
    return(Hrc2DimList(list(hrc), total)[[1]])
  hrc <- as.list(hrc)
  for (i in 1:length(hrc)) {
    hrc[[i]] <- as.character(hrc[[i]])
  }
  for (i in 1:length(hrc)) {
    codes <- gsub("@", "", hrc[[i]])
    n <- as.matrix(c(-1, nchar(hrc[[i]]) - nchar(codes))) + 2
    codes <- c(total, codes)
    levels <- apply(n, 1, function(x) paste(rep("@", x), collapse = ""))
    hrc[[i]] <- data.frame(levels = levels, codes = codes, stringsAsFactors = FALSE)
  }
  hrc
}



#' Hierarchy2Formula
#' 
#' Conversion between to-from coded hierarchy and formulas written with =, - and +.
#'
#' @param x Data frame with to-from coded hierarchy
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}.
#'
#' @return See Arguments
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- SSBtoolsData("sprt_emp_geoHier")
#' s <- Hierarchy2Formula(x)
#' Formula2Hierarchy(s)
#' 
Hierarchy2Formula <- function(x, hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level")) {
  x <- FixHierarchy(x, hierarchyVarNames)
  mapsTo <- unique(x$mapsTo)
  n <- length(mapsTo)
  z <- rep("", n)
  for (i in seq_len(n)) {
    rows <- x$mapsTo == mapsTo[i]
    m <- sum(rows)
    y <- rep("", 2 * m)
    y[2 * seq_len(m)] <- x$mapsFrom[rows]
    y[-1 + 2 * seq_len(m)] <- c("-", "+")[(3 + x$sign[rows])/2]
    if (y[1] == "+") 
      y <- y[-1]
    z[i] <- paste(c(mapsTo[i], "=", y), collapse = " ")
  }
  z
}


#' @rdname Hierarchy2Formula
#' @param s Character vector of formulas written with =, - and +.
#' @export
Formula2Hierarchy <- function(s) {
  s <- gsub("\\+", " + ", s)
  s <- gsub("\\-", " - ", s)
  s <- gsub("\\=", " = ", s)
  s <- strsplit(s, " ")
  s <- lapply(s, function(x) x[!x == ""])
  if (min(sapply(s, length) < 3)) 
    stop("At least 3 elements (including = needed in formula)")
  eq <- sapply(s, function(x) x[2])
  if (unique(eq) != "=") 
    stop("Second element must be =")
  mapsTo <- sapply(s, function(x) x[1])
  s <- lapply(s, function(x) x[-(1:2)])
  s <- lapply(s, function(x) {
    if (!(x[1] %in% c("-", "+"))) 
      x <- c("+", x)
    x
  })
  
  x <- sapply(s, length)
  if (any(x/2 != round(x/2))) 
    stop("Wrong number of elements")
  
  mapsFrom <- lapply(s, function(x) x[2 * seq_len(length(x)/2)])
  sign <- lapply(s, function(x) x[-1 + 2 * seq_len(length(x)/2)])
  
  n <- length(mapsTo)
  z <- vector("list", n)
  
  for (i in seq_len(n)) z[[i]] <- data.frame(mapsFrom = mapsFrom[[i]], mapsTo = mapsTo[i], sign = sign[[i]], stringsAsFactors = FALSE)
  
  HierarchyFix(RbindAll(z))
}




CombineHierarchies <- function(hierarchies, hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), autoLevel = TRUE, unionComplement = FALSE) {
  n <- length(hierarchies)
  m <- vector("list", n)
  rNames <- character(0)
  for (i in seq_len(n)) {
    hi <- HierarchyFix(hierarchies[[i]], hierarchyVarNames, autoLevel)
    m[[i]] <- as.data.frame(as.matrix(DummyHierarchy(hi$mapsFrom, hi$mapsTo, hi$sign, hi$level)))
    rNames <- c(rNames, rownames(m[[i]]))
  }
  m <- as.matrix(RbindAll(m))
  m[is.na(m)] <- 0
  uniqueRows <- RowGroups(m, returnGroupsId = TRUE)$idg
  ok <- TRUE
  if (length(uniqueRows) != length(unique(rNames))) 
    ok <- FALSE
  if (length(uniqueRows) != length(unique(rNames[uniqueRows]))) 
    ok <- FALSE
  if (!ok) 
    stop("Could not combine hierarchies")
  m <- m[uniqueRows, , drop = FALSE]
  rownames(m) <- rNames[uniqueRows]
  HierarchyFix(HierarchyFromDummy(m))
}


HierarchyFromDummy <- function(d) {
  x <- data.frame(mapsFrom = colnames(d)[as.vector(col(d))], mapsTo = rownames(d)[as.vector(row(d))], sign = as.vector(d), stringsAsFactors = FALSE)
  x[x$sign != 0, , drop = FALSE]
}

