


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
  z[ z$mapsFrom!=z$mapsTo, ,drop=FALSE]
}


FixDimListNames <- function(x) {  # CharacterDataFrame also
  x <- CharacterDataFrame(x)
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
#' # Demonstrate Hierarchies2Formulas and problems 
#' hi <- FindHierarchies(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")])
#' Hierarchies2Formulas(hi) # problematic formula since minus sign in coding 
#' AutoHierarchies(Hierarchies2Formulas(hi)) # Not same as hi because of problems 
#' 
#' # Change coding to avoid problems 
#' hi$age$mapsFrom <- gsub("-", "_", hi$age$mapsFrom)
#' Hierarchies2Formulas(hi)
#' AutoHierarchies(Hierarchies2Formulas(hi))
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


#' @rdname Hierarchy2Formula
#' @param ... Extra parameters. Only `hierarchyVarNames` is relevant.
#' @export
#' @note `Hierarchies2Formulas` is a wrapper for `lapply(x, Hierarchy2Formula, ...)` 
Hierarchies2Formulas <- function(x, ...) {
  lapply(x, Hierarchy2Formula, ...)
}