

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
#' A special possibility is to include character vector(s) as unnamed list element(s) of `hierarchies`. 
#' Then the elements of the character vector(s) must be variable names within data. 
#' This will cause hierarchies to be created from selected data columns by running \code{\link{FindDimLists}}. 
#' Total coded can be specified by parameter `total` or by naming the character vector. See examples. 
#'
#' @param hierarchies List of hierarchies
#' @param data Matrix or data frame with data containing codes of relevant variables
#' @param total Within \code{AutoHierarchies}: Vector of total codes (possibly recycled) used when running \code{\link{Hrc2DimList}} or \code{\link{FindDimLists}}.  
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}
#' @param combineHierarchies Whether to combine several hierarchies for same variable into a single hierarchy
#' @param unionComplement Logical vector as in \code{\link{Hierarchies2ModelMatrix}}. The parameter is only in use when hierarchies are combined. 
#' @param ... Extra unused parameters
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
#' 
#' 
#' ### Examples with character vector(s) as unnamed list elements.  
#' 
#' # Same output as FindHierarchies above
#' AutoHierarchies(list(c("geo", "eu", "age")), data = z)
#' 
#' # Now combined with a named list element 
#' AutoHierarchies(list(year = yearHier, c("geo", "eu", "age")), data = z)
#' 
#' # Total codes by unnamed list element as named character vector 
#' AutoHierarchies(list(year = yearHier, c(Europe = "geo", "eu", All = "age")), data = z)
#' 
#' # Two types of year input. Total codes by using the parameter `total`. 
#' AutoHierarchies(list("year", year = yearHier, c("geo", "eu", "age")), data = z, 
#'                 total = c("allYears", "unused", "Tot"))
#' 
#' # Avoid combineHierarchies to see effect of each year input separately 
#' # (even earlier return possible with `combineHierarchies = NA`)
#' AutoHierarchies(list("year", year = yearHier, c("geo", "eu", "age")), data = z, 
#'                 total = c("allYears", "unused", "Tot"), combineHierarchies = FALSE)
#' 
AutoHierarchies <- function(hierarchies, data = NULL, total = "Total", 
                            hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"),
                            combineHierarchies = TRUE, unionComplement = FALSE, ...) {
  total <- rep_len(total, length(hierarchies))
  
  if (is.null(names(hierarchies))) {
    names(hierarchies) <- rep(NA, length(hierarchies))
  }
  toFindDimLists <- (names(hierarchies) %in% c(NA, "")) & (sapply(hierarchies, is.character))
  if (any(toFindDimLists)) {
    if (is.null(data)) {
      stop("data input needed")
    }
    nHierarchies <- rep(1, length(hierarchies))
    newDimLists <- vector("list", sum(toFindDimLists))
    indNewDimLists <- which(toFindDimLists)
    for (i in seq_along(newDimLists)) {
      total_i <- total[indNewDimLists[i]]
      names_i <- names(hierarchies[[indNewDimLists[i]]])
      if (!is.null(names_i)) {
        ok_names <- !(names_i %in% c(NA, ""))
        if (!all(!ok_names)) {
          total_i <- rep(total_i, length(hierarchies[[indNewDimLists[i]]]))
          total_i[ok_names] <- names_i[ok_names]
        }
      }
      newDimLists[[i]] <- FindDimLists(data[, hierarchies[[indNewDimLists[i]]], drop = FALSE], total = total_i)
      nHierarchies[indNewDimLists[i]] <- length(newDimLists[[i]])
    }
    indHierarchies <- rep(seq_len(length(hierarchies)), nHierarchies)
    total <- total[indHierarchies]
    hierarchies <- hierarchies[indHierarchies]
    for (i in seq_along(newDimLists)) {
      hierarchies[indHierarchies == indNewDimLists[i]] <- newDimLists[[i]]
      names(hierarchies)[indHierarchies == indNewDimLists[i]] <- names(newDimLists[[i]])
    }
    if (is.na(combineHierarchies)) {  # Early return hack 
      return(hierarchies)
    }
  }
  
  namesHierarchies <- names(hierarchies)
  if (any(names(hierarchies) %in% c(NA, ""))) 
    stop("Unnamed elements of hierarchies could not be automatically handled (try `combineHierarchies = NA`)")
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



AutoHierarchies1 <- function(hi, data, total, hierarchyVarNames, varName) {
  if (is.character(hi)) 
    if (length(hi) == 1) if(!grepl("\\=", hi)) {
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
  
  
  
  rg <- RowGroups(data.frame(r_Na_Me_s_ = rNames,m,stringsAsFactors = FALSE))
  #rg <- RowGroups(m)
  #rg <- RowGroups(m, returnGroupsId = TRUE)
  #uniqueRows <- rg$idg
  
  
  ok <- TRUE
  
  selectedRows <- !duplicated(rNames)
  if(Nlevels(rg[selectedRows]) != Nlevels(rg))  
    ok <- FALSE # Same name used for different m rows
  

  #if (length(uniqueRows) != length(unique(rNames))) 
  #  ok <- FALSE
  #if (length(uniqueRows) != length(unique(rNames[uniqueRows]))) 
  #  ok <- FALSE
  
  if (!ok) 
    stop("Could not combine hierarchies")
  m <- m[selectedRows, , drop = FALSE]
  rownames(m) <- rNames[selectedRows]
  HierarchyFix(HierarchyFromDummy(m))
}


HierarchyFromDummy <- function(d) {
  x <- data.frame(mapsFrom = colnames(d)[as.vector(col(d))], mapsTo = rownames(d)[as.vector(row(d))], sign = as.vector(d), stringsAsFactors = FALSE)
  x[x$sign != 0, , drop = FALSE]
}

