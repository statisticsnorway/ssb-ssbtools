

#' Ensure standardized coding of hierarchies
#' 
#' Automatic convert list of hierarchies coded in different ways to standardized to-from coding
#' 
#' Input can be to-from coded hierarchies, hierarchies/dimList as in sdcTable, TauArgus coded hierarchies or formulas. 
#' Automatic coding from data is also supported. Output is on a from ready for input to \code{\link{HierarchyCompute}}.
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
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}. However:
#'   - `level` is by default not required (see `autoLevel` below).
#'   - If the `sign` variable is missing, it defaults to a variable of 1s.
#'   - Common 'from-to' variable names are recognized (see `autoNames` below).    
#' @param combineHierarchies Whether to combine several hierarchies for same variable into a single hierarchy (see examples).
#' @param unionComplement Logical vector as in \code{\link{Hierarchies2ModelMatrix}}. The parameter is only in use when hierarchies are combined. 
#' @param autoLevel When TRUE (default), the level is computed automatically, ignoring the input level variable. 
#'                  This parameter is passed to \code{\link{HierarchyFix}}..
#' @param autoNames Named character vector of 'from-to' variable names to be automatically recognized.
#'                  These names do not need to be specified in `hierarchyVarNames`.
#'                  Thus, `autoNames` can serve as an alternative to `hierarchyVarNames`.
#'                                   
#' @param ... Extra unused parameters
#' 
#' @seealso \code{\link{FindHierarchies}}, \code{\link{DimList2Hierarchy}}, \code{\link{DimList2Hrc}}, 
#'          \code{\link{Hierarchy2Formula}}, \code{\link{DummyHierarchies}}.
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
#' # Print the resulting hierarchies
#' h1 # = h2
#' h3 # = h4
#' h5
#' 
#' FindHierarchies(z[, c("geo", "eu", "age")])
#' 
#' 
#' # ===================================================================== 
#' #   Examples illustrating the combineHierarchies parameter
#' # =====================================================================
#' 
#' # First, create data
#' d <- SSBtoolsData("d2ws")[1:3]
#' d$isCounty1 <- "NO"
#' d$isCounty1[d$county == "county-1"] <- "YES"
#' d
#' 
#' # sdcTable coding showing two tree-shaped hierarchies
#' dimList <- FindDimLists(d)
#' dimList
#' 
#' # Two tree-shaped hierarchies can still be seen 
#' # Hierarchies with three and two levels
#' hA <- AutoHierarchies(dimList, combineHierarchies = FALSE)
#' hA
#' 
#' # A single hierarchy with only one level 
#' # Contains the information needed to create a dummy matrix
#' hB <- AutoHierarchies(dimList)
#' hB
#' 
#' # Dummy matrices from the hierarchies
#' DummyHierarchies(hA)
#' DummyHierarchies(hB)
#' 
#' 
#' # ===================================================================== 
#' #   Special examples with character vector(s) as unnamed list elements
#' # =====================================================================
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
                            combineHierarchies = TRUE, unionComplement = FALSE,
                            autoLevel = TRUE, 
                            autoNames = c(to = "from", parentCode = "code", parent = "child", root = "leaf"),
                            ...) {
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
    hierarchies[[i]] <- AutoHierarchies1(hierarchies[[i]], data = data, total = total[i], hierarchyVarNames = hierarchyVarNames, varName = namesHierarchies[i], 
                                         autoLevel = autoLevel, autoNames = autoNames)
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



AutoHierarchies1 <- function(hi, data, total, hierarchyVarNames, varName, 
                             autoLevel, autoNames) {
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
    if (NCOL(hi) == 2) { 
      if ("levels" %in% names(hi)) {
        if (identical(unique(unique(strsplit(hi$levels, NULL)[[1]])), "@")) {
          return(DimList2Hierarchy(hi))
        }
      }
    }
    
    hi <- FromToHierarchy(as.data.frame(hi), 
                          hierarchyVarNames,
                          fromCodes =   autoNames,
                          toCodes = names(autoNames))
    
    hi <- HierarchyFix(hi, hierarchyVarNames, autoLevel = autoLevel)
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



FromToHierarchy <- function(hi, hierarchyVarNames, fromCodes, toCodes) {
  
  mapsTo <- hierarchyVarNames["mapsTo"]
  mapsFrom <- hierarchyVarNames["mapsFrom"]
  
  if (!any(c(mapsFrom, mapsTo) %in% names(hi))) {
    
    ma1 <- match(tolower(names(hi)), tolower(fromCodes))
    ma2 <- match(tolower(names(hi)), tolower(toCodes))
    fromCol <- which(!is.na(ma1))
    toCol <- which(!is.na(ma2))
    ma1 <- ma1[fromCol]
    ma2 <- ma2[toCol]
    
    if (length(ma1 == 1) & identical(ma1, ma2)) {
      names(hi)[fromCol] <- mapsFrom
      names(hi)[toCol] <- mapsTo
    } else {
      stop("Interpreting 'from-to' variable names failed.")
    }
  }
  
  if (!(hierarchyVarNames["sign"] %in% names(hi))) {
    if ("sign" %in% tolower(names(hi))) {
      stop("suspicious sign column found. See parameter hierarchyVarNames.")
    } else {
      hi[hierarchyVarNames["sign"]] <- 1L
    }
  }
  
  
  equalFromTo <- hi[[mapsFrom]] == hi[[mapsTo]]
  
  if (any(equalFromTo, na.rm = TRUE)) {
    hi[[mapsTo]][equalFromTo] <- NA
    warnText <- "Codes removed due to 'to'=='from' or 'to'== NA:"
  } else {
    warnText <- "Codes removed due to NAs in the 'to' variable:"
  }
  
  
  if (anyNA(hi[[mapsTo]])) {
    uniqueCodes <- unique(c(hi[[mapsFrom]], hi[[mapsTo]]))
    rows <- !is.na(hi[[mapsTo]])
    hi <- hi[rows, , drop = FALSE]
    
    diffCodes <- setdiff(uniqueCodes, unique(c(hi[[mapsFrom]], hi[[mapsTo]])))
    diffCodes <- diffCodes[!is.na(diffCodes)]
    
    if (length(diffCodes)) {
      warning(paste(warnText, paste(diffCodes, collapse = ", ")))
    }
  }
  
  hi
  
}

  
  
  
  

