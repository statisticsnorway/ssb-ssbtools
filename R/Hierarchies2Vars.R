

#' Hierarchies coded as variables 
#' 
#' The hierarchical relations are stored as minimal datasets
#'
#' @param hierarchies List of hierarchies on the same form as input to \code{\link{AutoHierarchies}}
#' @param singleVars When `TRUE`, a single variable is created for all codes except the input codes. 
#' @param fromDummy Logical value indicating the method for handling hierarchies.
#'   - When `TRUE`, the algorithm uses dummy-coded hierarchies.
#'   - When `FALSE`, the algorithm works directly on hierarchies standardized by `AutoHierarchies`, which often results in well-structured output variables.
#'   - When `NA` (default), the algorithm first attempts the `FALSE` method; if this is not feasible, it falls back to the `TRUE` method.
#' @param dummyReorder  When `TRUE`, the dummy-coded hierarchies are reordered, potentially improving the structure of output variables.  
#' @param nameFunction  A function that defines how to name all columns except the first. 
#'                      The input consists of the hierarchy name (identical to the first column’s name, `name`) 
#'                      and the column number minus 1 (`level`).
#' @param ...  Further parameters sent to \code{\link{AutoHierarchies}} 
#'
#' @return Named list of data frames 
#' @seealso \code{\link{Vars2Hierarchies}}
#' @export
#'
#' @examples
#' 
#' # Examples based on those from AutoHierarchies
#' # You may also try converting other examples from AutoHierarchies
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' yearFormula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
#' geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#' 
#' Hierarchies2Vars(list(age = ageHier, geo = geoDimList, year = yearFormula))
#' Hierarchies2Vars(list(age = ageHier, geo = geoDimList, year = yearFormula), singleVars = TRUE)
#' 
#' 
#' # NAs are included in data when necessary
#' # Order may affect the results. In this case, the second formula gives better results.
#' Hierarchies2Vars(list(f1 = c("AB = A + B", "CD = C + D", "AC = A + C", "ABCD = AB + CD"),
#'                       f2 = c("AB = A + B", "AC = A + C", "CD = C + D", "ABCD = AB + CD")))
#' 
#' 
Hierarchies2Vars <- function(hierarchies, 
                             singleVars = FALSE, 
                             fromDummy = NA, 
                             dummyReorder = TRUE,
                             nameFunction = function(name, level) paste0(name, "_level_", level),
                             ...) {
  
  if (singleVars) {
    fromDummy <- TRUE
  }
  
  # fromDummy = NaN is hack to print messages 
  if (is.nan(fromDummy)) {
    message_here <- message 
  } else {
    message_here <- function(x) NULL
  }
  
  autoHierarchies <- AutoHierarchies(hierarchies = hierarchies, ...)
  dummyHierarchies <- DummyHierarchies(autoHierarchies)
  
  # Reorder can lead to smarter/less output variables
  if (dummyReorder) {
    dummyHierarchies <- DummyReorder(dummyHierarchies, 
                                     autoHierarchies, 
                                     message = message_here)
  }
 
  # list of FALSE better than list of NULL, since vars[[i]] = NULL not working as expected 
  vars <- as.list(rep(FALSE, length(autoHierarchies)))
  names(vars) <- names(autoHierarchies)
 
  
  for (i in seq_along(autoHierarchies)) {
    
    if (isFALSE(fromDummy) | is.na(fromDummy)) {
      vars[[i]] <- NiceHierarchy2Vars(dummyHierarchies[[i]], 
                                      autoHierarchies[[i]],
                                      message = message_here)
      if (isFALSE(fromDummy) & isFALSE(vars[[i]])) {
        stop("FALSE fromDummy not working")
      }
    }
    if (isFALSE(vars[[i]])) {
      vars[[i]] <- Dummy2Vars(dummyHierarchies[[i]], singleVars = singleVars)
    }
    names(vars[[i]])[1] <- names(vars)[i]
    if (!singleVars) {
      for (j in seq_len(ncol(vars[[i]]) - 1)) {
        names(vars[[i]])[j+1] <- nameFunction(names(vars)[i], j)
      }
    }
  }
  
  vars
}





#' Transform hierarchies coded as Variables to "to-from" format 
#' 
#' A kind of reverse operation of \code{\link{Hierarchies2Vars}}
#'
#' @param hierarchiesAsVars As output from \code{\link{Hierarchies2Vars}}
#'
#' @return List of hierarchies
#' 
#' @export
#'
#' @examples
#' 
#' a <- Hierarchies2Vars(list(f1 = 
#'        c("AB = A + B", "CD = C + D", "AC = A + C", "ABCD = AB + CD")))
#' a
#' 
#' Vars2Hierarchies(a)
#' 
Vars2Hierarchies <- function(hierarchiesAsVars) {
  if (any(!sapply(hierarchiesAsVars, is.data.frame))) {
    stop("Input must be a list of data frames")
  }
  if (any(sapply(hierarchiesAsVars, function(x) anyNA(x[[1]])))) {
    stop("The first column cannot have missing values")
  }
  lapply(hierarchiesAsVars, Vars2Hierarchies1)
}



Vars2Hierarchies1 <- function(a) {
  
  z <- data.frame(mapsFrom = character(0), mapsTo = character(0), 
                  sign = integer(0), level = integer(0))
  
  for (i in SeqInc(2, ncol(a))) {
    x <- a[c(1, i)]
    x <- x[!is.na(x[[2]]), , drop = FALSE]
    names(x) <- c("mapsFrom", "mapsTo")
    z <- rbind(z, cbind(x, sign = 1L, level = i - 1L))
  }
  rownames(z) <- NULL
  z
}





NiceHierarchy2Vars <- function(dummyHierarchy, autoHierarchy, message) {
  
  if (any(duplicated(autoHierarchy$mapsFrom))) {
    message(("duplicated(autoHierarchy$mapsFrom"))
    return(FALSE)
  }
  
  uniqueAuto2 <- unique(autoHierarchy[c("mapsTo", "level")])
  if (any(duplicated(uniqueAuto2$mapsTo))) {
    message("duplicated(uniqueAuto2$mapsTo")
    return(FALSE)
  }
  
  flat <- HierarchyFromDummy(dummyHierarchy)
  if (any(flat$sign != 1)) {
    message("any(flat$sign != 1)")
    return(FALSE)
  }
  
  ma <- match(flat$mapsTo, uniqueAuto2$mapsTo)
  flat$levelOriginal <- uniqueAuto2$level[ma]
  for (j in 1:max(flat$levelOriginal)) {
    if (any(duplicated(flat$mapsFrom[flat$levelOriginal == j]))) {
      message("duplicated(flat$mapsFrom[flat$levelOriginal == j])")
      return(FALSE)
    }
  }
  
  x <- unique(flat["mapsFrom"])
  for (i in seq_len(max(flat$levelOriginal))) {
    ma <- match(flat[flat$levelOriginal == i, "mapsFrom"], x$mapsFrom)
    y <- data.frame(mapsTo = rep(NA, nrow(x)))
    y[ma, "mapsTo"] <- flat[flat$levelOriginal == i, "mapsTo"]
    x <- cbind(x, y)
  }
  rownames(x) <- NULL
  x
}

# firstname used her, but in pratice changed later
Dummy2Vars <- function(dummy, singleVars = FALSE, firstname = "INPUT") {
  if (!all(unique(As_TsparseMatrix(dummy)@x) %in% c(0, 1))) {
    stop("Only 0 and 1 allowed in dummy matrix")
  }
  
  x <- t(dummy)
  n <- nrow(x)
  z <- vector("list", ncol(x) + 1)
  z[[1]] <- rownames(x)
  usez <- rep(FALSE, length(z))
  usez[1] <- TRUE
  colnames_x <- colnames(x)
  names_z <- c(firstname, colnames(x))
  names(z) <- names_z
  
  check <- FALSE
  for (i in seq_len(ncol(x))) {
    xi1 <- x[, i] == 1
    if (check) {
      if (any(!is.na(z[[names_z[j]]][xi1]))) {
        check <- FALSE
      }
    }
    if (!check) {
      j <- i + 1
      usez[j] <- TRUE
      z[[names_z[j]]] <- rep(NA, n)
      check <- !singleVars
    }
    z[[names_z[j]]][xi1] <- colnames_x[i]
  }
  if (singleVars) {
    return(as.data.frame(z))
  }
  as.data.frame(z[usez])
  
}

DummyReorder <- function(dummyHierarchies, autoHierarchies, message) {
  for (i in seq_along(dummyHierarchies)) {
    dummyHierarchies[[i]] <- DummyReorder1(dummyHierarchies[[i]], 
                                           autoHierarchies[[i]],  
                                           message = message)
  }
  dummyHierarchies
}


DummyReorder1 <- function(dummyHierarchy, autoHierarchy, message) {
  dummyHierarchy <<- dummyHierarchy
  autoHierarchy <<- autoHierarchy 
  if (!any(diff(autoHierarchy$level) < 0) | any(autoHierarchy$sign < 0)) {
    ord <- match(unique(autoHierarchy$mapsTo), rownames(dummyHierarchy))
    sum1 <- sum(rowSums(dummyHierarchy) * seq_len(nrow(dummyHierarchy)))
    sum2 <- sum(rowSums(dummyHierarchy)[ord] * seq_len(nrow(dummyHierarchy)))
    if (sum2 >= sum1) {
      dummyHierarchy <- dummyHierarchy[ord, , drop = FALSE]
      message("reorder")
    } else {
      message(":")
    }
  }
  dummyHierarchy
}


 


