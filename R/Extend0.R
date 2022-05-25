
#' Add zero frequency rows
#' 
#' Microdata or tabular frequency data is extended to contain all combinations of unique rows 
#' of (hierarchical) groups of dimensional variables. Extra variables are extended by `NA`'s or `0`'s. 
#' 
#' With no frequency variable in input (microdata), the frequency variable in output  consists of ones and zeros. 
#' By default, all variables, except the frequencies, are considered as dimensional variables.
#' By default, the grouping of dimensional variables is based on hierarchical relationships (`hierarchical = TRUE`).
#' With `varGroups = NULL` and `hierarchical = FALSE`,  
#' each dimensional variable forms a separate group (as `as.list(dimVar)`). 
#' Parameter `extraVar` can be specified as variable names. 
#' `TRUE` means all remaining variables and `FALSE` no variables. 
#' 
#' When the contents of `varGroups[[i]]` is variable names, the data frame `unique(data[varGroups[[i]]])` will be made as a 
#' building block within the function. A possibility is to supply such a data frame instead of variable names.
#' Then, the building block will be `unique(varGroups[[i]])`. Names and data frames can be mixed. 
#'
#' @param data data frame 
#' @param freqName Name of (existing) frequency variable
#' @param hierarchical Hierarchical variables treated atomatically when `TRUE`  
#' @param varGroups List of variable groups, possibly with data (see details and examples).
#' @param dimVar The dimensional variables
#' @param extraVar Extra variables as variable names, TRUE (all remaining) or FALSE (none). 
#'
#' @return Extended data frame
#' @export
#'
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")[c(1, 4:6, 8, 11:15), ]
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' Extend0(z[, -4])
#' Extend0(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"))
#' Extend0(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"), extraVar = "year")
#' Extend0(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"), extraVar = FALSE)
#' Extend0(z, varGroups = list(c("age", "geo", "year"), "eu"))
#' Extend0(MakeFreq(z[c(1, 1, 1, 2, 2, 3:10), -4]))
#' Extend0(z, "ths_per")
#' 
#' # varGroups with data frames (same result as with names above)
#' Extend0(z, varGroups = list(z[c("age", "geo", "year")], z["eu"]))
#' 
#' # varGroups with both names and data frame
#' Extend0(z, varGroups = list(c("year", "geo", "eu"), data.frame(age = c("middle", "old"))))
Extend0 <- function(data, freqName = "freq", hierarchical = TRUE, varGroups = NULL, dimVar = NULL, extraVar = TRUE) {
  
  if (!is.logical(extraVar)) {
    extraVar <- names(data[1, extraVar, drop = FALSE])
    eVar <- extraVar
  } else {
    eVar <- character(0)
  }
  
  if (is.null(dimVar) & is.null(varGroups)) {
    dimVar <- names(data)
    dimVar <- dimVar[!(dimVar %in% c(freqName, eVar))]
  } else {
    if (is.null(varGroups)) {
      dimVar <- names(data[1, dimVar, drop = FALSE])
    }
  }
  
  # Ensure varGroups exists
  if (is.null(varGroups)) {
    if (hierarchical) {
      varGroups <- HierarchicalGroups2(data[dimVar])
    } else {
      varGroups <- as.list(dimVar)
    }
  }
  
  FunctionExtend0 <-  attr(varGroups, "FunctionExtend0") 
  
  # Ensure varGroups exists with data
  for (i in seq_along(varGroups)) {
    if (length(nrow(varGroups[[i]]))) {  # One way to check data.frame without checking class
      varGroups[[i]] <- unique(varGroups[[i]])
    } else {
      varGroups[[i]] <- unique(data[varGroups[[i]]])
    }
  }
  
  # Back to varGroups without data
  varGroupsNames <- lapply(varGroups, names)
  
  dimVarFromVarGroups <- unlist(varGroupsNames)
  
  if (anyDuplicated(dimVarFromVarGroups)) {
    stop("Duplicated names in varGroups.")
  }
  
  if (!is.null(dimVar)) {
    if (!identical(sort(as.vector(dimVar)), sort(as.vector(dimVarFromVarGroups)))) {
      stop("Mismatch between input dimVar and VarGroups")
    }
  } else {
    dimVar <- dimVarFromVarGroups
  }
  
  if (is.logical(extraVar)) {
    if (extraVar) {
      extraVar <- names(data)
      extraVar <- extraVar[!(extraVar %in% c(dimVar, freqName))]
    } else {
      extraVar <- character(0)
    }
  }
  
  if (is.null(FunctionExtend0)) {
    z <- varGroups[[1]]
    for (i in SeqInc(2, length(varGroups))) {
      z <- CrossCodeFrames(z, varGroups[[i]])
    }
  } else {
    z <- FunctionExtend0(data = data, varGroups = varGroups)
  }
  
  
  if (ncol(z) != length(dimVar)) {
    stop("Mismatch between created output and dimVar.")
  }  
  
  ma <- Match(data[dimVar], z[dimVar])
  z[freqName] <- 0L
  newrows <- rep(TRUE, nrow(z))
  newrows[ma] <- FALSE
  z <- z[newrows, , drop = FALSE]
  
  allVar <- names(data)
  allVar <- allVar[allVar %in% c(dimVar, freqName, extraVar)]
  
  if (!(freqName %in% names(data))) {
    data <- data[allVar]
    data[freqName] <- 1L
    allVar <- c(allVar, freqName)
  }
  
  if (!nrow(z)) {
    if (identical(names(data), allVar)) {
      return(data)
    } else {
      return(data[allVar])
    }
  }
  
  if (length(extraVar)) {
    extraVar1 <- data[1, extraVar, drop = FALSE]
    for (i in seq_along(extraVar1)) {
      if (is.numeric(extraVar1[1, i])) {
        extraVar1[1, i] <- 0L
      } else {
        extraVar1[1, i] <- NA
      }
    }
    z <- cbind(z, extraVar1)
  }
  
  if (identical(names(data), allVar)) {
    z <- rbind(data, z[allVar])
  } else {
    z <- rbind(data[allVar], z[allVar])
  }
  
  rownames(z) <- NULL
  z
}
