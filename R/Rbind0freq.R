if(FALSE){
  
  z <- SSBtoolsData("sprt_emp_withEU")[c(1,4:6,8,11:15), ]
  z$age[z$age == "Y15-29"] <- "young"
  z$age[z$age == "Y30-64"] <- "old"
  
  Rbind0freq(z[, -4])  
  Rbind0freq(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"))  
  Rbind0freq(z, varGroups = list( c("age", "geo", "year"), "eu"))
  Rbind0freq(MakeFreq(z[c(1,1,1,2,2,3:10), -4]))
  Rbind0freq(z, "ths_per")  
  
}







#' Add zero frequency rows
#' 
#' Microdata or tabular frequency data is extended 
#'
#' @param data data frame 
#' @param freqName Name of (existing) frequency variable
#' @param hierarchical Hierarchical variables treated atomatically when `TRUE`  
#' @param varGroups List of variable groups
#' @param dimVar The dimensional variables
#'
#' @return Extended data frame
#' @export
#'
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")[c(1, 4:6, 8, 11:15), ]
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' Rbind0freq(z[, -4])
#' Rbind0freq(z, hierarchical = FALSE, dimVar = c("age", "geo", "eu"))
#' Rbind0freq(z, varGroups = list(c("age", "geo", "year"), "eu"))
#' Rbind0freq(MakeFreq(z[c(1, 1, 1, 2, 2, 3:10), -4]))
#' Rbind0freq(z, "ths_per")
Rbind0freq <- function(data, freqName = "freq", hierarchical = TRUE, varGroups = NULL, dimVar = NULL) {
  
  if (is.null(dimVar)) {
    dimVar <- names(data)
    dimVar <- dimVar[!(dimVar %in% freqName)]
  } else {
    dimVar <- names(data[1, dimVar, drop = FALSE])
  }
  
  if (is.null(varGroups)) {
    if (hierarchical) {
      varGroups <- HierarchicalGroups2(data[dimVar])
    } else {
      varGroups <- as.list(dimVar)
    }
  } else {
    dimVar <- dimVar[dimVar %in% unlist(varGroups)]
  }
  
  if (anyDuplicated(unlist(varGroups))) {
    stop("Problematic varGroups")
  }
  
  z <- unique(data[, varGroups[[1]], drop = FALSE])
  for (i in SeqInc(2, length(varGroups))) {
    x <- unique(data[, varGroups[[i]], drop = FALSE])
    z <- CrossCodeFrames(z, x)
  }
  
  if (ncol(z) != length(dimVar)) {
    stop("Something is wrong")
  }
  
  z <- z[dimVar]
  
  ma <- Match(data[dimVar], z)
  z[freqName] <- 0L
  newrows <- rep(TRUE, nrow(z))
  newrows[ma] <- FALSE
  z <- z[newrows, , drop = FALSE]
  
  if (!(freqName %in% names(data))) {
    data <- data[dimVar]
    data[freqName] <- 1L
  }
  
  z <- rbind(data[names(z)], z)
  rownames(z) <- NULL
  z
}
