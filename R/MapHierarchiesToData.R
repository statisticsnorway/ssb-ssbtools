
#' Add variables to dataset based on hierarchies
#' 
#' Uses \code{\link{hierarchies_as_vars}} to transform hierarchies, followed by mapping to the dataset.
#'
#' @param data A data frame containing variables with names matching the names of the hierarchies.
#' @inheritParams hierarchies_as_vars
#' @param ... Further parameters sent to \code{\link{hierarchies_as_vars}} 
#'
#' @return Input `data` with extra Variables
#' @export
#'
#' @examples
#' 
#' # Examples similar those from hierarchies_as_vars
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' yearFormula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
#' geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#' 
#' MapHierarchiesToData(z, list(age = ageHier, geo = geoDimList, year = yearFormula))
#' 
#' MapHierarchiesToData(data.frame(f = c("A", "B", "C", "D", "E", "A")), list(f = 
#'        c("AB = A + B", "AC = A + C", "CD = C + D", "ABCD = AB + CD")))
#'        
MapHierarchiesToData <- function(data, hierarchies, ...){
  a <- hierarchies_as_vars(hierarchies, ...)
  for(i in seq_along(a)){
    a[[i]] = MapHierarchyVars(a[[i]], data[[names(a[i])]])
  }
  names(a) <- NULL
  do.call(cbind, c(list(data), a))
}


MapHierarchyVars <- function(hierarchyAsVars, y){
  z <- hierarchyAsVars[match(y, hierarchyAsVars[[1]]), -1,drop = FALSE]
  rownames(z) <- NULL
  z
}


# AutoHierarchies
# Bør utvides slik at Vars2Hierarchies kjøres









if(FALSE){
  formler <- c(
    "P9300 = immatanl + varig + finam",
    "P9350 = varelager + sumford + sumomlinv + bank",
    "P9400 = P9300 + P9350",
    "P9450 = innskek + opptjek",
    "P9650 = P9450 + smgjd",
    "smgjd = avsforpl + sumlg + P9550",
    "P9100 = P9200 + skatkost",
    "P9050 = P9100 + resfin",
    "P9000 = P9050 + drkost"
  )
  
  
  print(hierarchies_as_vars(list(post = formler )))
  
}