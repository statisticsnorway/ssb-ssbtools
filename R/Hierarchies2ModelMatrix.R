#' HierarchyComputeDummy
#' 
#' From hierarchies to a sparse model matrix with possible cross table by wrapping HierarchyCompute
#' 
#' This function is a special wrapper of \code{\link{HierarchyCompute}} and the input argument hierarchies is specified the same way. 
#' That is, variables can also be coded by \code{"rowFactor"}  ( but not colFactor).
#'
#' @param data data 
#' @param hierarchies hierarchies
#' @param crossTable	Cross table in output when TRUE
#' @param ...  Further parameters sent to \code{\link{HierarchyCompute}} 
#'
#' @return A sparse model matrix or a list of model matrix and cross table
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
#' @examples
#' # Data and hierarchies used in the examples
#' x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
#' geoHier <- SSBtoolsData("sprt_emp_geoHier")
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#' HierarchyComputeDummy(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), 
#'                       inputInOutput = FALSE, crossTable = TRUE)
HierarchyComputeDummy <- function(data, hierarchies, inputInOutput = TRUE, crossTable = FALSE, ...) {
  a <- HierarchyCompute(data = data, hierarchies = hierarchies, inputInOutput = inputInOutput, output = "dataDummyHierarchyWithCodeFrame", asInput = TRUE, ...)
  if (!crossTable) 
    return(t(a$dataDummyHierarchy))
  list(modelMatrix = t(a$dataDummyHierarchy), crossTable = CharacterDataFrame(a$codeFrame))
}



#' Model matrix representing crossed hierarchies
#' 
#' Make a model matrix, x, that corresponds to data and represents all hierarchies crossed.
#' This means that aggregates corresponding to numerical variables can be computed as 
#' \code{x \%*\% y}, where  \code{y} is a matrix with one column for each numerical variable.
#' 
#' This function makes use of \code{\link{AutoHierarchies}}
#' and \code{\link{HierarchyCompute}}  via  \code{\link{HierarchyComputeDummy}}  
#'
#' @param data Matrix or data frame with data containing codes of relevant variables
#' @param hierarchies List of hierarchies, which can be converted by \code{\link{AutoHierarchies}}.
#' Thus, the variables can also be coded by \code{"rowFactor"} or \code{""}, which correspond to using the categories in the data.
#' @param inputInOutput Logical vector (possibly recycled) for each element of hierarchies.
#'         TRUE means that codes from input are included in output. Values corresponding to \code{"rowFactor"} or \code{""} are ignored.
#' @param crossTable Cross table in output when TRUE
#' @param total Vector of total codes (possibly recycled) used when running \code{\link{Hrc2DimList}} 
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}
#' @param unionComplement Logical vector (possibly recycled) for each element of hierarchies.
#'        When TRUE, sign means union and complement instead of addition or subtraction. 
#'        Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored. 
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
#' # First example has list output
#' Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), inputInOutput = FALSE, 
#'                         crossTable = TRUE)
#' 
#' m1 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), inputInOutput = FALSE)
#' m2 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList))
#' m3 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""),
#'                               inputInOutput = FALSE)
#' m4 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = "allYears"), 
#'                               inputInOutput = c(FALSE, FALSE, TRUE))
#' 
#' # Illustrate the effect of unionComplement, geoHier2 as in the examples of HierarchyCompute
#' geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
#'                   SSBtoolsData("sprt_emp_geoHier")[, -4])
#' m5 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoHier2, year = "allYears"), 
#'                               inputInOutput = FALSE)  # Spain is counted twice
#' m6 <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoHier2, year = "allYears"), 
#'                               inputInOutput = FALSE, unionComplement = TRUE)
#' 
#' # Compute aggregates
#' ths_per <- as.matrix(z[, "ths_per", drop = FALSE])  # matrix with the values to be aggregated
#' t(m1) %*% ths_per  # crossprod(m1, ths_per) is equivalent and faster
#' t(m2) %*% ths_per
#' t(m3) %*% ths_per
#' t(m4) %*% ths_per
#' t(m5) %*% ths_per
#' t(m6) %*% ths_per
Hierarchies2ModelMatrix <- function(data, hierarchies, inputInOutput = TRUE, crossTable = FALSE, total = "Total", 
                                    hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                                    unionComplement = FALSE) {
  autoHierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, total = total, hierarchyVarNames = hierarchyVarNames)
  HierarchyComputeDummy(data = data, hierarchies = autoHierarchies, inputInOutput = inputInOutput, crossTable = crossTable, unionComplement = unionComplement)
}







