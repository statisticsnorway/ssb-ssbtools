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
  if(crossTable)
    output <- "dataDummyHierarchyWithCodeFrame"
  else
    output = "dataDummyHierarchyQuick"
  
  a <- HierarchyCompute(data = data, hierarchies = hierarchies, inputInOutput = inputInOutput, output = output, asInput = TRUE, ...)
  
  if (!crossTable) 
    return(t(a))
  
  list(modelMatrix = t(a$dataDummyHierarchy), crossTable = CharacterDataFrame(a$codeFrame))
}



#' Model matrix representing crossed hierarchies
#' 
#' Make a model matrix, x, that corresponds to data and represents all hierarchies crossed.
#' This means that aggregates corresponding to numerical variables can be computed as 
#' \code{t(x) \%*\% y}, where  \code{y} is a matrix with one column for each numerical variable.
#' 
#' This function makes use of \code{\link{AutoHierarchies}}
#' and \code{\link{HierarchyCompute}}  via  \code{\link{HierarchyComputeDummy}}.
#' Since the dummy matrix is transposed in comparison to \code{HierarchyCompute}, the parameter \code{rowSelect} is renamed to \code{select}
#' and  \code{makeRownames} is renamed to \code{makeColnames}.  
#' 
#' The select parameter as a list can be partially specified in the sense that not all hierarchy names have to be included.
#' The parameter `inputInOutput` will only apply to hierarchies that are not in the `select` list (see note).
#' 
#' @note The `select` as a list is run via a special coding of the `inputInOutput` parameter. 
#' This parameter is converted into a list (`as.list`) and `select` elements are inserted into this list. 
#' This is also an additional option for users of the function.
#'
#' @param data Matrix or data frame with data containing codes of relevant variables
#' @param hierarchies List of hierarchies, which can be converted by \code{\link{AutoHierarchies}}.
#' Thus, the variables can also be coded by \code{"rowFactor"} or \code{""}, which correspond to using the categories in the data.
#' @param inputInOutput Logical vector (possibly recycled) for each element of hierarchies.
#'         TRUE means that codes from input are included in output. Values corresponding to \code{"rowFactor"} or \code{""} are ignored.
#'         Also see note.
#' @param crossTable Cross table in output when TRUE
#' @param total See \code{\link{AutoHierarchies}}
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}
#' @param unionComplement Logical vector (possibly recycled) for each element of hierarchies.
#'        When TRUE, sign means union and complement instead of addition or subtraction. 
#'        Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored. 
#' @param reOrder When TRUE (default) output codes are ordered in a way similar to a usual model matrix ordering. 
#' @param select Data frame specifying variable combinations for output 
#'               or a named list specifying code selections for each variable (see details).
#' @param removeEmpty When TRUE and when \code{select=NULL}, empty columns (only zeros) are not included in output.
#' @param selectionByMultiplicationLimit With non-NULL \code{select} and when the number of elements in the model matrix exceeds this limit,
#'          the computation is performed by a slower but more memory efficient algorithm. 
#' @param makeColnames Colnames included when TRUE (default).
#' @param verbose Whether to print information during calculations. FALSE is default.    
#' @param ... Extra unused parameters          
#'
#' @return A sparse model matrix or a list of two elements (model matrix and cross table)
#' @seealso \code{\link{ModelMatrix}}, \code{\link{HierarchiesAndFormula2ModelMatrix}}
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # Create some input
#' z <- SSBtoolsData("sprt_emp_withEU")
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#' geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' 
#' 
#' # First example has list output
#' Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), inputInOutput = FALSE, 
#'                         crossTable = TRUE)
#' 
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
#' 
#' # Compute aggregates
#' ths_per <- as.matrix(z[, "ths_per", drop = FALSE])  # matrix with the values to be aggregated
#' t(m1) %*% ths_per  # crossprod(m1, ths_per) is equivalent and faster
#' t(m2) %*% ths_per
#' t(m3) %*% ths_per
#' t(m4) %*% ths_per
#' t(m5) %*% ths_per
#' t(m6) %*% ths_per
#' 
#' 
#' # Example using the select parameter as a data frame
#' select <- data.frame(age = c("Y15-64", "Y15-29", "Y30-64"), geo = c("EU", "nonEU", "Spain"))
#' m2a <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), select = select)
#' 
#' # Same result by slower alternative
#' m2B <- Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), crossTable = TRUE)
#' m2b <- m2B$modelMatrix[, Match(select, m2B$crossTable), drop = FALSE]
#' t(m2b) %*% ths_per
#' 
#' # Examples using the select parameter as a list
#' Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), 
#'        inputInOutput = FALSE, 
#'        select = list(geo = c("nonEU", "Portugal")))
#' Hierarchies2ModelMatrix(z, list(age = ageHier, geo = geoDimList), 
#'        select = list(geo = c("nonEU", "Portugal"), age = c("Y15-64", "Y15-29")))
#' 
Hierarchies2ModelMatrix <- function(data, hierarchies, inputInOutput = TRUE, crossTable = FALSE, total = "Total", 
                                    hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                                    unionComplement = FALSE, reOrder = TRUE,
                                    select = NULL, removeEmpty = FALSE, 
                                    selectionByMultiplicationLimit = 10^7, 
                                    makeColnames = TRUE, verbose = FALSE, ...) {
  
  autoHierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, total = total, hierarchyVarNames = hierarchyVarNames)
  
  if (is.list(select) & !is.data.frame(select)) {
    inputInOutput <- rep_len(inputInOutput, length(hierarchies))  # similar to inside HierarchyCompute
    names(inputInOutput) <- names(hierarchies)
    inputInOutput <- as.list(inputInOutput)
    if (is.null(names(select))) {
      stop("select must be named")
    }
    if (any(!(names(select) %in% names(inputInOutput)))) {
      stop("Names of select must match hierarchy names")
    }
    inputInOutput[names(select)] <- select
    select <- NULL
  }
  
  if (is.null(select))
    if (removeEmpty)
      select <- "removeEmpty"
  
  HierarchyComputeDummy(data = data, hierarchies = autoHierarchies, inputInOutput = inputInOutput, crossTable = crossTable, 
                        unionComplement = unionComplement, reOrder=reOrder, rowSelect = select, 
                        selectionByMultiplicationLimit = selectionByMultiplicationLimit, 
                        makeRownames = makeColnames, verbose =  verbose)
}







