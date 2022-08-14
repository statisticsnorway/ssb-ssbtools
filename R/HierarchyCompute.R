

#' Change the hierarchy table to follow the standard
#'
#' Make sure that variable names and sign coding follow an internal standard. Level may be computed automatically
#'
#' @encoding UTF8
#'
#' @param hierarchy data frame with hierarchy table
#' @param hierarchyVarNames variable names
#' @param autoLevel When TRUE, level is computed by automatic method
#'
#' @return data frame with hierarchy table
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' # Make input data by changing variable names and sign coding.
#' h <- SSBtoolsData("FIFA2018ABCD")[, 1:3]
#' names(h)[1:2] <- c("from", "to")
#' minus <- h$sign < 0
#' h$sign <- "+"
#' h$sign[minus] <- "-"
#'
#' # Run HierarchyFix - Two levels created
#' HierarchyFix(h, c(mapsFrom = "from", mapsTo = "to", sign = "sign"))
#'
#' # Extend the hierarchy table
#' h2 <- rbind(data.frame(from = c("Oceania", "Asia", "Africa", "America", "Europe"),
#'                        to = "World", sign = "+"),
#'            data.frame(from = c("World", "Europe"),
#'                       to = "nonEurope", sign = c("+", "-")), h)
#'
#' # Run HierarchyFix - Three levels created
#' HierarchyFix(h2, c(mapsFrom = "from", mapsTo = "to", sign = "sign"))
#'
HierarchyFix <- function(hierarchy, hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), autoLevel = TRUE) {
  h <- FixHierarchy(hierarchy, hierarchyVarNames)
  if (autoLevel) 
    h <- AutoLevel(h)
  h
}





#' Hierarchical Computations
#'
#' This function computes aggregates by crossing several hierarchical specifications and factorial variables.
#'
#' A key element of this function is the matrix multiplication: 
#' \code{outputMatrix} \code{=} \code{dataDummyHierarchy} \code{\%*\%} \code{valueMatrix}.
#' The matrix, \code{valueMatrix} is a re-organized version of the valueVar vector from input. In particular,
#' if a variable is selected as \code{colFactor}, there is one column for each level of that variable.
#' The matrix, \code{dataDummyHierarchy} is constructed by crossing dummy coding of hierarchies (\code{\link{DummyHierarchy}}) and factorial variables
#' in a way that matches \code{valueMatrix}.  The code combinations corresponding to rows and columns of \code{dataDummyHierarchy}
#' can be obtained as \code{toCrossCode} and \code{fromCrossCode}.  In the default data frame output, the \code{outputMatrix} is stacked
#' to one column and combined with the code combinations of all variables.
#'
#' @encoding UTF8
#'
#' @param data The input data frame
#' @param hierarchies A named (names in \code{data}) list with hierarchies. Variables can also be coded by \code{"rowFactor"} and \code{"colFactor"}.
#' @param valueVar Name of the variable(s) to be aggregated.
#' @param colVar When non-NULL, the function \code{\link{HierarchyCompute2}} is called. See its documentation for more information.  
#' @param rowSelect Data frame specifying variable combinations for output. The colFactor variable is not included.
#'                  In addition \code{rowSelect="removeEmpty"} removes combinations corresponding to empty rows (only zeros) of \code{dataDummyHierarchy}.
#' @param colSelect Vector specifying categories of the colFactor variable for output.
#' @param select Data frame specifying variable combinations for output. The colFactor variable is included.
#' @param inputInOutput Logical vector (possibly recycled) for each element of hierarchies.
#'         TRUE means that codes from input are included in output. Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored.
#' @param output One of "data.frame" (default), "dummyHierarchies", "outputMatrix", "dataDummyHierarchy", "valueMatrix", "fromCrossCode",
#'        "toCrossCode", "crossCode" (as toCrossCode), "outputMatrixWithCrossCode", "matrixComponents", 
#'        "dataDummyHierarchyWithCodeFrame", "dataDummyHierarchyQuick". 
#'        The latter two do not require \code{valueVar} (\code{reduceData} set to \code{FALSE}).
#' @param autoLevel Logical vector (possibly recycled) for each element of hierarchies.
#'        When TRUE, level is computed by automatic method as in \code{\link{HierarchyFix}}.
#'        Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored.
#' @param unionComplement Logical vector (possibly recycled) for each element of hierarchies.
#'        When TRUE, sign means union and complement instead of addition or subtraction as in \code{\link{DummyHierarchy}}.
#'        Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored.
#' @param constantsInOutput A single row data frame to be combine by the other output.
#' @param hierarchyVarNames Variable names in the hierarchy tables as in \code{\link{HierarchyFix}}.
#' @param selectionByMultiplicationLimit With non-NULL \code{rowSelect} and when the number of elements in \code{dataDummyHierarchy} exceeds this limit,
#'          the computation is performed by a slower but more memory efficient algorithm.
#' @param colNotInDataWarning When TRUE, warning produced when elements of \code{colSelect} are not in data.
#' @param useMatrixToDataFrame When TRUE (default) special functionality for saving time and memory is used.
#' @param handleDuplicated Handling of duplicated code rows in data. One of: "sum" (default), "sumByAggregate", "sumWithWarning", "stop" (error), "single" or "singleWithWarning".
#'        With no colFactor sum and sumByAggregate/sumWithWarning are different (original values or aggregates in "valueMatrix"). 
#'        When single, only one of the values is used (by matrix subsetting). 
#' @param asInput When TRUE (FALSE is default) output matrices match input data. Thus
#'         \code{valueMatrix} \code{=} \code{Matrix(data[, valueVar],ncol=1)}. Only possible when no colFactor.
#' @param verbose Whether to print information during calculations. FALSE is default.
#' @param reOrder When TRUE (FALSE is default) output codes are ordered differently, more similar to a usual model matrix ordering. 
#' @param reduceData When TRUE (default) unnecessary (for the aggregated result) rows of \code{valueMatrix} are allowed to be removed.
#' @param makeRownames When TRUE \code{dataDummyHierarchy} contains rownames. By default, this is decided based on the parameter \code{output}. 
#'
#' @return As specified by the parameter \code{output}
#' @seealso \code{\link{Hierarchies2ModelMatrix}}, \code{\link{AutoHierarchies}}.
#' @importFrom methods hasArg
#' @export
#' @author Øyvind Langsrud
#'
#'
#' @examples
#' # Data and hierarchies used in the examples
#' x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
#' geoHier <- SSBtoolsData("sprt_emp_geoHier")
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#'
#' # Two hierarchies and year as rowFactor
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per")
#'
#' # Same result with year as colFactor (but columns ordered differently)
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per")
#'
#' # Internally the computations are different as seen when output='matrixComponents'
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", 
#'                  output = "matrixComponents")
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
#'                  output = "matrixComponents")
#'
#'
#' # Include input age groups by setting inputInOutput = TRUE for this variable
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
#'                  inputInOutput = c(TRUE, FALSE))
#'
#' # Only input age groups by switching to rowFactor
#' HierarchyCompute(x, list(age = "rowFactor", geo = geoHier, year = "colFactor"), "ths_per")
#'
#' # Select some years (colFactor) including a year not in input data (zeros produced)
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
#'                  colSelect = c("2014", "2016", "2018"))
#'
#' # Select combinations of geo and age including a code not in data or hierarchy (zeros produced)
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
#'                  rowSelect = data.frame(geo = "EU", age = c("Y0-100", "Y15-64", "Y15-29")))
#'                  
#' # Select combinations of geo, age and year 
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", 
#'      select = data.frame(geo = c("EU", "Spain"), age = c("Y15-64", "Y15-29"), year = 2015))
#'
#' # Extend the hierarchy table to illustrate the effect of unionComplement 
#' # Omit level since this is handled by autoLevel
#' geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
#'                   geoHier[, -4])
#'
#' # Spain is counted twice
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per")
#'
#' # Can be seen in the dataDummyHierarchy matrix
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", 
#'                  output = "matrixComponents")
#'
#' # With unionComplement=TRUE Spain is not counted twice
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", 
#'                  unionComplement = TRUE)
#'
#' # With constantsInOutput
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per",
#'                  constantsInOutput = data.frame(c1 = "AB", c2 = "CD"))
#'                  
#' # More that one valueVar
#' x$y <- 10*x$ths_per
#' HierarchyCompute(x, list(age = ageHier, geo = geoHier), c("y", "ths_per"))
HierarchyCompute <- function(data, hierarchies, valueVar, 
                             colVar = NULL,
                             rowSelect = NULL, colSelect = NULL, 
                             select = NULL,
                             inputInOutput = FALSE, output = "data.frame", 
                             autoLevel = TRUE, unionComplement = FALSE, constantsInOutput = NULL, 
                             hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                             selectionByMultiplicationLimit = 10^7, colNotInDataWarning = TRUE, useMatrixToDataFrame = TRUE,
                             handleDuplicated = "sum", asInput = FALSE, verbose = FALSE, reOrder = FALSE, 
                             reduceData = TRUE, makeRownames = NULL) {
  
  if(length(colVar)){
    sysCall <- sys.call()
    sysCall[[1]] <- as.name("HierarchyCompute2")
    parentFrame = parent.frame()
    return(eval(sysCall, envir=parentFrame))
  }
  
  # # To test whether tibble input works 
  # data <- tibble::as_tibble(data)  
  # for (i in seq_along(hierarchies)) if (is.list(hierarchies[[i]])) hierarchies[[i]] <- tibble::as_tibble(hierarchies[[i]])
  # if (!is.null(rowSelect)) if(!is.character(rowSelect)) rowSelect <- tibble::as_tibble(rowSelect)
  
  if(!(handleDuplicated %in% c("sum", "sumByAggregate", "sumWithWarning", "stop", "single", "singleWithWarning")))
    stop("invalid 'handleDuplicated' argument")
  
  
  if(is.null(makeRownames))
    makeRownames <- output %in% c("dataDummyHierarchyWithCodeFrame", "dataDummyHierarchyQuick", "dataDummyHierarchy", "matrixComponents")
  
  
  if(hasArg(valueVar)){
    nValueVar = length(valueVar)
  } else {
    nValueVar = 1L
  }
  
  if(output=="dataDummyHierarchyWithCodeFrame"  | output=="dataDummyHierarchyQuick"){  # used by HierarchyComputeDummy/Hierarchies2ModelMatrix
    reduceData <- FALSE                                                                # valueVar not used
  } 
  
  if(asInput){
    reduceData <- FALSE
    if(handleDuplicated !="sum")
      stop("'handleDuplicated' must be 'sum' when 'asInput'")
  }
  
  noRowGroupsWhenNoColVar <- !(handleDuplicated !="sum")
    
  removeEmpty <- FALSE
  if(!is.null(rowSelect ))
    if(is.character(rowSelect))
      if(length(rowSelect)==1)
        if(rowSelect=="removeEmpty"){
          removeEmpty <- TRUE
          rowSelect <- NULL
        }
    
  nHier <- length(hierarchies)
  
  inputInOutput <- rep_len(inputInOutput, nHier)
  autoLevel <- rep_len(autoLevel, nHier)
  unionComplement <- rep_len(unionComplement, nHier)
  
  
  gf <- GetFirstStringInList(hierarchies)
  
  if (any(!(gf %in% c("", "rowFactor", "colFactor")))) {
    stop(paste("Wrong input: ", paste(gf[!(gf %in% c("", "rowFactor", "colFactor"))], collapse =", ") ))
  }
  
  colInd <- which(gf == "colFactor")
  
  if (length(colInd) > 1) {
    stop("Only a single colVar allowed in this implementation")
  }
  
  colVar <- names(hierarchies)[colInd]
  
  if (length(colInd) == 0) {
    noColVar <- TRUE
    colSelect <- NULL
  } else {
    noColVar <- FALSE
    if (is.list(colSelect)) {
      colSelect <- colSelect[[colVar]]
      if (is.null(colSelect))
        stop("Wrong colSelect input")
    }
  }
  
  if (!noColVar & asInput) 
    stop("asInput only possible when no colFactor")
  
  
  if (noColVar) 
    hierarchyInd <- seq_len(nHier) else hierarchyInd <- seq_len(nHier)[-colInd]  # colVar excluded
  
  hierarchyNames <- names(hierarchies)[hierarchyInd]  # colVar excluded
  
  
  selectOrder <- NULL
  
  if (!is.null(select)) {
    if ((!is.null(rowSelect)) | (!is.null(colSelect))) 
      stop("With non-NULL select, rowSelect and colSelect must be NULL")
    if (noColVar) {
      rowSelect <- select
    } else {
      
      colSelect <- as.character(select[, colVar])
      colSelInt <- as.integer(factor(colSelect))
      colSelect <- unique(colSelect)
      
      rgSelect <- RowGroups(CharacterDataFrame(select[, hierarchyNames, drop=FALSE]), returnGroups = TRUE)
      
      selectOrder <- matrix(NaN, NROW(rgSelect$groups), length(colSelect))
      selectOrder[cbind(rgSelect$idx, colSelInt)] <- seq_len(NROW(select))
      selectOrder <- order(as.vector(selectOrder))[seq_len(NROW(select))]
      rowSelect <- rgSelect$groups
      rm(rgSelect)
    }
    inputColnamesRowSelect <- colnames(rowSelect)
  } else {
    if (!is.null(rowSelect)) {
      inputColnamesRowSelect <- colnames(rowSelect)
      rowSelect <- CharacterDataFrame(unique(rowSelect[, hierarchyNames, drop = FALSE]))   
    }
  }

  
  dummyHierarchies <- vector("list", nHier)
  dataDummyHierarchies <- vector("list", nHier)
  codeFrames <- vector("list", nHier)
  names(dummyHierarchies) <- names(hierarchies)
  names(dataDummyHierarchies) <- names(hierarchies)
  names(codeFrames) <- names(hierarchies)
  
  
  
  if (!is.null(rowSelect)) {
      inputInOutput[] <- FALSE  # Handeled by keep...
  }
  
  for (i in seq_len(nHier)) {
    if (is.list(hierarchies[[i]])) {
      
      
      hierarchies[[i]] <- FixHierarchy(hierarchies[[i]], hierarchyVarNames)
      if (autoLevel[i]) 
        hierarchies[[i]] <- AutoLevel(hierarchies[[i]])
      hierarchies[i] <- AddMapsInput(hierarchies[i], data)  
      if (!is.null(rowSelect)) {
        hierarchies[i] <- AddNonExistingCode(hierarchies[i], rowSelect, inputInOutput[i])
        mapsInput <- attr(hierarchies[[i]], "mapsInput")
        keepCodes <- attr(hierarchies[[i]], "keepCodes")
        hierarchies[[i]] <- AutoLevel(hierarchies[[i]])
        attr(hierarchies[[i]], "mapsInput") <- mapsInput
        attr(hierarchies[[i]], "keepCodes") <- keepCodes
      }
      
      dummyHierarchies[[i]] <- DummyHierarchy(mapsFrom = hierarchies[[i]]$mapsFrom, mapsTo = hierarchies[[i]]$mapsTo, mapsInput = attr(hierarchies[[i]], "mapsInput"),
                                              keepCodes = attr(hierarchies[[i]], "keepCodes"), sign = hierarchies[[i]]$sign, 
                                              level = hierarchies[[i]]$level, inputInOutput = inputInOutput[i], unionComplement = unionComplement[i],
                                              reOrder = reOrder)
    } else {
      if (hierarchies[[i]] == "rowFactor") {
        dummyHierarchies[[i]] <- fac2sparse(sort(factor(unique(data[, names(hierarchies)[i], drop = TRUE]))))
        colnames(dummyHierarchies[[i]]) <- rownames(dummyHierarchies[[i]])
      }
      
    }
  }
  
  
  if (output == "dummyHierarchies") 
    return(dummyHierarchies)
  
  if(verbose){
    cat(" [ HierarchyCompute initial calculations.")
    flush.console()}
  
  if (!is.null(rowSelect)) {
    for (i in hierarchyInd) {
      iRows <- rownames(dummyHierarchies[[i]]) %in% rowSelect[, names(hierarchies)[i], drop=TRUE]
      dummyHierarchies[[i]] <- dummyHierarchies[[i]][iRows, , drop = FALSE]
    }
  }
  
  if (!is.null(colSelect)) {
    datacolvar <- as.character(data[, colVar, drop = TRUE])
    colSelect <- unique(as.character(colSelect))
    colNotInData <- colSelect[!(colSelect %in% datacolvar)]
    if (length(colNotInData) > 0) {
      rowsData <- which(datacolvar %in% colSelect)
      
      if (length(rowsData) == 0) {
        if (colNotInDataWarning) 
          warning(paste("No items in colSelect in data[,'", colVar, "']. Only zeros produced: ", paste(colNotInData, collapse = ", "), sep = ""))
        
        rowsData <- c(rep(1, length(colNotInData)))
      } else {
        
        if (colNotInDataWarning) 
          warning(paste("Items in colSelect not in data[,'", colVar, "'] set to zero: ", paste(colNotInData, collapse = ", "), sep = ""))
        
        rowsData <- c(rep(rowsData[1], length(colNotInData)), rowsData)
      }
      datacolvar <- datacolvar[rowsData]
      datacolvar[seq_len(length(colNotInData))] <- colNotInData
      data <- data[rowsData, , drop = FALSE]
      data[, colVar] <- datacolvar
      data[seq_len(length(colNotInData)), valueVar] <- 0L
      
    } else {
      rowsData <- datacolvar %in% colSelect
      data <- data[rowsData, , drop = FALSE]
    }
  }

  rownames(data) <- NULL
  
  if (reduceData) 
    data <- ReduceDataByDummyHierarchiesAndValue(data, dummyHierarchies, valueVar, colVar)
  
  if(verbose){
    cat(".")
    flush.console()}
  
  if(noColVar & noRowGroupsWhenNoColVar){
    rowGroups <- list(idx = seq_len(NROW(data)), groups = data[, hierarchyNames, drop = FALSE])
  } else {
    rowGroups <- RowGroups(data[, hierarchyNames, drop = FALSE], returnGroups = TRUE)
  }  
  
  if(verbose){
    cat(".")
    flush.console()}
 
  for (i in hierarchyInd) {
    dataDummyHierarchies[[i]] <- DataDummyHierarchy(as.character(rowGroups$groups[names(hierarchies)[i]][[1]]), dummyHierarchies[[i]])
    codeFrames[[i]] <- data.frame(a = factor(rownames(dummyHierarchies[[i]])))
    names(codeFrames[[i]]) <- names(codeFrames[i])
  }
  
  if(verbose){
    cat("]")
    flush.console()}
  
  
  
  runCrossDataDummyHierarchies <- is.null(rowSelect)
  
  if (!is.null(rowSelect)) {
    selectionByMultiplication <- (as.numeric(dim(rowGroups$groups)[1]) * as.numeric(NROW(rowSelect))) < selectionByMultiplicationLimit
  }
  
  if (runCrossDataDummyHierarchies) {
    if(output=="dataDummyHierarchyQuick") 
      codeFrames <- NULL
    k <- CrossDataDummyHierarchies(dataDummyHierarchies = dataDummyHierarchies[hierarchyInd], codeFrames = codeFrames[hierarchyInd], makeDimnames = makeRownames, 
                                   useMatrixToDataFrame = useMatrixToDataFrame, removeEmpty = removeEmpty, verbose = verbose, reOrder = reOrder)
    if(output=="dataDummyHierarchyQuick" | output=="dataDummyHierarchyWithCodeFrame") {
      if(output=="dataDummyHierarchyWithCodeFrame"){
        k$codeFrame <- CharacterDataFrame(k$codeFrame)
      }
      return(k)
    }
  } else {
    k <- list(dataDummyHierarchy = NULL, codeFrame = rowSelect)
    if (!selectionByMultiplication) {
        k <- ReductionCrossDataDummyHierarchies(dataDummyHierarchies[hierarchyInd], codeFrames = codeFrames[hierarchyInd], codeFrame = k[[2]], 
                                                makeDimnames = makeRownames, useMatrixToDataFrame = useMatrixToDataFrame, verbose = verbose)
    } else {
      k[[1]] <- SelectionCrossDataDummyHierarchy(dataDummyHierarchies[hierarchyInd], k$codeFrame, verbose = verbose)
    }
  }
  
  if(makeRownames){
    if( is.null(rownames(k$dataDummyHierarchy) )){
      rownames(k$dataDummyHierarchy) <- apply(k$codeFrame, 1, paste, collapse = ":")
    }
  }
    
  if(output=="dataDummyHierarchyQuick")
    return(k$dataDummyHierarchy)
  
  if(output=="dataDummyHierarchyWithCodeFrame"){
    k$codeFrame <- CharacterDataFrame(k$codeFrame)
    return(k)
  }
  
  
  readyValueMatrix <- FALSE
  if (noColVar) {
    valueMatrix <- Matrix(0, dim(rowGroups$groups)[1], nValueVar)
    colnames(valueMatrix) <- colnames(data[1, valueVar, drop = FALSE])
    if(handleDuplicated !="single")
      if( NROW(valueMatrix) != NROW(data)){
        if(handleDuplicated =="singleWithWarning"){
          warning("Duplicated rows in input. By matrix subsetting one of the is used.")
        } else {
          if(handleDuplicated =="stop") 
            stop("Duplicated rows in input ('handleDuplicated=stop').")
          if(handleDuplicated %in% c("sum", "sumByAggregate","sumWithWarning")){ 
            if(handleDuplicated == "sumWithWarning") 
              warning("Duplicated rows in input summed ('handleDuplicated=sumWithWarning').")
            if(verbose){
              cat(" [ aggregate..")
              flush.console()}
            aggData <- aggregate(data[, valueVar, drop = FALSE], by = list(idx69_3h4_6kd = rowGroups$idx), FUN = sum)
            if(verbose){
              cat(".")
              flush.console()}
            if(nValueVar>1)
              valueMatrix[aggData$idx69_3h4_6kd, valueVar] <- as.matrix(aggData[, valueVar])
            else
              valueMatrix[aggData$idx69_3h4_6kd, valueVar] <- aggData[, valueVar]
            rm(aggData)
            if(verbose){
              cat("]")
              flush.console()}
            readyValueMatrix <- TRUE
          }
        }
      }
    if(!readyValueMatrix){
      if (nValueVar>1){
        valueMatrix[rowGroups$idx,  ] <- as.matrix(data[, valueVar])
      } else {
        valueMatrix[rowGroups$idx, 1] <- data[, valueVar, drop = TRUE]
      }
    }
  } else {
    
    colData <- factor(data[, colVar, drop = TRUE])
    integerColData <- as.integer(colData)
    nCol <- max(integerColData, 0L)
    
    valueMatrix <- Matrix(0, dim(rowGroups$groups)[1], nCol*nValueVar)
    
    colnames(valueMatrix) <- rep(levels(colData), nValueVar) 
    
    idx_integerColData <- cbind(idx = rowGroups$idx, integerColData = integerColData)
    
  
    if(handleDuplicated !="single")
      if(anyDuplicated(idx_integerColData)){
        if(handleDuplicated =="singleWithWarning"){
          warning("Duplicated rows in input. By matrix subsetting one of the is used.")
        } else {
          if(handleDuplicated =="stop") 
            stop("Duplicated rows in input ('handleDuplicated=stop').")
          if(handleDuplicated %in% c("sum", "sumByAggregate","sumWithWarning")){ 
            if(handleDuplicated == "sumWithWarning") 
              warning("Duplicated rows in input summed ('handleDuplicated=sumWithWarning').")
            if(verbose){
              cat(" [ aggregate..")
              flush.console()}
            aggData <- aggregate(data[, valueVar, drop = FALSE], 
                                 by = list(idx69_3h4_6kd = rowGroups$idx, integerColData7y9_56 = integerColData), FUN = sum)
            if(verbose){
              cat(".")
              flush.console()}
            mIntegerColData <- rep(aggData$integerColData7y9_56, nValueVar) + rep(nCol * SeqInc(0, nValueVar - 1), each = nrow(aggData))
            if(nValueVar>1)
              valueMatrix[cbind(aggData$idx69_3h4_6kd, mIntegerColData)] <- as.vector(as.matrix(aggData[, valueVar]))
            else
              valueMatrix[cbind(aggData$idx69_3h4_6kd, mIntegerColData)] <- aggData[, valueVar]   # test-HierarchyCompute problem seen here ... i=4 temporarily omitted due to  development version of Matrix 1.4-2 
            rm(aggData)
            if(verbose){
              cat("]")
              flush.console()}
            readyValueMatrix <- TRUE
          }
        }
      }
    if(!readyValueMatrix){
      if (nValueVar>1){
        mIntegerColData = rep(integerColData, nValueVar) + rep(nCol*SeqInc(0,nValueVar-1),each = length(integerColData))
        valueMatrix[cbind(idx = rowGroups$idx, mIntegerColData = mIntegerColData)] <-  as.vector(as.matrix(data[, valueVar]))
      } else {
        valueMatrix[idx_integerColData] <- data[, valueVar, drop = TRUE] # test-HierarchyCompute problem seen here ... j=1 temporarily omitted due to  development version of Matrix 1.4-2
      }
    }
    
  }
  
  rownames(valueMatrix) <- NULL # rownames when square since first matrix (zeros) is  symmetric  
  
  
  if ((!is.null(rowSelect)) & runCrossDataDummyHierarchies) {
    #rg <- RowGroups(rbind(CharacterDataFrame(k$codeFrame), CharacterDataFrame(rowSelect[, hierarchyNames, drop = FALSE])))
    rg <- RowGroups(rbind(CharacterDataFrame(k$codeFrame), rowSelect))
    rg1 <- rg[seq_len(dim(k$codeFrame)[1])]
    rg2 <- rg[-seq_len(dim(k$codeFrame)[1])]
    selectedRows <- match(rg2, rg1)
  } else {
    selectedRows <- NULL
  }
  
  if (is.null(selectedRows)) {
    outputMatrix <- Mult(k[[1]], valueMatrix) #k[[1]] %*% valueMatrix
    xCrossCode <- k$codeFrame
  } else {
      outputMatrix <- Mult(k[[1]][selectedRows, , drop = FALSE], valueMatrix) #k[[1]][selectedRows, , drop = FALSE] %*% valueMatrix
    xCrossCode <- k$codeFrame[selectedRows, , drop = FALSE]
  }
  
  xCrossCode <- CharacterDataFrame(xCrossCode)
  rownames(xCrossCode) <- NULL
  
  hierarchyNamesForOutput <- NULL
  
  if ((!is.null(rowSelect)) & is.null(hierarchyNamesForOutput)) {
    hierarchyNamesForOutput <- inputColnamesRowSelect
  }
  
  
  if (!is.null(hierarchyNamesForOutput)) {
    macol <- match(hierarchyNamesForOutput, colnames(xCrossCode))
    macol <- macol[!is.na(macol)]
    xCrossCode <- xCrossCode[, macol, drop = FALSE]
  }
  
  if (output == "outputMatrix") {
    if(verbose){
      cat("\n")
      flush.console()}
    return(outputMatrix)
  }
  
  if (output == "dataDummyHierarchy") {
    if(verbose){
      cat("\n")
      flush.console()}
    if (is.null(selectedRows)) {
      return(k[[1]])
    } else {
      return(k[[1]][selectedRows, , drop = FALSE])
    }
  }
  
  
  if (output == "valueMatrix") {
    if(verbose){
      cat("\n")
      flush.console()}
    return(valueMatrix)
  }
  
  if (output == "fromCrossCode") {
    if(verbose){
      cat("\n")
      flush.console()}
    return(rowGroups$groups)
  }
  
  
  if (output == "crossCode" | output == "toCrossCode") {
    # Denne kan flyttes opp ..
    if(verbose){
      cat("\n")
      flush.console()}
    return(xCrossCode)
  }
  
  
  if (output == "outputMatrixWithCrossCode") {
    if(verbose){
      cat("\n")
      flush.console()}
    return(list(outputMatrix = outputMatrix, xCrossCode = xCrossCode))
  }
  
  
  if (output == "matrixComponents") {
    if(verbose){
      cat("\n")
      flush.console()}
    if (is.null(selectedRows)) {
      return(list(dataDummyHierarchy = k[[1]], valueMatrix = valueMatrix, fromCrossCode = rowGroups$groups, toCrossCode = xCrossCode))
    } else {
      return(list(dataDummyHierarchy = k[[1]][selectedRows, , drop = FALSE], valueMatrix = valueMatrix, fromCrossCode = rowGroups$groups, toCrossCode = xCrossCode))
    }
  }
  
  
  if(verbose){
    cat(" [ output='data.frame'...")
    flush.console()}
  
  x <- as.matrix(outputMatrix)
  if(ncol(x) == 0){
    colnamesX <- character(0)
  }
  else{
    colnamesX <- colnames(x)
  }
  dimX1 <- dim(x)[1]
  
  if(nValueVar>1){
    if(!noColVar){
      colnamesX <- colnamesX[seq_len(nCol)]
      x <- matrix(x, ncol = nValueVar)
      colnames(x) <- valueVar
      z <- as.data.frame(x)
    } else {
      z <- as.data.frame(x)
    }
  } else {
    z <- data.frame(a = as.vector(x), stringsAsFactors = FALSE)
    names(z) <- valueVar
  }
  
  if (noColVar) {
    colDataSelected <- xCrossCode[, integer(0)]
  } else {
    colDataSelected <- data.frame(a = rep(colnamesX, times = 1, each = dimX1), stringsAsFactors = FALSE)
    names(colDataSelected) <- colVar
  }
  
  if (nrow(colDataSelected) == 0) {
    xCrossCode <- xCrossCode[integer(0), , drop = FALSE]
    if (!is.null(constantsInOutput)) 
      constantsInOutput <- constantsInOutput[integer(0), , drop = FALSE]
  }
  
  if (!is.null(constantsInOutput)) 
    w <- cbind(constantsInOutput, colDataSelected, xCrossCode, z) else w <- cbind(colDataSelected, xCrossCode, z)
  
  
  if (!is.null(selectOrder)) {
    w <- w[selectOrder, , drop = FALSE]
  }
  
  rownames(w) <- NULL
  
  
  if(verbose){
    cat("]\n")
    flush.console()}
  
  # return(list(w=w,outputMatrix=outputMatrix,valueMatrix=valueMatrix,k=k,codeFrames=codeFrames,dataDummyHierarchies=dataDummyHierarchies,dummyHierarchies=dummyHierarchies,rowGroups=rowGroups,hierarchyInd=hierarchyInd))
  w
}




#' AddMapsInput
#'
#' Brukes til å generere feil
#'
#' @param hierarchies hierarchies
#' @param data data
#'
#' @keywords internal
#'
AddMapsInput <- function(hierarchies, data = NULL) {
  for (i in length(hierarchies)) {
    if (is.list(hierarchies[[i]])) {
      mapsInput <- as.character(hierarchies[[i]]$mapsFrom)[!(as.character(hierarchies[[i]]$mapsFrom) %in% as.character(hierarchies[[i]]$mapsTo))]
      if (!is.null(data)) 
        mapsInput <- c(mapsInput, as.character(unique(data[, names(hierarchies)[i], drop = TRUE])))
      mapsInput <- as.character(sort(as.factor(unique(mapsInput))))
      
      if (any(mapsInput %in% as.character(hierarchies[[i]]$mapsTo))) {
        stop(paste(names(hierarchies)[i], "codes in mapsTo already in input data:", paste(mapsInput[mapsInput %in% as.character(hierarchies[[i]]$mapsTo)], collapse = ", ")))
        
      }
      
      attr(hierarchies[[i]], "mapsInput") <- mapsInput
    }
  }
  hierarchies
}



#' AddNonExistingCode
#'
#' @param hierarchies hierarchies
#' @param data data
#' @param rowSelect rowSelect
#'
#' @keywords internal
#'
AddNonExistingCode <- function(hierarchies, rowSelect = NULL, inputInOutput = TRUE) {
  if (is.null(rowSelect)) 
    return(hierarchies)
  for (i in length(hierarchies)) {
    if (is.list(hierarchies[[i]])) {
      mapsInput <- attr(hierarchies[[i]], "mapsInput")
      allCodes <- unique(c(as.character(mapsInput), hierarchies[[i]]$mapsTo))
      
      uniqueRowSelect <- unique(rowSelect[, names(hierarchies)[i], drop = TRUE])
      newCodes <- uniqueRowSelect[!(uniqueRowSelect %in% allCodes)]
      if (length(newCodes) > 0) {
        hierarchyExtra <- hierarchies[[i]][rep(1, (length(newCodes))), , drop = FALSE]
        hierarchyExtra$mapsTo <- newCodes
        hierarchyExtra$mapsFrom <- "N_oNEX_istIn_gCOd_e"
        hierarchyExtra$level <- 1L
        hierarchies[[i]] <- rbind(hierarchies[[i]], hierarchyExtra)
        mapsInput <- c(mapsInput, "N_oNEX_istIn_gCOd_e")
      }
      if (!inputInOutput) {
        keepCodes <- uniqueRowSelect[uniqueRowSelect %in% as.character(mapsInput)]
      } else keepCodes <- uniqueRowSelect[integer(0)]
      attr(hierarchies[[i]], "mapsInput") <- mapsInput
      attr(hierarchies[[i]], "keepCodes") <- keepCodes
    }
  }
  hierarchies
}




#' CrossDataDummyHierarchies
#'
#' @param dataDummyHierarchies dataDummyHierarchies
#' @param codeFrames codeFrames
#' @param makeDimnames makeDimnames
#' @param removeEmpty removeEmpty
#' @param verbose Whether to print information during calculations. FALSE is default.
#' @keywords internal
#'
CrossDataDummyHierarchies <- function(dataDummyHierarchies, codeFrames = NULL, makeDimnames = FALSE, useMatrixToDataFrame = TRUE, 
                                      removeEmpty = FALSE, verbose = FALSE, reOrder = FALSE) {
  
  
  if(reOrder)
    CrossDataDummyHierarchyHere = CrossDataDummyHierarchyReOrder
  else
    CrossDataDummyHierarchyHere =CrossDataDummyHierarchy
  
  if(verbose){
    if(removeEmpty)
      cat(" [ KhatriRaoRemoveEmpty...")
    else
      cat(" [ KhatriRao...")
    flush.console()
  }
  
  if (is.null(codeFrames)) 
    useCodeFrames <- FALSE else useCodeFrames <- !any(sapply(codeFrames, is.null))
  
  n <- length(dataDummyHierarchies)

  if(removeEmpty){
    for(i in seq_len(n)){
      rowNonZero <- RowNonZero(dataDummyHierarchies[[i]])
      dataDummyHierarchies[[i]] <- dataDummyHierarchies[[i]][rowNonZero, ,drop=FALSE]
      if (useCodeFrames) 
        codeFrames[[i]] <- codeFrames[[i]][rowNonZero, ,drop=FALSE]
    }
  }
      
  if (useCodeFrames) {
    for (i in seq_len(n)){ 
      if(i==1)
        z <- CrossDataDummyHierarchyHere(dataDummyHierarchy1 = dataDummyHierarchies[[1]], codeFrame1 = codeFrames[[1]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      else
        z <- CrossDataDummyHierarchyHere(z[[1]], dataDummyHierarchies[[i]], z[[2]], codeFrames[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      if(removeEmpty){
        rowNonZero <- RowNonZero(z[[1]])
        z[[1]] <- z[[1]][rowNonZero, , drop = FALSE]
        z[[2]] <- z[[2]][rowNonZero, , drop = FALSE]
      }
    }
  } else {
    for (i in seq_len(n)){ 
      if(i==1)
        z <- CrossDataDummyHierarchyHere(dataDummyHierarchy1 = dataDummyHierarchies[[1]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      else
        z <- CrossDataDummyHierarchyHere(z, dataDummyHierarchies[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      if(removeEmpty){
        rowNonZero <- RowNonZero(z)
        z <- z[rowNonZero, , drop = FALSE]
      }
    }
  }
  if(verbose){
    cat("]")
    flush.console()}
  z
} 
  


RowNonZero <- function(x){
  rowSums(x!=0) > 0
}







ReductionCrossDataDummyHierarchies <- function(dataDummyHierarchies, codeFrames = NULL, makeDimnames = FALSE, codeFrame = NULL, 
                                               useMatrixToDataFrame = TRUE, verbose = FALSE) {
  if(verbose){
    cat(" [ ReductionKhatriRao...")
    flush.console()}
  if (is.null(codeFrames)) 
    useCodeFrames <- FALSE else useCodeFrames <- !any(sapply(codeFrames, is.null))
  
  n <- length(dataDummyHierarchies)
  
  if (useCodeFrames) {
    
    
    varNames <- names(dataDummyHierarchies)
    
    
    z <- CrossDataDummyHierarchy(dataDummyHierarchy1 = dataDummyHierarchies[[1]], codeFrame1 = codeFrames[[1]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
    
    selecti <- z[[2]][, 1, drop = TRUE] %in% codeFrame[, 1, drop = TRUE]
    z[[1]] <- z[[1]][selecti, , drop = FALSE]
    z[[2]] <- z[[2]][selecti, , drop = FALSE]
    
    
    for (i in matlabColon(2, n)) {
      
      z <- CrossDataDummyHierarchy(z[[1]], dataDummyHierarchies[[i]], z[[2]], codeFrames[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      
      
        if(i == n) {
          selecti <- Match(codeFrame[, seq_len(i)], z[[2]])
        }  else {
          selecti <- Match(unique(codeFrame[, seq_len(i)]), z[[2]])
        }
        if (anyNA(selecti)) {
          selecti <- selecti[!is.na(selecti)]
          warning("Not all rowSelect possible. Row removed")  # Sette inn NA isteden??
        }
       
      z[[1]] <- z[[1]][selecti, , drop = FALSE]
      z[[2]] <- z[[2]][selecti, , drop = FALSE]
      
    }
  } else {
    z <- CrossDataDummyHierarchy(dataDummyHierarchy1 = dataDummyHierarchies[[1]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
    for (i in matlabColon(2, n)) z <- CrossDataDummyHierarchy(z, dataDummyHierarchies[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
  }
  if(verbose){
    cat("]")
    flush.console()}
  z
}








SelectionDataDummyHierarchy <- function(dataDummyHierarchy, codeVector) {
  x <- factor(codeVector, levels = rownames(dataDummyHierarchy))
  
  xInteger <- as.integer(x)
  naxInteger <- is.na(xInteger)
  
  if (any(naxInteger)) {
    dataDummyHierarchy <- rbind(dataDummyHierarchy, 0)
    xInteger[naxInteger] <- NROW(dataDummyHierarchy)
  }
  
  m <- dataDummyHierarchy[xInteger, , drop = FALSE]
  rownames(m) <- names(codeVector)
  m
}


SelectionCrossDataDummyHierarchy <- function(dataDummyHierarchies, codeFrame, verbose = FALSE) {
  if(verbose){
    cat(" [ SelectionByMultiplication...")
    flush.console()}
  n <- length(dataDummyHierarchies)
  if (n == 0) 
    return(dataDummyHierarchies)
  z <- SelectionDataDummyHierarchy(dataDummyHierarchies[[1]], codeFrame[, names(dataDummyHierarchies)[1],drop = TRUE])
  for (i in matlabColon(2, n)) {
    z <- z * SelectionDataDummyHierarchy(dataDummyHierarchies[[i]], codeFrame[, names(dataDummyHierarchies)[i],drop = TRUE])
  }
  if(verbose){
    cat("]")
    flush.console()}
  z
}








FixHierarchy <- function(hi, hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign", level = "level")) {
  ma <- match(names(hi), hierarchyVarNames)
  wma <- which(!is.na(ma))
  ma <- ma[wma]
  hi <- hi[, wma, drop = FALSE]
  names(hierarchyVarNames[ma])
  colnames(hi) <- names(hierarchyVarNames[ma])
  sig <- suppressWarnings(as.integer(hi$sign))
  if (anyNA(sig)) 
    hi$sign <- 2L * as.integer(hi$sign == "+") - 1L else hi$sign <- sig
  hi
}



AutoLevel <- function(x) {
  mapsFrom <- as.character(x$mapsFrom)
  mapsTo <- as.character(x$mapsTo)
  sign <- x$sign
  if (any(mapsFrom == mapsTo)) {
    sel <- !(mapsFrom == mapsTo)
    mapsFrom <- mapsFrom[sel]
    mapsTo <- mapsTo[sel]
    sign <- sign[sel]
    warning("hierarchy rows where mapsFrom==mapsTo removed")
  }
  mapsInput <- mapsFrom[!(mapsFrom %in% mapsTo)]
  
  level <- rep(0L, length(mapsFrom))
  
  mapsNow <- as.character(mapsInput)
  
  i <- 0
  sumLevel0 <- sum(level == 0)
  while (sumLevel0 > 0) {
    i <- i + 1
    # print(i)
    level[(mapsFrom %in% mapsNow) & level == 0] <- i
    
    uniquei <- unique(mapsTo[level == i])
    
    for (l in uniquei) {
      mTl <- mapsTo == l
      
      if (any(!(unique(mapsFrom[mTl]) %in% mapsNow))) 
        level[mTl] <- 0
    }
    
    
    mapsNow <- unique(c(mapsNow, unique(mapsTo[level == i])))
    sumLevel0old <- sumLevel0
    sumLevel0 <- sum(level == 0)
    if (sumLevel0 == sumLevel0old) {
      sumLevel0 <- 0
      li <- paste(paste(mapsFrom[level == 0], mapsTo[level == 0], sep = "->"), collapse = ", ")
      warning(paste("AutoLevel had problems:", li))
    }
  }
  
  
  data.frame(mapsFrom = mapsFrom, mapsTo = mapsTo, sign = sign, level = level, stringsAsFactors = FALSE)
}




#' Converting hierarchy specifications to a (signed) dummy matrix
#'
#' A matrix for mapping input codes (columns) to output codes (rows) are created.
#' The elements of the matrix specify how columns contribute to rows.
#'
#'
#' @param mapsFrom Character vector from hierarchy table
#' @param mapsTo Character vector from hierarchy table
#' @param sign  Numeric vector of either 1 or -1 from hierarchy table
#' @param level Numeric vector from hierarchy table
#' @param mapsInput All codes in mapsFrom not in mapsTo (created automatically when NULL) and possibly other codes in input data.
#' @param inputInOutput When FALSE all output rows represent codes in mapsTo
#' @param keepCodes To prevent some codes to be removed when inputInOutput = FALSE
#' @param unionComplement When TRUE, sign means union and complement instead of addition or subtraction (see note)
#' @param reOrder When TRUE (FALSE is default) output codes are ordered differently, more similar to a usual model matrix ordering.
#'
#' @return
#' A sparse matrix with row and column and names
#' @export
#' @author Øyvind Langsrud
#' @import Matrix
#'
#' @note
#' With unionComplement = FALSE (default), the sign of each mapping specifies the contribution as addition or subtraction.
#' Thus, values above one and negative values in output can occur.
#' With unionComplement = TRUE,  positive is treated as union and negative as complement. Then 0 and 1 are the only possible elements in the output matrix.
#'
#' @examples
#' # A hierarchy table
#' h <- SSBtoolsData("FIFA2018ABCD")
#'
#' DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level)
#' DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level, inputInOutput = TRUE)
#' DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level, keepCodes = c("Portugal", "Spain"))
#'
#' # Extend the hierarchy table to illustrate the effect of unionComplement
#' h2 <- rbind(data.frame(mapsFrom = c("EU", "Schengen"), mapsTo = "EUandSchengen", 
#'                        sign = 1, level = 3), h)
#'
#' DummyHierarchy(h2$mapsFrom, h2$mapsTo, h2$sign, h2$level)
#' DummyHierarchy(h2$mapsFrom, h2$mapsTo, h2$sign, h2$level, unionComplement = TRUE)
#'
#' # Extend mapsInput - leading to zero columns.
#' DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level,
#'                mapsInput = c(h$mapsFrom[!(h$mapsFrom %in% h$mapsTo)], "Norway", "Finland"))
#'
#' # DummyHierarchies
#' DummyHierarchies(FindHierarchies(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")]), 
#'                  inputInOutput = c(FALSE, TRUE))
DummyHierarchy <- function(mapsFrom, mapsTo, sign, level, mapsInput = NULL, inputInOutput = FALSE, keepCodes = mapsFrom[integer(0)], unionComplement = FALSE, reOrder = FALSE) {
  
  mapsFrom <- as.character(mapsFrom)  # Ensure character (if factor)
  mapsTo <- as.character(mapsTo)  # Ensure character (if factor)
  
  if (is.null(mapsInput)) 
    mapsInput <- mapsFrom[!(mapsFrom %in% mapsTo)]
  
  mapsInput <- sort(as.factor(unique(mapsInput)))
  
  m <- Matrix::t(fac2sparse(mapsInput))
  rownames(m) <- as.character(mapsInput)  #dimnames(m)[[2]]  = as.character(mapsInput)
  
  dropInput <- rownames(m)
  if (length(keepCodes) > 0) 
    dropInput <- dropInput[!(dropInput %in% keepCodes)]
  
  nInput <- dim(m)[1]
  
  for (i in unique(sort(level))) {
    ri <- (level == i)
    mapsToi <- factor(mapsTo[ri])
    mapsFromi <- factor(mapsFrom[ri], levels = rownames(m))
    
    if (anyNA(mapsFromi)) {
      warning("Problematic hierarchy specification")
    }
    mNew <- Matrix(0, NROW(m), length(levels(mapsToi)), dimnames = list(levels(mapsFromi), levels(mapsToi)))
    mNew[cbind(as.integer(mapsFromi), as.integer(mapsToi))] <- sign[ri]
    if(reOrder){
      if (unionComplement) 
        m <- rbind(CrossprodUnionComplement(mNew, m),m)  #  Better ordering 
      else m <- rbind(Mult_crossprod(mNew, m),m) #rbind(crossprod(mNew, m),m)  
    } else {
      if (unionComplement) 
        m <- rbind(m, CrossprodUnionComplement(mNew, m))  # Matrix::rBind(m,  CrossprodUnionComplement(mNew,m))
      else m <- rbind(m, Mult_crossprod(mNew, m)) #rbind(m, crossprod(mNew, m))  # Matrix::rBind(m,  crossprod(mNew,m))
    }
  }
  if (!inputInOutput & length(dropInput) > 0) {
    keepRows <- rownames(m)[!(rownames(m) %in% dropInput)]
    m <- m[keepRows, , drop = FALSE]
  }
  m  # Lage warnig/error om annet i matrisa enn 0, -1, 1 ?
}

#' @rdname DummyHierarchy
#' @details `DummyHierarchies` is a user-friendly wrapper for the original function `DummyHierarchy`.
#'           Then, the logical input parameters are vectors (possibly recycled).
#'           `mapsInput` and `keepCodes` can be supplied as attributes.
#'           `mapsInput` will be generated when `data` is non-NULL.   
#'            
#' 
#' @param hierarchies  List of hierarchies
#' @param data data
#' @export
DummyHierarchies <- function(hierarchies, data = NULL, inputInOutput = FALSE, unionComplement = FALSE, reOrder = FALSE) {
  
  n <- length(hierarchies)
  inputInOutput <- rep_len(inputInOutput, n)
  unionComplement <- rep_len(unionComplement, n)
  reOrder <- rep_len(reOrder, n)
  
  
  for (i in seq_len(n)) {
    if (!is.null(data)) {
      hierarchies[i] <- AddMapsInput(hierarchies[i], data)
    }
    
    hierarchies[[i]] <- DummyHierarchy(mapsFrom = hierarchies[[i]]$mapsFrom, 
                                       mapsTo = hierarchies[[i]]$mapsTo, 
                                       mapsInput = attr(hierarchies[[i]], "mapsInput"),
                                       keepCodes = attr(hierarchies[[i]], "keepCodes"), 
                                       sign = hierarchies[[i]]$sign, 
                                       level = hierarchies[[i]]$level, 
                                       inputInOutput = inputInOutput[i],
                                       unionComplement = unionComplement[i], 
                                       reOrder = reOrder[i])
  }
  hierarchies
}



#' Create a (signed) dummy matrix for hierarcical mapping of codes in data
#'
#' @param dataVector A vector of codes in data
#' @param dummyHierarchy Output from \code{\link{DummyHierarchy}}
#'
#' @return  A sparse matrix.
#' Column names are taken from dataVector (if non-NULL) and row names are taken from
#' the row names of dummyHierarchy.
#' @export
#' @author Øyvind Langsrud
#'
DataDummyHierarchy <- function(dataVector, dummyHierarchy) {
  x <- factor(dataVector, levels = colnames(dummyHierarchy))
  m <- dummyHierarchy[, as.integer(x), drop = FALSE]
  colnames(m) <- names(dataVector)
  m
}



#' @rdname DataDummyHierarchy
#' @details `DataDummyHierarchies` is a user-friendly wrapper for the original function `DataDummyHierarchy`.
#'          When `colNamesFromData` is `FALSE` (default), this function returns
#'          `mapply(DataDummyHierarchy,` `data[names(dummyHierarchies)],` `dummyHierarchies)`. 
#'            
#' @param data data
#' @param dummyHierarchies  Output from \code{\link{DummyHierarchies}}
#' @param colNamesFromData  Column names from data when `TRUE` 
#' @export
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")[1:9, ]
#' hi <- FindHierarchies(z[, c("geo", "eu", "age", "year")])
#' dhi <- DummyHierarchies(hi, inputInOutput = TRUE)
#' DataDummyHierarchies(z, dhi, colNamesFromData = TRUE)
DataDummyHierarchies <- function(data, dummyHierarchies,  colNamesFromData = FALSE) {
  
  if(!colNamesFromData)
    return(mapply(DataDummyHierarchy, data[names(dummyHierarchies)], dummyHierarchies))
  
  mapply(function(a, b){names(a) <- a; DataDummyHierarchy(a,b)}, data[names(dummyHierarchies)], dummyHierarchies)
  
}



#' CrossDataDummyHierarchy
#'
#' @param dataDummyHierarchy1 dataDummyHierarchy1
#' @param dataDummyHierarchy2 dataDummyHierarchy2
#' @param codeFrame1 codeFrame1
#' @param codeFrame2 codeFrame2
#' @param makeDimnames makeDimnames
#'
#' @keywords internal
#'
CrossDataDummyHierarchy <- function(dataDummyHierarchy1, dataDummyHierarchy2 = NULL, codeFrame1 = NULL, codeFrame2 = NULL, makeDimnames = FALSE, useMatrixToDataFrame = TRUE) {
  if (is.null(dataDummyHierarchy2)) {
    if (is.null(codeFrame1)) 
      return(dataDummyHierarchy1) else return(list(dataDummyHierarchy = dataDummyHierarchy1, codeFrame = codeFrame1))
  }
  
  if (is.null(codeFrame1) | is.null(codeFrame2)) 
    return(KhatriRao(dataDummyHierarchy2, dataDummyHierarchy1, make.dimnames = makeDimnames))
  return(list(dataDummyHierarchy = KhatriRao(dataDummyHierarchy2, dataDummyHierarchy1, make.dimnames = makeDimnames), codeFrame = CrossCodeFrames(codeFrame1, codeFrame2, useMatrixToDataFrame = useMatrixToDataFrame)))
}



ReduceDataByDummyHierarchiesAndValue <- function(data, dummyHierarchies, valueVar, colVar) {
  if (length(valueVar)>1){
    sel <- rowSums(abs(data[, valueVar])) != 0
  } else {
    sel <- data[, valueVar, drop = TRUE] != 0
  }
  for (i in seq_len(length(dummyHierarchies))) {
    if (!is.null(dummyHierarchies[[i]])) {
      keepCodes <- colnames(dummyHierarchies[[i]])[colSums(abs(dummyHierarchies[[i]])) != 0]
      sel <- sel & (data[, names(dummyHierarchies)[i], drop = TRUE] %in% keepCodes)
    }
  }
  # må sørge for minst en av hver colVar
  if (length(colVar) > 0) {
    setTRUE <- match(unique(data[, colVar, drop = TRUE]), data[, colVar, drop = TRUE])
    sel[setTRUE] <- TRUE  # Sørger for misnt en rad            # kan forbedre dette
  }
  data[sel, , drop = FALSE]
}



CrossprodUnionComplement <- function(x, y) {
  # cat('&')
  yPlus <- y
  yMinus <- y
  yPlus[y < 0] <- 0
  yMinus[y > 0] <- 0
  zPlus <- Mult_crossprod(x, yPlus)
  zMinus <- Mult_crossprod(x, yMinus)
  zPlus[zPlus > 1] <- 1
  z <- zPlus + zMinus
  z[z < 0] <- 0
  z
}




GetFirstStringInList <- function(x) {
  if (!is.list(x)) 
    stop("list needed")
  z <- rep("", length(x))
  for (i in seq_len(length(x))) {
    if (is.character(x[[i]])) 
      z[i] <- x[[i]][1]
  }
  z
}




KhatriRaoReOrder = function(x,y,make.dimnames = FALSE){
  a <- nrow(x)
  b <- nrow(y)
  r <- rep(b*SeqInc(0,a-1),b) + rep(seq_len(b),each=a)
  KhatriRao(x, y, make.dimnames = make.dimnames)[r, ,drop=FALSE]
}

CrossDataDummyHierarchyReOrder <- function(dataDummyHierarchy1, dataDummyHierarchy2 = NULL, codeFrame1 = NULL, codeFrame2 = NULL, makeDimnames = FALSE, useMatrixToDataFrame = TRUE) {
  if (is.null(dataDummyHierarchy2)) {
    if (is.null(codeFrame1)) 
      return(dataDummyHierarchy1) else return(list(dataDummyHierarchy = dataDummyHierarchy1, codeFrame = codeFrame1))
  }
  
  if (is.null(codeFrame1) | is.null(codeFrame2)) 
    return(KhatriRaoReOrder(dataDummyHierarchy2, dataDummyHierarchy1, make.dimnames = makeDimnames))
  
  return(list(dataDummyHierarchy = KhatriRaoReOrder(dataDummyHierarchy2, dataDummyHierarchy1, make.dimnames = makeDimnames), 
              codeFrame = CrossCodeFramesReOrder(codeFrame1, codeFrame2, useMatrixToDataFrame = useMatrixToDataFrame)))
}

CrossCodeFramesAReOrder <- function(codeFrame1, codeFrame2) {
  n1 <- NROW(codeFrame1)
  n2 <- NROW(codeFrame2)
  
  r1 <- rep(seq_len(n1), times = 1,  each = n2)
  
  rownames(codeFrame1) <- NULL
  
  print(cbind(codeFrame1[r1, , drop = FALSE], codeFrame2))
  
  cbind(codeFrame1[r1, , drop = FALSE], codeFrame2)
}


CrossCodeFramesReOrder <- function(codeFrame1, codeFrame2, useMatrixToDataFrame = TRUE) {
  if (!useMatrixToDataFrame) 
    return(CrossCodeFramesAReOrder(codeFrame1, codeFrame2))

  n1 <- NROW(codeFrame1)
  n2 <- NROW(codeFrame2)
  
  codeFrame1 <- DataFrameToMatrix(codeFrame1)
  codeFrame2 <- DataFrameToMatrix(codeFrame2)
  
  rownames(codeFrame1) <- NULL
  rownames(codeFrame2) <- NULL
  
  r1 <- rep(seq_len(n1), times = 1,  each = n2)
  r2 <- rep(seq_len(n2), times = n1, each = 1)
  
  z <- cbind(codeFrame1[r1, , drop = FALSE], codeFrame2[r2, , drop = FALSE])
  
  attr(z, "namesDF") <- c(attr(codeFrame1, "namesDF"), attr(codeFrame2, "namesDF"))
  attr(z, "classDF") <- c(attr(codeFrame1, "classDF"), attr(codeFrame2, "classDF"))
  attr(z, "levelsDF") <- c(attr(codeFrame1, "levelsDF"), attr(codeFrame2, "levelsDF"))
  
  MatrixToDataFrame(z)
}
