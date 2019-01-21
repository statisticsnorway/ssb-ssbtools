

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
#' @param valueVar Name of the variable to be aggregated.
#' @param rowSelect Data frame specifying variable combinations for output. The colFactor variable is not included.
#' @param colSelect Vector specifying categories of the colFactor variable for output.
#' @param inputInOutput Logical vector (possibly recycled) for each element of hierarchies.
#'         TRUE means that codes from input are included in output. Values corresponding to \code{"rowFactor"} and \code{"colFactor"} are ignored.
#' @param output One of "data.frame" (default), "dummyHierarchies", "outputMatrix", "dataDummyHierarchy", "valueMatrix", "fromCrossCode",
#'        "toCrossCode", "crossCode" (as toCrossCode), "outputMatrixWithCrossCode", "matrixComponents".
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
#'
#' @return As specified by the parameter \code{output}
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
HierarchyCompute <- function(data, hierarchies, valueVar, 
                             rowSelect = NULL, colSelect = NULL, inputInOutput = FALSE, output = "data.frame", 
                             autoLevel = TRUE, unionComplement = FALSE, constantsInOutput = NULL, 
                             hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                             selectionByMultiplicationLimit = 10^7, colNotInDataWarning = TRUE, useMatrixToDataFrame = TRUE,
                             handleDuplicated = "sum",
                             asInput = FALSE,
                             verbose = FALSE) {
  
  
  # Fixed values instead of parameters to function
  hierarchyNamesForOutput <- NULL
  orderAsRowSelect <- TRUE
  outputMatrixSelection <- FALSE
  reductionWhenKhatriRao <- TRUE
  stringsAsFactors <- FALSE
  setFALSEinputInOutput <- TRUE
  reduceDataByDummyHierarchiesAndValue <- TRUE
  
  noRowGroupsWhenNoColVar <- TRUE
  
  if(!(handleDuplicated %in% c("sum", "sumByAggregate", "sumWithWarning", "stop", "single", "singleWithWarning")))
    stop("invalid 'handleDuplicated' argument")
  
  if(output=="dataDummyHierarchyWithCodeFrame")
    reduceDataByDummyHierarchiesAndValue <- FALSE
  
  if(asInput){
    reduceDataByDummyHierarchiesAndValue <- FALSE
    if(handleDuplicated !="sum")
      stop("'handleDuplicated' must be 'sum' when 'asInput'")
  }
  
  if(handleDuplicated !="sum")
    noRowGroupsWhenNoColVar <- FALSE
  
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
  
  # if(length(colInd)!=1){ stop('A single colVar needed in this implementation') }
  
  if (length(colInd) > 1) {
    stop("Only a single colVar allowed in this implementation")
  }
  
  if (length(colInd) == 0) {
    #warning("No colVar is newly implemented and little tested")
    noColVar <- TRUE
    colSelect <- NULL
  } else noColVar <- FALSE
  
  
  if (!noColVar & asInput) 
    stop("asInput only possible when no colFactor")
  
  
  if (noColVar) 
    hierarchyInd <- seq_len(nHier) else hierarchyInd <- seq_len(nHier)[-colInd]  # colVar excluded
  
  hierarchyNames <- names(hierarchies)[hierarchyInd]  # colVar excluded
  
  colVar <- names(hierarchies)[colInd]
  
  
  dummyHierarchies <- vector("list", nHier)
  dataDummyHierarchies <- vector("list", nHier)
  codeFrames <- vector("list", nHier)
  names(dummyHierarchies) <- names(hierarchies)
  names(dataDummyHierarchies) <- names(hierarchies)
  names(codeFrames) <- names(hierarchies)
  
  
  
  if (!is.null(rowSelect)) {
    if (setFALSEinputInOutput) 
      inputInOutput[] <- FALSE  # Handeled by keep...
    inputColnamesRowSelect <- colnames(rowSelect)
    rowSelect <- CharacterDataFrame(rowSelect[, hierarchyNames, drop = FALSE])  # CharacterDataFrame van be roemoved below? 
  }
  
  
  for (i in seq_len(nHier)) {
    if (is.list(hierarchies[[i]])) {
      
      
      hierarchies[[i]] <- FixHierarchy(hierarchies[[i]], hierarchyVarNames)
      if (autoLevel[i]) 
        hierarchies[[i]] <- AutoLevel(hierarchies[[i]])
      hierarchies[i] <- AddMapsInput(hierarchies[i], data)  # AddMapsInput(hierarchies[i],dataOriginal)
      if (!is.null(rowSelect)) {
        hierarchies[i] <- AddNonExistingCode(hierarchies[i], rowSelect, inputInOutput[i])
        mapsInput <- attr(hierarchies[[i]], "mapsInput")
        keepCodes <- attr(hierarchies[[i]], "keepCodes")
        hierarchies[[i]] <- AutoLevel(hierarchies[[i]])
        attr(hierarchies[[i]], "mapsInput") <- mapsInput
        attr(hierarchies[[i]], "keepCodes") <- keepCodes
      }
      
      dummyHierarchies[[i]] <- DummyHierarchy(mapsFrom = hierarchies[[i]]$mapsFrom, mapsTo = hierarchies[[i]]$mapsTo, mapsInput = attr(hierarchies[[i]], "mapsInput"), keepCodes = attr(hierarchies[[i]], "keepCodes"), sign = hierarchies[[i]]$sign, level = hierarchies[[i]]$level, inputInOutput = inputInOutput[i], unionComplement = unionComplement[i])
    } else {
      if (hierarchies[[i]] == "rowFactor") {
        dummyHierarchies[[i]] <- fac2sparse(sort(factor(unique(data[, names(hierarchies)[i]]))))
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
      iRows <- rownames(dummyHierarchies[[i]]) %in% rowSelect[, names(hierarchies)[i]]
      dummyHierarchies[[i]] <- dummyHierarchies[[i]][iRows, , drop = FALSE]
    }
  }
  
  
  if (!is.null(colSelect)) {
    datacolvar <- as.character(data[, colVar])
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
  
  
  if (reduceDataByDummyHierarchiesAndValue) 
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
    
    
    if ((!selectionByMultiplication) & !reductionWhenKhatriRao) 
      runCrossDataDummyHierarchies <- TRUE
  }
  
  if (runCrossDataDummyHierarchies) {
    k <- CrossDataDummyHierarchies(dataDummyHierarchies = dataDummyHierarchies[hierarchyInd], codeFrames = codeFrames[hierarchyInd], makeDimnames = TRUE, 
                                   useMatrixToDataFrame = useMatrixToDataFrame, removeEmpty = removeEmpty, verbose = verbose)
  } else {
    
    k <- list(dataDummyHierarchy = NULL, codeFrame = CharacterDataFrame(rowSelect[, hierarchyNames, drop = FALSE]))
    if (!selectionByMultiplication) {
      if (reductionWhenKhatriRao) 
        k <- ReductionCrossDataDummyHierarchies(dataDummyHierarchies[hierarchyInd], codeFrames = codeFrames[hierarchyInd], codeFrame = k[[2]], 
                                                makeDimnames = TRUE, useMatrixToDataFrame = useMatrixToDataFrame, verbose = verbose)
    } else {
      k[[1]] <- SelectionCrossDataDummyHierarchy(dataDummyHierarchies[hierarchyInd], k$codeFrame, verbose = verbose)
    }
  }
  
  
  if(output=="dataDummyHierarchyWithCodeFrame")
    return(k)
  
  
  readyValueMatrix <- FALSE
  if (noColVar) {
    valueMatrix <- Matrix(0, dim(rowGroups$groups)[1], 1)
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
            aggData = aggregate(value~idx,
                                data=data.frame(idx = rowGroups$idx, value = data[, valueVar]),
                                FUN=sum)
            valueMatrix[aggData$idx, 1] <- aggData$value
            rm(aggData)
            readyValueMatrix <- TRUE
          }
        }
      }
    # matrix isteden Nesten ingen forskjell
    if(!readyValueMatrix)
      valueMatrix[rowGroups$idx, 1] <- data[, valueVar]
  } else {
    
    colData <- factor(data[, colVar])
    integerColData <- as.integer(colData)
    nCol <- max(integerColData)
    
    valueMatrix <- Matrix(0, dim(rowGroups$groups)[1], nCol)
    # matrix isteden Nesten ingen forskjell
    colnames(valueMatrix) <- levels(colData)
    
    idx_integerColData <- cbind(idx = rowGroups$idx, integerColData = integerColData)
  
    #if(!(handleDuplicated %in% c("sum", "sumWithWarning", "stop", "single", "singleWithWarning")))
    #  stop("invalid 'handleDuplicated' argument")
    if(FALSE){
      aggData = aggregate(value~idx+integerColData,
              data=data.frame(idx = rowGroups$idx, integerColData = integerColData, value = data[, valueVar]),
              FUN=sum)
      valueMatrix[cbind(aggData$idx,aggData$integerColData)] <- aggData$value
      rm(aggData)
    }
    
    if(FALSE){
    if(anyDuplicated(idx_integerColData)){
      stop("Duplicated rows in input not handled in this version.")
    }
    
    valueMatrix[idx_integerColData] <- data[, valueVar]
    }
    
    
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
            aggData = aggregate(value~idx+integerColData,
                                data=data.frame(idx = rowGroups$idx, integerColData = integerColData, value = data[, valueVar]),
                                FUN=sum)
            valueMatrix[cbind(aggData$idx,aggData$integerColData)] <- aggData$value
            rm(aggData)
            readyValueMatrix <- TRUE
          }
        }
      }
    # matrix isteden Nesten ingen forskjell
    if(!readyValueMatrix)
      valueMatrix[idx_integerColData] <- data[, valueVar]

    
  }
  
  
  if ((!is.null(rowSelect)) & runCrossDataDummyHierarchies) {
    #### Kode for å lagre utvalgte kombinasjoner
    rg <- RowGroups(rbind(CharacterDataFrame(k$codeFrame), CharacterDataFrame(rowSelect[, hierarchyNames, drop = FALSE])))
    ## Trengs CharacterDataFrame?? men på sikre side ..
    
    rg1 <- rg[seq_len(dim(k$codeFrame)[1])]
    rg2 <- rg[-seq_len(dim(k$codeFrame)[1])]
    selectedRows <- match(rg2, rg1)
  } else {
    selectedRows <- NULL
  }
  
  
  if (is.null(selectedRows)) {
    outputMatrix <- k[[1]] %*% valueMatrix
    xCrossCode <- k$codeFrame
  } else {
    if (outputMatrixSelection) {
      outputMatrix <- (k[[1]] %*% valueMatrix)[selectedRows, , drop = FALSE]
    } else {
      # sparer litt på denne
      outputMatrix <- k[[1]][selectedRows, , drop = FALSE] %*% valueMatrix
    }
    xCrossCode <- k$codeFrame[selectedRows, , drop = FALSE]
  }
  
  xCrossCode <- CharacterDataFrame(xCrossCode)
  rownames(xCrossCode) <- NULL
  
  if ((!is.null(rowSelect)) & orderAsRowSelect & is.null(hierarchyNamesForOutput)) {
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
  
  z <- data.frame(a = as.vector(x), stringsAsFactors = stringsAsFactors)
  names(z) <- valueVar
  
  # return(list(x=x,colVar=colVar))
  
  if (noColVar) {
    colDataSelected <- xCrossCode[, integer(0)]
  } else {
    colDataSelected <- data.frame(a = rep(colnames(x), times = 1, each = dim(x)[1]), stringsAsFactors = stringsAsFactors)
    names(colDataSelected) <- colVar
  }
  
  if (!is.null(constantsInOutput)) 
    w <- cbind(constantsInOutput, colDataSelected, xCrossCode, z) else w <- cbind(colDataSelected, xCrossCode, z)
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
        mapsInput <- c(mapsInput, unique(data[, names(hierarchies)[i]]))
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
      
      uniqueRowSelect <- unique(rowSelect[, names(hierarchies)[i]])
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
                                      removeEmpty = FALSE, verbose = FALSE) {
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
        z <- CrossDataDummyHierarchy(dataDummyHierarchy1 = dataDummyHierarchies[[1]], codeFrame1 = codeFrames[[1]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      else
        z <- CrossDataDummyHierarchy(z[[1]], dataDummyHierarchies[[i]], z[[2]], codeFrames[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      if(removeEmpty){
        rowNonZero <- RowNonZero(z[[1]])
        z[[1]] <- z[[1]][rowNonZero, , drop = FALSE]
        z[[2]] <- z[[2]][rowNonZero, , drop = FALSE]
      }
    }
  } else {
    for (i in seq_len(n)){ 
      if(i==1)
        z <- CrossDataDummyHierarchy(dataDummyHierarchy1 = dataDummyHierarchies[[1]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      else
        z <- CrossDataDummyHierarchy(z, dataDummyHierarchies[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
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
    
    selecti <- z[[2]][, 1] %in% codeFrame[, 1]
    z[[1]] <- z[[1]][selecti, , drop = FALSE]
    z[[2]] <- z[[2]][selecti, , drop = FALSE]
    
    
    for (i in matlabColon(2, n)) {
      
      z <- CrossDataDummyHierarchy(z[[1]], dataDummyHierarchies[[i]], z[[2]], codeFrames[[i]], makeDimnames = makeDimnames, useMatrixToDataFrame = useMatrixToDataFrame)
      
      
        selecti <- Match(unique(codeFrame[, seq_len(i)]), z[[2]])
        #w <<- list(unique(codeFrame[, seq_len(i)]), z[[2]])
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
  z <- SelectionDataDummyHierarchy(dataDummyHierarchies[[1]], codeFrame[, names(dataDummyHierarchies)[1]])
  for (i in matlabColon(2, n)) {
    z <- z * SelectionDataDummyHierarchy(dataDummyHierarchies[[i]], codeFrame[, names(dataDummyHierarchies)[i]])
  }
  if(verbose){
    cat("]")
    flush.console()}
  z
}







#' FixHierarchy
#'
#' @param hi hi
#' @param hierarchyVarNames hierarchyVarNames
#'
#' @keywords internal
#'
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


#' AutoLevel
#'
#' @param x x
#'
#' @return x
#' @keywords internal
#'
AutoLevel <- function(x) {
  mapsFrom <- as.character(x$mapsFrom)
  mapsTo <- as.character(x$mapsTo)
  mapsInput <- mapsFrom[!(mapsFrom %in% mapsTo)]
  sign <- x$sign
  if (any(mapsFrom == mapsTo)) {
    sel <- !(mapsFrom == mapsTo)
    mapsFrom <- mapsFrom[sel]
    mapsTo <- mapsTo[sel]
    mapsInput <- mapsInput[sel]
    sign <- sign[sel]
    warning("hierarchy rows where mapsFrom==mapsTo removed")
  }
  
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
#'
#' The elements of the matrix specify how columns contribute to rows.
#'
#'
#' @param mapsFrom Character vector from hierarchy table
#' @param mapsTo Character vector from hierarchy table
#' @param sign  Numeric vector of either 1 or -1 from hierarchy table
#' @param level Numeric vector from hierarchy table
#' @param mapsInput All codes in mapsTo not in mapsFrom (created automatically when NULL) and possibly other codes in input data.
#' @param inputInOutput When FALSE all output rows represent codes in mapsTo
#' @param keepCodes To prevent some codes to be removed when inputInOutput = TRUE
#' @param unionComplement When TRUE, sign means union and complement instead of addition or subtraction (see note)
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
#' #' # Extend mapsInput - leading to zero columns.
#' DummyHierarchy(h$mapsFrom, h$mapsTo, h$sign, h$level,
#'                mapsInput = c(h$mapsFrom[!(h$mapsFrom %in% h$mapsTo)], "Norway", "Finland"))
DummyHierarchy <- function(mapsFrom, mapsTo, sign, level, mapsInput = NULL, inputInOutput = FALSE, keepCodes = mapsFrom[integer(0)], unionComplement = FALSE) {
  
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
    if (unionComplement) 
      m <- rbind(m, CrossprodUnionComplement(mNew, m))  # Matrix::rBind(m,  CrossprodUnionComplement(mNew,m))
    else m <- rbind(m, crossprod(mNew, m))  # Matrix::rBind(m,  crossprod(mNew,m))
  }
  if (!inputInOutput & length(dropInput) > 0) {
    keepRows <- rownames(m)[!(rownames(m) %in% dropInput)]
    m <- m[keepRows, , drop = FALSE]
  }
  m  # Lage warnig/error om annet i matrisa enn 0, -1, 1 ?
}

#' Create a (signed) dummy matrix for hierarcical mapping of codes in data
#'
#' @param dataVector A vector of codes in data
#' @param dummyHierarchy Output from DummyHierarchy()
#'
#' @return  A sparse matrix.
#' Column names are taken from dataVector (if non-NULL) and row names are taken from
#' the row names of dummyHierarchy.
#' @export
#' @author Øyvind Langsrud
#' @keywords internal
#'
DataDummyHierarchy <- function(dataVector, dummyHierarchy) {
  x <- factor(dataVector, levels = colnames(dummyHierarchy))
  m <- dummyHierarchy[, as.integer(x), drop = FALSE]
  colnames(m) <- names(dataVector)
  m
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
  sel <- data[, valueVar] != 0
  for (i in seq_len(length(dummyHierarchies))) {
    if (!is.null(dummyHierarchies[[i]])) {
      keepCodes <- colnames(dummyHierarchies[[i]])[colSums(abs(dummyHierarchies[[i]])) != 0]
      sel <- sel & (data[, names(dummyHierarchies)[i]] %in% keepCodes)
    }
  }
  # må sørge for minst en av hver colVar
  if (length(colVar) > 0) {
    setTRUE <- match(unique(data[, colVar]), data[, colVar])
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
  zPlus <- crossprod(x, yPlus)
  zMinus <- crossprod(x, yMinus)
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
