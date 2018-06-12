#' Stack columns from a data frame and include variables.
#'
#' @param data  A data frame 
#' @param stackVar Indices of variables to be stacked 
#' @param blockVar Indices of variables to be replicated
#' @param rowData A separate data frame where NROW(rowData)=length(stackVar) 
#'        such that each row may contain multiple information of each stackVar variable.
#'        The output data frame will contain an extended variant of rowData.
#' @param valueName Name of the stacked/concatenated output variable 
#' @param indName Name of the output variable with information of which vector in x the observation originated.
#'        When indName is NULL this variable is not included in output.
#'
#' @return A data frame where the variable ordering corresponds to: blockVar, rowData, valueName, indName
#' 
#' 
#' @export
#' @importFrom utils stack
#' 
#' @seealso  \code{\link{Unstack}}
#'
#' @examples
#' 
#'  z <- data.frame(n=c(10,20,30), ssb=c('S','S','B'),
#'  Ayes=1:3,Ano=4:6,Byes=7:9,Bno=10:12)
#'  zRow <- data.frame(letter=c('A','A','B','B'),answer=c('yes','no','yes','no') )
#'  
#'  x <- Stack(z,3:6,1:2,zRow)
#'  
#'  Unstack(x,6,3:4,numeric(0),1:2)
#'  Unstack(x,6,5,numeric(0),1:2)
#'  Unstack(x,6,3:4,5,1:2)
Stack <- function(data, stackVar = 1:NCOL(data), blockVar = integer(0), rowData = data.frame(stackVar)[, 
                                                                                                       integer(0), drop = FALSE], valueName = "values", indName = "ind") {
  z <- stack(data, stackVar)
  zind <- 1
  if (is.null(indName)) 
    z <- z[, 1, drop = FALSE] else zind <- 2:1
  colnames(z) <- c(valueName, indName)
  cbind(data[, blockVar, drop = FALSE], rowData[RepEach(seq_len(length(stackVar)), 
                                                        NROW(data)), , drop = FALSE], z[, zind, drop = FALSE], row.names = seq_len(length(stackVar) * 
                                                                                                                                     NROW(data)))
}



#' Unstack a column from a data frame and include additional variables.
#'
#' @param data A data frame 
#' @param mainVar Index of the variable to be unstacked   
#' @param stackVar Index of variables defining the unstack grouping 
#' @param extraVar Indices of within-replicated variables to be added to the rowData output 
#' @param blockVar Indices of between-replicated variables to be added to the data output 
#' @param sep A character string to separate when creating variable names
#' @param returnRowData When FALSE output is no list, but only data
#' @param sorted When TRUE the created variables is in sorted order. Otherwise input order is used.
#'
#' @return When returnRowData=TRUE output is list of two elements.
#'         \item{data}{Unstacked data}
#'         \item{rowData}{A separate data frame with one row for each unstack grouping composed of the stackVar variables}
#' @export
#' @importFrom utils unstack
#' @importFrom stats formula
#'
#' @seealso  \code{\link{Stack}}  (examples)
Unstack <- function(data, mainVar = 1, stackVar = (1:NCOL(data))[-mainVar], extraVar = integer(0), 
                    blockVar = integer(0), sep = "_", returnRowData = TRUE, sorted = FALSE) {
  if (length(mainVar) != 1) 
    stop("mainVar must be single")
  n <- NROW(data)
  stackVarSingle <- apply(data[, stackVar, drop = FALSE], 1, paste, collapse = sep)
  if (!sorted) {
    # stackVarSingle become factor ok?
    stackVarSingle <- factor(stackVarSingle, levels = unique(as.character(stackVarSingle)))
    ss <- NULL
    ss$ix <- order(stackVarSingle)
    ss$x <- stackVarSingle[ss$ix]
  } else {
    ss <- sort(stackVarSingle, index.return = TRUE)
  }
  uniquerows <- ss$ix[!duplicated(ss$x)]
  nUnique <- length(uniquerows)
  if (nUnique == n) 
    stop("All unique")
  if (length(extraVar) > 0) {
    if (Nlevels(data[, c(stackVar, extraVar), drop = FALSE]) != nUnique) 
      stop("extraVar is not consistent with stackVar.")
  }
  rowData <- data[uniquerows, c(stackVar, extraVar), drop = FALSE]
  rownames(rowData) <- stackVarSingle[uniquerows]
  data[, stackVar[1]] <- stackVarSingle
  formulaString <- paste(names(data)[mainVar], "~", names(data)[stackVar[1]])
  x <- unstack(data, formula(formulaString))
  if (!is.data.frame(x)) 
    stop("all columns not same length")
  
  match_ <- match(colnames(x), stackVarSingle[uniquerows])
  if (any(is.na(match_))) 
    match_ <- match(colnames(x), paste("X", stackVarSingle[uniquerows], sep = ""))
  if (any(is.na(match_))) 
    warning("Could not extra check to guarantee correct ordering") else rowData <- rowData[match_, , drop = FALSE]
  if (length(blockVar) > 0) {
    blockData <- data[seq_len(NROW(x)), blockVar, drop = FALSE]
    for (j in seq_len(length(blockVar))) {
      i <- blockVar[j]
      formulaString <- paste(names(data)[i], "~", names(data)[stackVar[1]])
      v <- apply(unstack(data, formula(formulaString)), 1, unique)
      if (is.list(v)) 
        stop(paste(names(data)[i], " cannot be used as blockVar."))
      if (NCOL(v) != 1) 
        stop(paste(names(data)[i], " cannot be used as blockVar."))
      blockData[, j] <- v
    }
    rownames(blockData) <- NULL
    x <- cbind(blockData, x)
  }
  if (!returnRowData) 
    return(x)
  list(data = x, rowData = rowData)
}

nosort <- function(x) list(x = x, ix = seq_len(length(x)))

RepEach <- function(x, n) as.vector(t(matrix(rep(x, n), ncol = n)))

