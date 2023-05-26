
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
  if (is.list(inputInOutput)) {   # When list: Extended use of inputInOutput (hack)
    inputInOutput <- inputInOutput[[1]]
    if (is.character(inputInOutput)) {
      ma <- match(inputInOutput, rownames(m))
      if (anyNA(ma)) {
        warning(paste("Output codes not found in the hierarchy result in empties:", 
                      paste(HeadEnd(inputInOutput[is.na(ma)]), collapse = ", ")))
        m0 <- Matrix(0, sum(is.na(ma)), ncol(m))
        rownames(m0) <- inputInOutput[is.na(ma)]
        m <- rbind(m, m0)
        ma <- match(inputInOutput, rownames(m))
      }
      m <- m[ma, , drop = FALSE]
      return(m)
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