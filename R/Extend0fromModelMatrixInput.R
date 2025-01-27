# 
# This function is made by copying code from Package GaussSuppression.
# Extend0fromHierarchies is a direct copy. 
# Other code is copy of lines within GaussSuppressionFromData.
# Minor difference is the parameter isExtend0 instead of changing extend0 from input.
# In the future, this function should be moved to SSBtools and also be used by  Package GaussSuppression.
# 

#' A specialized version of Extend0()
#' 
#' `Extend0fromModelMatrixInput()` is a specialized function that extends the input data based on the provided parameters.
#' It is designed specifically to work with input to [ModelMatrix()].
#'
#' - `Extend0fromModelMatrixInput()`: The main function that processes and extends input data according to the specified parameters.
#' - `IsExtend0()`: A helper function that evaluates the `extend0` parameter and returns `TRUE` or `FALSE`, indicating whether the data should be extended.
#' 
#' @inheritParams ModelMatrix 
#' @inheritParams Extend0
#' @param data Input data frame 
#' @param extend0  When `extend0` is set to `TRUE`, the data is automatically extended.  
#'        Additionally, `extend0` can be specified as a list, representing the `varGroups` parameter 
#'        in the \code{\link{Extend0}} function. 
#'        Can also be set to `"all"` which means that input codes in hierarchies 
#'        are considered in addition to those in data.
#' @param dVar Optional. Specifies the `dimVar` input for [Extend0()]. 
#'        If not provided, `dimVar` is calculated by the [NamesFromModelMatrixInput()] function.
#' @param ... Further arguments to underlying functions. 
#'
#' @return Extended data frame
#' @export
#' @keywords internal
#' 
#' @seealso [Extend0()]
#'
Extend0fromModelMatrixInput = function(data, 
                                       freqName, 
                                       hierarchies,
                                       formula,
                                       dimVar,
                                       extend0, 
                                       dVar = NULL, ...){
  
  e0 <- Extend0recode(extend0)
  
  if (!e0$isExtend0) {
    return(data)
  }
  extend0all <- e0$extend0all
  varGroups <- e0$varGroups
  
  if(is.null(dVar)){
    dVar <- NamesFromModelMatrixInput(hierarchies = hierarchies, formula = formula, dimVar = dimVar)
  }
  
  
  # Capture possible avoidHierarchical argument to Formula2ModelMatrix
  if (!is.null(formula) & is.null(hierarchies)) {
    AH <- function(avoidHierarchical = FALSE, ...){avoidHierarchical}
    avoidHierarchical <- AH(...)
  } else {
    avoidHierarchical <- FALSE
  }
  
  
  # To keep hierarchical = FALSE in Extend0 when !is.null(hierarchies):  AutoHierarchies needed first  when unnamed elements in hierarchies  
  # AutoHierarchies needed also when extend0all
  if (!is.null(hierarchies)) {
    if (is.null(names(hierarchies))) names(hierarchies) <- rep(NA, length(hierarchies))
    toFindDimLists <- (names(hierarchies) %in% c(NA, "")) & (sapply(hierarchies, is.character))  # toFindDimLists created exactly as in AutoHierarchies
  } else {
    toFindDimLists <- FALSE # sum is 0 below
  }  
  if (!is.null(hierarchies) & is.null(varGroups) & (sum(toFindDimLists) | extend0all)) {
    data = Extend0fromHierarchies(data, freqName = freqName, hierarchies = hierarchies, 
                                  dimVar = dVar, extend0all = extend0all, ...)
    hierarchies <- data$hierarchies
    data <- data$data 
  } else {
    data <- Extend0(data, freqName = freqName, dimVar = dVar,  varGroups = varGroups, extraVar = TRUE, 
                    hierarchical = !avoidHierarchical & is.null(hierarchies))
  }
  data
}


Extend0recode <- function(extend0, asLogical = FALSE) {
  extend0all <- FALSE
  if (is.list(extend0)) {
    varGroups <- extend0
    extend0 <- TRUE
  } else {
    varGroups <- NULL
    if (is.character(extend0)) {
      if (extend0 == "all") {
        extend0all <- TRUE
        extend0 <- TRUE
      } else {
        stop("extend0 must be \"all\" when supplied as character")
      }
    }
  }
  if (asLogical) {
    return(extend0)
  }
  if (!extend0) {
    extend0all <- NULL
  }
  list(isExtend0 = extend0, extend0all = extend0all, varGroups = varGroups)
}


#' @rdname Extend0fromModelMatrixInput
#' @export
#' @keywords internal
#'
IsExtend0 <- function(extend0) {
  Extend0recode(extend0, asLogical = TRUE)
}
  




# This function is made to handle cases with extend0all and cases with special hierarchies (unnamed list) so that SSBtools:::NamesFromHierarchies before and after AutoHierarchies differ. 
# A straightforward handling of the latter means that some  dimVar become extraVar. Then if they are numeric, they will be set to 0. This function change this to NA (maybe NA can be solved by future parameter to Extend0 ).
# Hopefully this function also avoids that dimVar become extraVar.  I general hierarchical relationships are described by hierarchies and not data. But when data are used to generate (parts of) the hierarchies, data may be used.
# All the code made to avoid dimVar become extraVar have no practical consequences for standard use of GaussSuppressionFromData. The code is relevant  when "inner" is included in output.
# This function may be a future function in SSBtools. Then dimVar is not needed in input. Instead found by  SSBtools:::NamesFromHierarchies


Extend0fromHierarchies <- function(data, freqName, hierarchies, dimVar, extend0all, ...) {
  
  toFindDimLists <- (names(hierarchies) %in% c(NA, "")) & (sapply(hierarchies, is.character))  # toFindDimLists created exactly as in AutoHierarchies
  all_toFindDimLists <- unique(unlist(hierarchies[toFindDimLists]))
  
  hierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, ...)
  dVar <- dimVar  # as in code before separate function
  dVar_ <- names(hierarchies)
  
  varGroups <- as.list(dVar_)  # This is standard in Extend0 when !hierarchical
  for (i in seq_along(varGroups)) {
    varGroups[[i]] <- unique(data[varGroups[[i]]])  # This is standard in Extend0
  }
  
  if (extend0all) {
    for (i in seq_along(varGroups)) {
      mapsFrom <- unique(hierarchies[[dVar_[i]]]$mapsFrom)
      mapsTo <- unique(hierarchies[[dVar_[i]]]$mapsTo)
      mapsExtra <- mapsFrom[!(mapsFrom %in% mapsTo)]
      mapsExtra <- mapsExtra[!(mapsExtra %in% varGroups[[i]][[1]])]
      if (length(mapsExtra)) {
        extra_varGroups_i <- varGroups[[i]][rep(1, length(mapsExtra)), , drop = FALSE]
        extra_varGroups_i[[1]] <- mapsExtra
        varGroups[[i]] <- rbind(varGroups[[i]], extra_varGroups_i)
      }
    }
  }
  if (length(dVar_) < length(dVar)) {
    varGroups_special <- HierarchicalGroups2(data[all_toFindDimLists])
    if (is.null(names(varGroups))) {
      names(varGroups) <- unlist(lapply(varGroups, names))
    }
    varGroups_special <- varGroups_special[names(varGroups_special) %in% names(varGroups)]
    removenames <- unique(unlist(varGroups_special))
    removenames <- removenames[!(removenames %in% names(varGroups_special))]
    removenames <- removenames[(removenames %in% names(varGroups))]
    for (i in seq_along(varGroups_special)) {
      varGroups_special[[i]] <- varGroups_special[[i]][!(varGroups_special[[i]] %in% removenames)]
    }
    varGroups_special <- varGroups_special[unlist(lapply(varGroups_special, length)) > 1]
    if (length(varGroups_special)) {
      dVar_ <- unique(c(dVar_, unlist(varGroups_special)))
      for (i in seq_along(varGroups_special)) {
        varGroups_special[[i]] <- unique(data[varGroups_special[[i]]])  # This is standard in Extend0
      }
    }
    ma <- match(names(varGroups_special), names(varGroups))
    for (i in seq_along(varGroups_special)) {
      if (nrow(varGroups[[ma[i]]]) == nrow(varGroups_special[[i]])) {
        varGroups[[ma[i]]] <- varGroups_special[[i]]
      }
    }
  }
  
  nrowPreExtend0 <- nrow(data)
  
  data <- Extend0(data, freqName = freqName, dimVar = NULL, varGroups = varGroups, extraVar = TRUE, hierarchical = FALSE)
  
  # Set to NA instead of 0 for possible numeric dimVar not in hierarchy after AutoHierarchies (above)
  if (length(dVar_) < length(dVar)) {
    warning("Some dimVar columns set to NA in extended part of data")
    extra_dVar <- dVar[!(dVar %in% dVar_)]
    extra_dVar <- extra_dVar[sapply(data[1, extra_dVar], is.numeric)]
    if (length(extra_dVar)) {
      newrows <- SeqInc(nrowPreExtend0 + 1L, nrow(data))
      if (length(newrows)) {
        data[newrows, extra_dVar] <- NA
      }
    }
  }
  list(data = data, hierarchies = hierarchies)
}
