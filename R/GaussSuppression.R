#' Secondary suppression by Gaussian elimination
#' 
#' Sequentially the secondary suppression candidates (columns in x) are used to reduce the x-matrix by Gaussian elimination. 
#' Candidates who completely eliminate one or more primary suppressed cells (columns in x) are omitted and made secondary suppressed. 
#' This ensures that the primary suppressed cells do not depend linearly on the non-suppressed cells.  
#' How to order the input candidates is an important choice. 
#' The singleton problem and the related problem of zeros are also handled.
#' 
#' It is possible to specify too many (all) indices as `candidates`. 
#' Indices specified as `primary` or `hidded` will be removed. 
#' Hidden indices (not candidates or primary) refer to cells that will not be published, but do not need protection. 
#' 
#' * **Singleton methods for frequency tables:** 
#'         All singleton methods, except `"sub2Sum"` and the \code{\link{NumSingleton}} methods, have been implemented with frequency tables in mind.
#'         The singleton method `"subSum"` makes new imaginary primary suppressed cells, which are the sum of the singletons 
#'         within each group. The `"subSpace"` method is conservative and ignores the singleton dimensions when looking for 
#'         linear dependency. The default method, `"anySum"`, is between the other two. Instead of making imaginary cells of 
#'         sums within groups, the aim is to handle all possible sums, also across groups. In addition, `"subSumSpace"`  and 
#'         `"subSumAny"` are possible methods, primarily for testing. These methods are similar to `"subSpace"` and `"anySum"`,
#'         and additional cells are created as in `"subSum"`. It is believed that the extra cells are redundant.
#'         Note that in order to give information about unsafe cells, `"anySum"`  is internally changed to `"subSumAny"` when there are forced cells. 
#'         All the above methods assume that any published singletons are primary suppressed. 
#'         When this is not the case, `"anySumNOTprimary"` must be used.
#' * **Singleton methods for magnitude tables:**          
#'  The singleton method `"sub2Sum"` makes new imaginary primary suppressed cells, which are the sum of two inner cells. 
#'  This is done when a group contains exactly two primary suppressed inner cells provided that at least one of them is singleton.
#'  This was the first method implemented. Other magnitude methods follow the coding according to \code{\link{NumSingleton}}.  
#'  The `"sub2Sum"` method is equivalent to `"numFFT"`.
#'  Also note that `"num"`, `"numFFF"` and `"numFTF"` are equivalent to `"none"`.   
#' * **Combined:**  
#'  For advanced use, `singleton` can be a two-element list with names `"freq"` and `"num"`. 
#'  Then `singletonMethod` must be a corresponding named two-element vector.
#'  For example: `singletonMethod = c(freq = "anySumNOTprimary", num = "sub2Sum")`
#'  
#'
#' @param x Matrix that relates cells to be published or suppressed to inner cells. yPublish = crossprod(x,yInner)
#' @param candidates Indices of candidates for secondary suppression   
#' @param primary    Indices of primary suppressed cells
#' @param forced     Indices forced to be not suppressed. `forced` has precedence over `primary`. See `whenPrimaryForced` below.
#' @param hidden     Indices to be removed from the above `candidates` input (see details)  
#' @param singleton Logical or integer vector of length `nrow(x)` specifying inner cells for singleton handling.
#'            Normally, for frequency tables, this means cells with 1s when 0s are non-suppressed and cells with 0s when 0s are suppressed.  
#'            For some singleton methods, integer values representing the unique magnitude table contributors are needed. 
#'            For all other singleton methods, only the values after conversion with `as.logical` matter.      
#' @param singletonMethod Method for handling the problem of singletons and zeros: 
#'             `"anySum"` (default), `"anySumNOTprimary"`, `"subSum"`, `"subSpace"`, `"sub2Sum"`, `"none"` 
#'             or a \code{\link{NumSingleton}} method (see details).
#' @param printInc Printing "..." to console when TRUE
#' @param tolGauss A tolerance parameter for sparse Gaussian elimination and linear dependency. This parameter is used only in cases where integer calculation cannot be used.
#' @param whenEmptySuppressed Function to be called when empty input to primary suppressed cells is problematic. Supply NULL to do nothing.
#' @param whenEmptyUnsuppressed Function to be called when empty input to candidate cells may be problematic. Supply NULL to do nothing.
#' @param whenPrimaryForced Function to be called if any forced cells are primary suppressed (suppression will be ignored). Supply NULL to do nothing.
#'            The same function will also be called when there are forced cells marked as singletons (will be ignored).
#' @param removeDuplicated Whether to remove duplicated columns in `x` before running the main algorithm. 
#' @param iFunction A function to be called during the iterations. See the default function, \code{\link{GaussIterationFunction}}, for description of parameters. 
#' @param iWait The minimum number of seconds between each call to `iFunction`.
#'              Whenever `iWait<Inf`, `iFunction` will also be called after last iteration. 
#' @param xExtraPrimary Extra x-matrix that defines extra primary suppressed cells in addition to those defined by other inputs.  
#' @param unsafeAsNegative  When `TRUE`, unsafe primary cells due to forced cells are included in the output vector as negative indices.              
#' @param ... Extra unused parameters
#'
#' @return Secondary suppression indices  
#' @importFrom Matrix colSums t Matrix
#' @export
#'
#' @examples
#' # Input data
#' df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
#'                  var1 = rep(1:3, each = 5), 
#'                  var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)
#' 
#' # Make output data frame and x 
#' fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
#' x <- fs$modelMatrix
#' datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))
#' 
#' # Add primary suppression 
#' datF$primary <- datF$values
#' datF$primary[datF$values < 5 & datF$values > 0] <- NA
#' datF$suppressedA <- datF$primary
#' datF$suppressedB <- datF$primary
#' datF$suppressedC <- datF$primary
#' 
#' # zero secondary suppressed
#' datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA
#' 
#' # zero not secondary suppressed by first in ordering
#' datF$suppressedB[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
#'                             primary = is.na(datF$primary))] <- NA
#' 
#' # with singleton
#' datF$suppressedC[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
#'                             primary = is.na(datF$primary), singleton = df$values == 1)] <- NA
#' 
#' datF
#' 
GaussSuppression <- function(x, candidates = 1:ncol(x), primary = NULL, forced = NULL, hidden = NULL, 
                             singleton = rep(FALSE, nrow(x)), singletonMethod = "anySum", printInc = TRUE, tolGauss = (.Machine$double.eps)^(1/2),
                             whenEmptySuppressed = warning, 
                             whenEmptyUnsuppressed = message,
                             whenPrimaryForced = warning,
                             removeDuplicated = TRUE, 
                             iFunction = GaussIterationFunction, iWait = Inf,
                             xExtraPrimary = NULL,
                             unsafeAsNegative = FALSE,
                             ...) {
  
  if (identical(removeDuplicated, "test")){
    sysCall <- match.call()
    parentFrame <- parent.frame()
    cat("\n ----------------   removeDuplicated = TRUE  --------------------------\n")
    sysCall["removeDuplicated"] <- TRUE
    outTRUE <- eval(sysCall, envir = parentFrame)
    cat("\n ----------------   removeDuplicated = FALSE  --------------------------\n")
    sysCall["removeDuplicated"] <- FALSE
    outFALSE <- eval(sysCall, envir = parentFrame)
    if(isTRUE(all.equal(outTRUE, outFALSE))){
      return(outTRUE)
    }
    print(outTRUE)
    print(outFALSE)
    stop("removeDuplicated test: Not all equal")
  }
  
  if (is.logical(primary)) 
    primary <- which(primary) 
  else 
    primary <- unique(primary)
  
  
  ncol_x_input <- ncol(x)
  if (!is.null(xExtraPrimary)) {
    # primary has already been converted to indexes
    primary <- c(primary, ncol(x) + seq_len(ncol(xExtraPrimary)))
    # forced and hidden can be untreated since conversion to indexes below
    x <- cbind(x, xExtraPrimary)
  }
  ncol_x_with_xExtraPrimary <- ncol(x)
    
  if (!length(primary)) 
    return(integer(0))
    
  if (is.logical(candidates)) 
    candidates <- which(candidates) 
  else 
    candidates <- unique(candidates)
      
  if (is.logical(hidden)) 
    hidden <- which(hidden) 
  else 
    hidden <- unique(hidden)
        
  if (is.logical(forced)) 
    forced <- which(forced) 
  else forced <- unique(forced)
          
  if (length(hidden)) 
    candidates <- candidates[!(candidates %in% hidden)]
  
  if (!is.null(whenPrimaryForced)) {
    if (any(primary %in% forced)) {
      whenPrimaryForced("Primary suppression of forced cells ignored")
    }
  }
  
  unsafePrimary <- integer(0)
  
  if (removeDuplicated) {
    # idxDD <- DummyDuplicated(x, idx = TRUE, rnd = TRUE)
    idxDD <- DummyDuplicatedSpec(x,  candidates, primary, forced)
    idxDDunique <- unique(idxDD)
    
    if (length(idxDDunique) == length(idxDD)) {
      removeDuplicated <- FALSE
    } else {
      if (length(forced)) { # Needed for warning
        primary <- primary[!(primary %in% forced)]
      }
      
      idNew <- rep(0L, ncol(x))
      idNew[idxDDunique] <- seq_len(length(idxDDunique))
      
      candidatesOld <- candidates
      primaryOld <- primary
      
      primary <- idNew[unique(idxDD[primary])]
      candidates <- idNew[unique(idxDD[candidates])]
      forced <- idNew[unique(idxDD[forced])]
      x <- x[, idxDDunique, drop = FALSE]
      
      if (any(primary %in% forced)) {
        unsafePrimary <- c(unsafePrimary, primary[primary %in% forced])  # c(... since maybe future extension 
        unsafePrimaryAsFinal <- -SecondaryFinal(secondary = -unsafePrimary, primary = integer(0), idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld)
        
        unsafeOrinary <- unsafePrimaryAsFinal[unsafePrimaryAsFinal <= ncol_x_input]
        unsafeExtra <- unsafePrimaryAsFinal[unsafePrimaryAsFinal > ncol_x_input]
        
        if (length(unsafeExtra)) {
          s <- paste0(length(unsafePrimaryAsFinal), " (", length(unsafeOrinary), " ordinary, ", length(unsafeExtra), " extra)")
        } else {
          s <- length(unsafeOrinary)
        }
        warning(paste(s, "unsafe primary cells due to forced cells when evaluating duplicates"))  # Forced cells -> All primary cells are not safe (duplicated)
      }
    }
  }
  
  if (!removeDuplicated) {
    idxDD <- NULL
    idxDDunique <- NULL
    candidatesOld <- NULL
    primaryOld <- NULL
  }
  
  
  candidates <- candidates[!(candidates %in% primary)]
          
  nForced <- length(forced)
  
  if (nForced) {
    primary <- primary[!(primary %in% forced)]
    candidates <- candidates[!(candidates %in% forced)]
    candidates <- c(forced, candidates)
  }
  
  if(is.null(singleton)){
    singleton <- rep(FALSE, nrow(x))
  }
  
  if (is.list(singleton)){
    if(!identical(as.vector(sort(names(singleton))), c("freq", "num"))){
      stop('names of singleton when list must be "freq" and "num"')
    }
    if(!identical(as.vector(sort(names(singleton))), c("freq", "num"))){
      stop('names of singletonMethod when several must be "freq" and "num"')
    }
    singleton_num <- singleton[["num"]]
    singleton <- as.logical(singleton[["freq"]])
    singletonMethod_num <- singletonMethod[["num"]] 
    singletonMethod <- singletonMethod[["freq"]]
  } else {
    if (is.logical(singleton)) {
      if(length(singleton) == 1L){
        singleton <- rep(singleton, nrow(x))
      }
    }
    if(is.integer(singleton)){
      singleton_num <- singleton
      singleton <- as.logical(singleton)
    } else {
      singleton_num <- singleton
    }
    if (!is.logical(singleton)) {
      stop("singleton must be logical or integer")
    }
    if (singletonMethod %in% c("sub2Sum") | !is.null(NumSingleton(singletonMethod))) {
      singletonMethod_num <- singletonMethod
      singletonMethod <- "none"
    } else {
      singletonMethod_num <- "none"
    }
  }
  if (is.integer(singleton_num)) {
    if (min(singleton_num) < 0) {
      stop("integer singletons must be nonzero")
    }
  }
  if(length(singleton) != nrow(x) | length(singleton_num) != nrow(x)){
    stop("length(singleton) must be nrow(x)")
  }
  
  #if (is.function(singletonMethod)) {   # Alternative function possible
  #  return(singletonMethod(x, candidates, primary, printInc, singleton = singleton, nForced = nForced))
  #}
  
  if (!(singletonMethod %in% c("subSum", "subSpace", "anySum", "anySumNOTprimary", "subSumSpace", "subSumAny", "none"))) {
    stop("wrong singletonMethod")
  }
  if (singletonMethod_num == "sub2Sum") {
    singletonMethod_num <- "numFFT"
  }
  #if (singletonMethod_num == "sub2SumUnique") {
  #  singletonMethod_num <- "numFTT"
  #}
  if (singletonMethod_num == "none") {
    singletonMethod_num <- "num"
  }
  if (is.null(NumSingleton(singletonMethod_num))) {
    stop("wrong singletonMethod")
  }
  
    
    if(length(primary) &!is.null(whenEmptySuppressed)){
      if(min(colSums(abs(x[, primary, drop = FALSE]))) == 0){
        whenEmptySuppressed("Suppressed cells with empty input will not be protected. Extend input data with zeros?")
      }
    }
    
    secondary <- GaussSuppression1(x, candidates, primary, printInc, singleton = singleton, nForced = nForced, 
                                           singletonMethod = singletonMethod, singletonMethod_num = singletonMethod_num, singleton_num = singleton_num, tolGauss=tolGauss, 
                                           iFunction = iFunction, iWait = iWait,
                                   main_primary = primary, idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld,
                                   ncol_x_input = ncol_x_input, ncol_x_with_xExtraPrimary = ncol_x_with_xExtraPrimary,
                                   whenPrimaryForced = whenPrimaryForced, 
                                           ...)
    
    unsafePrimary <- c(unsafePrimary, -secondary[secondary < 0])
    secondary <- secondary[secondary > 0]
    
    if(length(secondary) & !is.null(whenEmptyUnsuppressed)){
      lateUnsuppressed <- candidates[SeqInc(1L + min(match(secondary, candidates)), length(candidates))]
      lateUnsuppressed <- lateUnsuppressed[!(lateUnsuppressed %in% secondary)]
      if(length(lateUnsuppressed)){
        if(min(colSums(abs(x[, lateUnsuppressed, drop = FALSE]))) == 0){
          whenEmptyUnsuppressed("Cells with empty input will never be secondary suppressed. Extend input data with zeros?")
        }
      }
    }
    
    if(unsafeAsNegative){
      secondary <- c(secondary, -unsafePrimary)
    }

    secondary <- SecondaryFinal(secondary = secondary, primary = primary, idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld)
    
    return(secondary)
  #}
  
  #stop("wrong singletonMethod")
}

# Function to handle removeDuplicated
SecondaryFinal <- function(secondary, primary, idxDD, idxDDunique, candidatesOld, primaryOld) {
  if (is.null(idxDD)) {
    return(secondary)
  }
  unsafePrimary <- -secondary[secondary < 0]
  secondary <- secondary[secondary > 0]
  
  ma <- match(idxDD[candidatesOld], c(idxDDunique[secondary], idxDDunique[primary]))
  secondary <- candidatesOld[!is.na(ma)]
  secondary <- secondary[!(secondary %in% primaryOld)]
  
  if (!length(unsafePrimary)) {
    return(secondary)
  }
  
  unsafePrimaryA <- unsafePrimary[unsafePrimary <= length(idxDDunique)]
  unsafePrimaryB <- unsafePrimary[unsafePrimary > length(idxDDunique)]
  
  ma <- match(idxDD[primaryOld], idxDDunique[unsafePrimaryA])
  unsafePrimaryA <- primaryOld[!is.na(ma)]
  unsafePrimaryB <- unsafePrimaryB - length(idxDDunique) + length(idxDD)
  unsafePrimary <- c(unsafePrimaryA, unsafePrimaryB)
  
  c(secondary, -unsafePrimary)
  
}



GaussSuppression1 <- function(x, candidates, primary, printInc, singleton, nForced, singletonMethod, singletonMethod_num, singleton_num, tolGauss, testMaxInt = 0, allNumeric = FALSE,
                              iFunction, iWait, 
                              main_primary, idxDD, idxDDunique, candidatesOld, primaryOld, # main_primary also since primary may be changed 
                              ncol_x_input, ncol_x_with_xExtraPrimary, whenPrimaryForced,
                              ...) {
  
  # Trick:  GaussSuppressionPrintInfo <- message
  PrintInfo <- get0("GaussSuppressionPrintInfo",ifnotfound = function(x) NULL)

  if (!is.numeric(iWait)) {
    iWait <- Inf
  } else {
    if (is.na(iWait)) iWait <- Inf
  }
  if (!is.function(iFunction)) iWait <- Inf
  use_iFunction <- iWait < Inf
  
  if (use_iFunction) {
    sys_time <- Sys.time()
  }
  
  unsafePrimary <- integer(0)
  
  # testMaxInt is parameter for testing 
  # The Integer overflow situation will be forced when testMaxInt is exceeded   
  DoTestMaxInt = testMaxInt > 0
  
  # allNumeric is parameter for testing 
  # All calculations use numeric algorithm when TRUE
  if(allNumeric){
    Matrix2listInt <- SSBtools::Matrix2list 
  }
  
  if (printInc) {
    singletonMethod_print <- c(singletonMethod, singletonMethod_num)
    singletonMethod_print <- c(singletonMethod_print[!(singletonMethod_print %in% c("none", "num"))])
    if (!length(singletonMethod_print)) {
      singletonMethod_print <- "none"
    }
    singletonMethod_print <- paste(singletonMethod_print, collapse = "_")
    cat(paste0("GaussSuppression_", singletonMethod_print))
    flush.console()
  }
  
  numSingleton <- NumSingleton(singletonMethod_num)
  if (numSingleton[["singleton2Primary"]] == "T") {
    singleton2Primary <- TRUE
    forceSingleton2Primary <- TRUE
  } else {
    singleton2Primary <- numSingleton[["singleton2Primary"]] == "t"
    forceSingleton2Primary <- FALSE
  }
  integerUnique <- as.logical(numSingleton[["integerUnique"]])
  if (is.na(integerUnique)) {  # When 't'
    integerUnique <- is.integer(singleton_num)
  }
  if (integerUnique & !is.integer(singleton_num)) {
    stop("singleton as integer needed")
  }
  if (!integerUnique & is.integer(singleton_num)) {
    singleton_num <- as.logical(singleton_num)
  }
  
  numSingleton_elimination_ <- numSingleton[["elimination"]]
  numRevealsMessage <- numSingleton_elimination_ == "f"
  allow_GAUSS_DUPLICATES <- numSingleton_elimination_ %in% LETTERS
  numSingleton_elimination_ <- toupper(numSingleton_elimination_)
  
  numSingletonElimination <- numSingleton_elimination_ != "F"
  if (numSingletonElimination) {
    numRevealsMessage <- get0("force_numRevealsMessage", ifnotfound = FALSE)
  }
  WhenProblematicSingletons <- NULL
  if (numSingleton_elimination_ == "M") WhenProblematicSingletons <- message
  if (numSingleton_elimination_ == "W") WhenProblematicSingletons <- warning
  if (numRevealsMessage) WhenProblematicSingletons <- message
  
  sub2Sum <- as.logical(numSingleton[["sum2"]])
  if (is.na(sub2Sum)) {  # When 'H'
    sub2Sum <- TRUE
    hierarchySearch <- TRUE
  } else {
    hierarchySearch <- FALSE
  }
  
  if (singletonMethod == "none") {
    singleton <- FALSE
  }
  if (singletonMethod_num %in% c("none", "num")) {
    singleton_num <- FALSE
  }
  
  forceForcedNotSingletonNum <- (nForced > 0) & any(singleton_num)
  forceForcedNotSingletonFreq <- (nForced > 0) & any(singleton)
  
  
  if (forceForcedNotSingletonNum | forceForcedNotSingletonFreq) {
    cS1 <- which(colSums(x) == 1)
    cS1 <- cS1[cS1 %in% candidates[seq_len(nForced)]]
    if (length(cS1)) {
      cS1rS <- rowSums(x[, cS1, drop = FALSE]) > 0
      if (forceForcedNotSingletonNum & any(singleton_num & cS1rS)) {
        if (!is.null(whenPrimaryForced)) {
          whenPrimaryForced("Singleton marking of forced cells ignored (num)")
        }
        singleton_num[cS1rS] <- FALSE  # this is ok when integer: -> 0L 
      }
      if (forceForcedNotSingletonFreq & any(singleton & cS1rS)) {
        if (!is.null(whenPrimaryForced)) {
          whenPrimaryForced("Singleton marking of forced cells ignored (freq)")
        }
        singleton[cS1rS] <- FALSE
      }
    }
  }
  
  
  if (singletonMethod == "anySumNOTprimary") {
    singletonMethod <- "anySum"
    singletonNOTprimary <- TRUE
  } else {
    if (any(singleton)) {
      colSums_x <- colSums(x)
      singletonZ <- (colSums(x[singleton, , drop = FALSE]) == 1 & colSums_x == 1)
      singletonNOTprimary <- (sum(singletonZ) > sum(singletonZ[primary]))
    } else {
      singletonNOTprimary <- FALSE
    }
    if (singletonNOTprimary) {
      if (singletonMethod != "anySum")
        stop('singletonMethod must be "anySumNOTprimary" when singletons not primary suppressed')
      warning('singletonMethod is changed to "anySumNOTprimary"')
    }
  }
  
  
  # In order to give information about unsafe cells, "anySum" is internally changed to "subSumAny" when there are forced cells.
  if (!singletonNOTprimary & singletonMethod == "anySum" & nForced > 0) {
    singletonMethod <- "subSumAny"
  }
  
  ##
  ##  START extending x based on singleton
  ##
  
  input_ncol_x <- ncol(x)
  relevant_ncol_x <- ncol(x)
  
  # make new primary suppressed subSum-cells
  if (sub2Sum | singleton2Primary | forceSingleton2Primary) {  
    if (any(singleton_num)) {
      singleton_num_logical <- as.logical(singleton_num)
      if (singleton2Primary) {   # Change from if(forceSingleton2Primary)  
        cS1 <- which(colSums(x) == 1)
        cS1 <- cS1[!(cS1 %in% primary)]
        if (length(cS1)) {
          cS1 <- cS1[colSums(x[singleton_num_logical, cS1, drop = FALSE]) == 1]
        }
        if (length(cS1)) {
          if (forceSingleton2Primary) {   # Now forceSingleton2Primary used instead of above
            primary <- c(primary, cS1)
            PrintInfo("forceSingleton2Primary is used")
          }
        } else {
          forceSingleton2Primary <- TRUE  # When known that forceSingleton2Primary=TRUE give same result as FALSE (useful later)
        }
      }
      if (singleton2Primary) {
        singletonNotInPublish <- singleton_num_logical
        singletonNotInPublish[rowSums(x[, primary[colSums(x[, primary, drop = FALSE]) == 1], drop = FALSE]) > 0] <- FALSE  # singletonNotInPublish[innerprimary] <- FALSE
        if (any(singletonNotInPublish)) {
          PrintInfo("singleton2Primary is used")
          pZ <- Matrix(0, length(singletonNotInPublish), sum(singletonNotInPublish))
          pZ[cbind(which(singletonNotInPublish), seq_len(sum(singletonNotInPublish)))] <- 1
          primary <- c(primary, NCOL(x) + seq_len(NCOL(pZ)))  # same code as below
          x <- cbind(x, pZ)                                   # ---- // -----
        }
      }
      relevant_ncol_x <- ncol(x)
      if (sub2Sum) {
        pZs <- x * singleton_num_logical
        pZ <- x * (rowSums(x[, primary[colSums(x[, primary, drop = FALSE]) == 1], drop = FALSE]) > 0)  #  x * innerprimary
        pZ[ , primary] <- 0  # Not relevant when already suppressed 
        if (integerUnique) {
          if (!is.integer(singleton_num)) {
            stop("singleton as integer needed, but something is wrong since this check has been done earlier")
          }
          relevant_unique_index <- -seq_len(nrow(x))  # negative is guaranteed different from singleton_num
          relevant_unique_index[singleton_num_logical] <- singleton_num[singleton_num_logical]
          colSums_pZ_g_1 <- colSums(pZ) > 1
          if (any(colSums_pZ_g_1)) { # with this, DummyApply problem when onlys zeros in pZ also avoided
            cols_g_2 <- DummyApply(pZ, relevant_unique_index, function(x) length(unique(x))) > 2
            colSums_pZ_requirement <- !cols_g_2 & colSums_pZ_g_1
          } else {
            colSums_pZ_requirement <- colSums_pZ_g_1
            cols_g_2 <- FALSE
          }
          # colSums(pZ) > 1 since primary already exists when colSums(pZ) == 1
          # =2 before "&" here similar to =2 in sub2Sum: 
          #      * two primary suppressed inner cells provided that at least one of them is singleton (colSums(pZs) > 0)
          #      * Difference is that same singleton counted as 1
          # =1 before "&" here is extra 
          #      * All primary suppressed inner cells in group are same singleton and counted as 1 
          #      * The sum of this group needs protection
          # =0 before "&" here 
          #      * will never happen when colSums(pZ) > 1)
          #
          freq_max_singleton <- max(table(singleton_num[singleton_num_logical]))
        } else {  # not integerUnique
          colSums_pZ_requirement <- colSums(pZ) == 2
          if (hierarchySearch) {
            cols_g_2 <- colSums(pZ) > 2
          }
          freq_max_singleton <- 1L
        }
        if (hierarchySearch) {
          if (any(cols_g_2)) {
            cols_g_2 <- which(cols_g_2)
            PrintInfo(paste("freq_max_singleton for FindDiffMatrix:", freq_max_singleton))
            diffMatrix <- FindDiffMatrix(x[, primary[colSums(x[, primary, drop = FALSE]) > 1], drop = FALSE], # primary with more than 1, =1 already treated  
                                         pZ[, cols_g_2, drop = FALSE],  # (x * innerprimary) with more than 2
                                         freq_max_singleton)
            colnames(diffMatrix) <- cols_g_2[as.integer(colnames(diffMatrix))]  # now colnames correspond to pZ columns
            # Is there any difference column that corresponds to a unique contributor? The code below tries to answer.
            if (ncol(diffMatrix)) {
              diffMatrix <- diffMatrix[, colSums(diffMatrix[!singleton_num_logical, , drop = FALSE]) == 0, drop = FALSE]
              diffMatrix <- diffMatrix[singleton_num_logical, , drop = FALSE]
              if (ncol(diffMatrix)) {
                colSums_diffMatrix_is1 <- colSums(diffMatrix) == 1
                if (any(colSums_diffMatrix_is1)) {
                  PrintInfo("hierarchySearch is used in the standard way")
                  colSums_pZ_requirement[as.integer(colnames(diffMatrix)[colSums_diffMatrix_is1])] <- TRUE
                  diffMatrix <- diffMatrix[, !colSums_diffMatrix_is1, drop = FALSE]
                }
                if (integerUnique & ncol(diffMatrix)) {
                  cols_eq_1 <- DummyApply(diffMatrix, relevant_unique_index[singleton_num_logical], function(x) length(unique(x))) == 1
                  if (any(cols_eq_1)) {
                    PrintInfo("hierarchySearch is used in combination with integerUnique")
                    colSums_pZ_requirement[as.integer(colnames(diffMatrix)[cols_eq_1])] <- TRUE
                  }
                }
              }
            }
          }
        }
        colZ <- ((colSums(pZs) > 0) & colSums_pZ_requirement)
      } else {
        colZ <- FALSE  # This is not logical, but due to code change
      }
      if (any(colZ)) {
        pZ <- pZ[, colZ, drop = FALSE]
        nodupl <- which(!DummyDuplicated(pZ, rnd = TRUE)) # nodupl <- which(!duplicated(as.matrix(t(pZ)))) 
        pZ <- pZ[, nodupl, drop = FALSE]
        primary <- c(primary, NCOL(x) + seq_len(NCOL(pZ)))
        x <- cbind(x, pZ)
      }
    }
  }
  
  if (!all(SeqInc(input_ncol_x + 1L, input_ncol_x) %in% primary)) {
    stop("extending x based on singleton failed")
  }
  
  # make new primary suppressed subSum-cells
  if (grepl("subSum", singletonMethod)) {
    if (any(singleton)) {
      pZ <- x * singleton
      colZ <- colSums(pZ) > 1
      if (any(colZ)) {                                     # Same code below  
        pZ <- pZ[, colZ, drop = FALSE]
        nodupl <- which(!DummyDuplicated(pZ, rnd = TRUE)) # which(!duplicated(as.matrix(t(pZ)))) 
        pZ <- pZ[, nodupl, drop = FALSE]
        primary <- c(primary, NCOL(x) + seq_len(NCOL(pZ)))
        x <- cbind(x, pZ)
      }
    }
    if (singletonMethod == "subSum") 
      singleton <- FALSE
  }
  
  keep_all_singleton_primary <- TRUE
  
  if (keep_all_singleton_primary) {
    ddx <- rep(FALSE, ncol(x))
    ddx[primary] <- DummyDuplicated(x[, primary, drop = FALSE], rnd = TRUE)
    ddx[seq_len(input_ncol_x)] <- FALSE
    if (any(ddx)) {
      x <- x[, !ddx]
      primary <- primary[seq_len(length(primary) - sum(ddx))]
      PrintInfo("duplicates found")
    }
  } else {
    ddx <- DummyDuplicated(x, rnd = TRUE)
    ddx[seq_len(input_ncol_x)] <- FALSE
    if (any(ddx)) {
      x <- x[, !ddx]
      primary <- primary[seq_len(length(primary) - sum(ddx))]
      PrintInfo("duplicates found")
    }
  }
  
  ##
  ##  END extending x based on singleton
  ##
  
  n_relevant_primary <- sum(primary <= relevant_ncol_x)
  
  
  if (!any(singleton)) 
    singleton <- NULL
  
  # Change to unique integers. Other uses of singleton_num are finished  
  if ((numSingletonElimination|numRevealsMessage) & is.logical(singleton_num)) {
    singleton_num[singleton_num] <- seq_len(sum(singleton_num))
  }
  
  force_GAUSS_DUPLICATES    <- get0("force_GAUSS_DUPLICATES", ifnotfound = FALSE)
  order_GAUSS_DUPLICATES    <- get0("order_GAUSS_DUPLICATES", ifnotfound = TRUE)
  
  if (numSingletonElimination|numRevealsMessage) {
    
    # singleton_num as rows, primary as columns
    sspp <- fac2sparse(singleton_num[singleton_num > 0]) %*% x[singleton_num > 0, primary[seq_len(n_relevant_primary)], drop = FALSE]
    
    # Indices of primary originated from unique singleton
    uniqueSingletonPrimary <- which(colSums(sign(sspp)) == 1)
    if (order_GAUSS_DUPLICATES) {
      order_singleton_num <- Order_singleton_num(singleton_num)
    } else {
      order_singleton_num <- order(singleton_num)
    }
    x <- x[order_singleton_num,  , drop = FALSE]
    singleton_num <- singleton_num[order_singleton_num]
    if (!is.null(singleton)) {
      singleton <- singleton[order_singleton_num]
    }
  }
  
  order_singleton_num  <- NULL
  
  if (!is.null(singleton)) {
    ordSingleton <- order(singleton)
    singleton <- singleton[ordSingleton]
    
    maTRUE <- match(TRUE, singleton)
    
    if (!is.na(maTRUE)) {
      ordyB <- ordSingleton[seq_len(maTRUE - 1)]
      maxInd <- maTRUE - 1
    } else {
      ordyB <- ordSingleton
      maxInd <- length(singleton)
    }
    
    # maxInd made for subSpace, maxInd2 needed by anySum
    maxInd2 <- maxInd
    
    # Removes cells that are handled by anySum/subSpace anyway
    # In order to give correct information about unsafe cells, do not remove when there are forced cells.
    if (!singletonNOTprimary & nForced == 0) {
      if (!grepl("subSum", singletonMethod)) {
        primary <- primary[colSums(x[ordyB, primary, drop = FALSE]) != 0]
      }
    }
    
    A <- Matrix2listInt(x[ordSingleton, candidates, drop = FALSE])
    if (grepl("Space", singletonMethod)) {
      B <- Matrix2listInt(x[ordyB, primary, drop = FALSE])
      order_singleton_num  <- ordyB
    } else {
      B <- Matrix2listInt(x[ordSingleton, primary, drop = FALSE])
      maxInd <- nrow(x)
      order_singleton_num  <- ordSingleton
    }
  } else {
    A <- Matrix2listInt(x[, candidates, drop = FALSE])
    B <- Matrix2listInt(x[, primary, drop = FALSE])
    maxInd <- nrow(x)
  }
  
  
  if (numSingletonElimination|numRevealsMessage) {
    
    #singleton-integer-value when primary originated from unique singleton
    primarySingletonNum <- rep(0, length(primary))
    for (i in uniqueSingletonPrimary) {
      primarySingletonNum[i] <- singleton_num[B$r[[i]][1]]
    }
    
    if (!is.null(order_singleton_num)) {
      singleton_num <- singleton_num[order_singleton_num]
    }
  }
  
  
  m <- nrow(x)
  n <- length(A$r)
  nB <- length(B$r)
  secondary <- rep(FALSE, n)
  
  if (printInc) {
    cat(": ")
    flush.console()
  }
  ii <- 1L
  nrA <- rep(NA_integer_, n)
  nrB <- rep(NA_integer_, nB)
  
  
  # To store cumulative factors from ReduceGreatestDivisor
  # Used to rescale when switching to numeric algorithm (caused by integer overflow).
  kk_2_factorsA <- rep(1, n)
  kk_2_factorsB <- rep(1, nB)
  
  
  subUsed <- rep(FALSE, m)  # needed by anySum
  
  dot <- "."
  # dot will change to "-" when integer overflow occur (then numeric algorithm)  
  dash <- c("-", "=")   # dot <- dash[N_GAUSS_DUPLICATES]
  # when  N_GAUSS_DUPLICATES==2  dot will change to ":" or "=" (integer overflow) 
  
  
  
  ###################################################################################################
  # START - define AnyProportionalGaussInt
  #         when !numSingletonElimination: 
  #                         old function outside this function is used (see below)
  #   Since function defined inside, it is possible to "cheat" and avoid extra input-parameters.
  #   Now  primarySingletonNum and numSingletonElimination avoided
  #
  #  This function reuses code from old branch “Feature/safety-range”. 
  #  Comments about rangeValues/rangeLimits are from this old code. 
  #  It is possible to further develop this within this new function.
  #####################################################################################################
  
  
  Check_s_unique <- function(s_unique, i) {
    if (length(s_unique) > 1) {
      return(FALSE)
    }
    if (length(s_unique) == 0) {
      return(TRUE)
    }
    if (s_unique == 0) {
      return(FALSE)
    }
    if (s_unique == primarySingletonNum[i]) {
      return(FALSE)
    }
    TRUE
  }
   
  AnyProportionalGaussInt_NEW <- function(r, x, rB, xB, tolGauss, kk_2_factorsB, singleton_num = NULL) {
    n <- length(r)
    if (!n) {
      return(TRUE)  # Empty 'A-input' regarded as proportional
    }
    for (i in seq_along(rB)) {
      numSingletonEliminationCheck <- numSingletonElimination
      if(i > n_relevant_primary){
        numSingletonEliminationCheck <- FALSE
      }
      ni <- length(xB[[i]])
      if (ni) { # Empty 'B-input' not regarded as proportional
        doCheck <- FALSE
        if (ni == n) {
          if (identical(r, rB[[i]])) {        # Same as in old function 
            doCheck <- TRUE
            x_here <- x
            xBi_here <- xB[[i]]
            if (numSingletonEliminationCheck) {
              #restLimit <- rangeLimits[i]     # This is new
              s_unique <- integer(0)
              r_in_rB <- rep(TRUE, length(r))
              rB_in_r <- r_in_rB 
            } else {
              #restLimit <- 0
            }
          }
        }
        if (!doCheck) {
          if (numSingletonEliminationCheck) {
            if (r[1] %in% rB[[i]]) {         # No gauss elimination if r[1] not in rB[[i]]
              r_in_rB <- r %in% rB[[i]]
              rB_in_r <- rB[[i]] %in% r
              rdiff <- c(r[!r_in_rB], rB[[i]][!rB_in_r])  # elements not common 
              # sum_rdiff <- sum(rangeValues[rdiff])
              s_unique <- unique(singleton_num[rdiff])
              x_here <- x[r_in_rB]                        # x reduced to common elements 
              xBi_here <- xB[[i]][rB_in_r]                # xB[[i]] reduced to common elements
              #restLimit <- rangeLimits[i] - sum_rdiff     
              #doCheck <- restLimit >= 0   # New when non-NULL rangeLimits
              #doCheck <- (length(s_unique) <= 1) & (min(s_unique) > 0)
              doCheck <- Check_s_unique(s_unique, i)
            }
          }
        }
        if (doCheck) {
          if (n == 1L)
            return(TRUE)
          if (identical(x_here, xBi_here))
            return(TRUE)
          if (identical(-x_here, xBi_here))
            return(TRUE)
          
          cx1xBi1 <- c(x_here[1], xBi_here[1])
          if (is.integer(cx1xBi1)) {
            kk <- ReduceGreatestDivisor(cx1xBi1)
            suppressWarnings({
              kk_2_x <- kk[2] * x_here
              kk_1_xB_i <- kk[1] * xBi_here
            })
            if (anyNA(kk_2_x) | anyNA(kk_1_xB_i)) {
              kk <- as.numeric(kk)
              kk_2_x <- kk[2] * x_here
              kk_1_xB_i <- kk[1] * xBi_here
              
            }
            if (identical(kk_2_x, kk_1_xB_i))
              return(TRUE)
            if (is.numeric(kk)) {
              if (all(abs(xBi_here - kk_2_x/kk[1]) < tolGauss))
                return(TRUE)
            }
            if (numSingletonEliminationCheck) { #if (restLimit) {  # Same logical vectors again when TRUE not returned and when rangeLimits used (simplification possible)
              if (!is.numeric(kk)) {  
                rrest <- (r[r_in_rB])[kk_2_x != kk_1_xB_i]
              } else {
                rrest <- (r[r_in_rB])[!(abs(xBi_here - kk_2_x/kk[1]) < tolGauss)]
              }
              s_unique <- unique(c(s_unique, singleton_num[rrest]))
              if (Check_s_unique(s_unique, i)) { #if ((length(s_unique) <= 1) & (min(s_unique) > 0)) {
                return(TRUE) # New possible TRUE-return caused by rangeLimits
              }
            }
            
          } else {
            #  Possible code here to look at distribution of numeric computing errors  
            #  aabb <- abs((xB[[i]] - (cx1xBi1[2]/cx1xBi1[1]) * x)/kk_2_factorsB[i])
            #  aabb <- aabb[aabb > 0 & aabb < 1e-04]
            if (all(abs(xBi_here - (cx1xBi1[2]/cx1xBi1[1]) * x) < tolGauss * abs(kk_2_factorsB[i])))
              return(TRUE)
            if (numSingletonEliminationCheck) {# if (restLimit) {
              rrest <- (r[r_in_rB])[!(abs(xBi_here - (cx1xBi1[2]/cx1xBi1[1]) * x) < tolGauss * abs(kk_2_factorsB[i]))]
              s_unique <- unique(c(s_unique, singleton_num[rrest]))
              # if (sum(rangeValues[rrest]) < restLimit) {
              if (Check_s_unique(s_unique, i)) { #if ((length(s_unique) <= 1) & (min(s_unique) > 0)) {
                return(TRUE) # New possible TRUE-return caused by rangeLimits (as above)
              }
            }
          }
        }
      }
    }
    FALSE
  }
  
  
  if (force_GAUSS_DUPLICATES) {
    if (!numSingletonElimination) {
      singleton_num <- rep(0L, m)
      numSingletonElimination <- TRUE
    }
  }
  
  if (numSingletonElimination) {
    #AnyProportionalGaussInt <- AnyProportionalGaussInt_NEW
    AnyProportionalGaussInt <- function(...){
      anyP <- AnyProportionalGaussInt_NEW(A$r[[j]], A$x[[j]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB, singleton_num = singleton_num) 
      if (anyP) return(TRUE)
      if (singleton_num[A$r[[j]]][1] & length(A$r[[j]]) > 1) {   # More may be seen since A$r[[j]]][1] used in AnyProportionalGaussInt_NEW (elimination)
        r <- c(SeqInc(2, length(A$r[[j]])), 1L)                  # length(A$r[[j]]) > 1  should be unnecessary
        anyP <- AnyProportionalGaussInt_NEW(A$r[[j]][r], A$x[[j]][r], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB, singleton_num = singleton_num)
      }
      if (anyP) return(TRUE)
      if (N_GAUSS_DUPLICATES == 1) {
        return(anyP)
      }
      anyP <- AnyProportionalGaussInt_NEW(A_DUPLICATE$r[[j]], A_DUPLICATE$x[[j]], B_DUPLICATE$r, B_DUPLICATE$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB_DUPLICATE, singleton_num = singleton_num_DUPLICATE)
      if (anyP) return(TRUE)
      if (singleton_num[A_DUPLICATE$r[[j]]][1] & length(A_DUPLICATE$r[[j]]) > 1) {
        r <- c(SeqInc(2, length(A_DUPLICATE$r[[j]])), 1L)
        anyP <- AnyProportionalGaussInt_NEW(A_DUPLICATE$r[[j]][r], A_DUPLICATE$x[[j]][r], B_DUPLICATE$r, B_DUPLICATE$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB_DUPLICATE, singleton_num = singleton_num_DUPLICATE)
      }
      anyP
    }
    
  } else {
    if (get0("testAnyProportionalGaussInt", ifnotfound = FALSE)) {
      AnyProportionalGaussInt <- function(...) {
        apgiOLD <- AnyProportionalGaussInt_OLD(...)
        apgiNEW <- AnyProportionalGaussInt_NEW(...)
        if (apgiOLD != apgiNEW) {
          stop("AnyProportionalGaussInt NEW/OLD problem")
        } 
        apgiOLD
      }
    } else {
      AnyProportionalGaussInt <- AnyProportionalGaussInt_OLD
    }
  }
  
  #####################################################################
  # END - define AnyProportionalGaussInt
  #####################################################################
  
  eliminatedRows <- rep(FALSE, m)
  
  MessageProblematicSingletons <- function() {   # internal function since used twice below
    if (!is.null(WhenProblematicSingletons) & (numSingletonElimination|numRevealsMessage)) {
      if (!numRevealsMessage) {
        rowsP <- which(eliminatedRows & as.logical(singleton_num))
        singleP <- singleton_num[rowsP]
        if (N_GAUSS_DUPLICATES == 2) {
          rows2 <- DUPLICATE_order_singleton_num[which(eliminatedRows_DUPLICATE & as.logical(singleton_num_DUPLICATE))]
          rowsP <- rowsP[rowsP %in% rows2]
          singleP <- singleP[singleP %in% singleton_num[rows2]]
        }
        n_unique <- length(unique(singleton_num[as.logical(singleton_num)]))
        singleP <- unique(singleP)
        if (!forceSingleton2Primary) {
          if (length(singleP)) {
            WhenProblematicSingletons(paste(sum(length(singleP)), "out of", n_unique, "unique singletons problematic. Whether reveals exist is not calculated."))
          } 
        }
      } else {
        if (!forceSingleton2Primary) {
          message('Actual reveals cannot be calculated. See ?NumSingleton. Try T as "1st character"?')
        } else {
          singleP <- unique(singleton_num[as.logical(singleton_num)])
          n_unique <- length(singleP)
        }
      }
      if (forceSingleton2Primary) {
        if (length(singleP)) {
          eliminatedBySingleton <- rep(FALSE, length(singleP))
          for (i in seq_len(n_relevant_primary)) {
            if (!length(B$r[[i]])) {     # Avoid special situation
              primarySingletonNum[i] <- 0
            }
          }
          B$r <- B$r[seq_len(n_relevant_primary)]
          B$x <- B$x[seq_len(n_relevant_primary)]
          primarySingletonNum <- primarySingletonNum[seq_len(n_relevant_primary)]
          kk_2_factorsB <- kk_2_factorsB[seq_len(n_relevant_primary)]
          for (i in seq_along(singleP)) {
            p <- primarySingletonNum == singleP[i]
            eliminatedBySingleton[i] <- AnyEliminatedBySingleton(list(r = B$r[p], x = B$x[p]), 
                                                                 list(r = B$r[!p], x = B$x[!p]), 
                                                                 kk_2_factorsB[p], kk_2_factorsB[!p], 
                                                                 singleton = singleton,
                                                                 DoTestMaxInt = DoTestMaxInt, tolGauss = tolGauss,
                                                                 N_GAUSS_DUPLICATES = N_GAUSS_DUPLICATES, dash = dash,
                                                                 maxInd = maxInd, testMaxInt = testMaxInt)
          }
          if (sum(eliminatedBySingleton)) { 
            WhenProblematicSingletons(paste(sum(eliminatedBySingleton), "out of", n_unique, "unique singletons can reveal primary cells."))
          }
        }
      }  
    }
    NULL
  }
  
  N_GAUSS_DUPLICATES <- 1
  
  # The main Gaussian elimination loop 
  # Code made for speed, not readability
  for (j in seq_len(n)) {
    if (printInc) 
      if (j%%max(1, n%/%25) == 0) {
        cat(dot)
        flush.console()
      }
    
    if (nForced > 0 & j == 1) {
      is0Br <- sapply(B$r, length) == 0
    }
    if (nForced > 0 & ((j == (nForced + 1)) |((ii > m) & (j <= nForced)))) {
      is0Br_ <- sapply(B$r, length) == 0
      if (any(is0Br != is0Br_)) {
        unsafePrimary <- c(unsafePrimary, primary[is0Br != is0Br_]) # c(... since maybe future extension 
        
        unsafePrimaryAsFinal <- -SecondaryFinal(secondary = -unsafePrimary, primary = integer(0), idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld)
        
        unsafeOrinary <- unsafePrimaryAsFinal[unsafePrimaryAsFinal <= ncol_x_input]
        unsafeExtra <- unsafePrimaryAsFinal[unsafePrimaryAsFinal > ncol_x_input  & unsafePrimaryAsFinal <= ncol_x_with_xExtraPrimary]
        unsafeSingleton <- unsafePrimaryAsFinal[unsafePrimaryAsFinal > ncol_x_with_xExtraPrimary]
        
        if (length(unsafeExtra)+length(unsafeSingleton)) {
          s <- paste0(length(unsafePrimaryAsFinal), " (", length(unsafeOrinary), " ordinary, ", length(unsafeExtra), " extra, ", length(unsafeSingleton), " singleton)")
        } else {
          s <- length(unsafeOrinary)
        }
        warning(paste(s, "unsafe primary cells due to forced cells"))  #  Forced cells -> All primary cells are not safe
      }
    }
    if (ii > m){ 
      if (printInc) {
        cat("\n")
        flush.console()
      }
      MessageProblematicSingletons()
      return(c(candidates[secondary], -unsafePrimary))
    }
    
    if (length(A$r[[j]])) {
if(numSingletonElimination)
  if((allow_GAUSS_DUPLICATES & singleton_num[A$r[[j]][1]]) | force_GAUSS_DUPLICATES)
    if(N_GAUSS_DUPLICATES==1){
      A_DUPLICATE <- A
      B_DUPLICATE <- B
      eliminatedRows_DUPLICATE <- eliminatedRows
      kk_2_factorsA_DUPLICATE <- kk_2_factorsA
      kk_2_factorsB_DUPLICATE <- kk_2_factorsB
      eliminatedRows_DUPLICATE <- eliminatedRows
      
      DUPLICATE_order_singleton_num <- seq_len(m)
      singleton_logical <- as.logical(singleton_num)
      above_maxInd <- rep(FALSE, m)
      above_maxInd[SeqInc(maxInd + 1, m)] <- TRUE
      DUPLICATE_order_singleton_num[singleton_logical & above_maxInd]  <- rev(DUPLICATE_order_singleton_num[singleton_logical & above_maxInd])
      DUPLICATE_order_singleton_num[singleton_logical & !above_maxInd] <- rev(DUPLICATE_order_singleton_num[singleton_logical & !above_maxInd])
      if (force_GAUSS_DUPLICATES) {   # reverse other cells as well 
        DUPLICATE_order_singleton_num[!singleton_logical & above_maxInd]  <- rev(DUPLICATE_order_singleton_num[!singleton_logical & above_maxInd])
        DUPLICATE_order_singleton_num[!singleton_logical & !above_maxInd] <- rev(DUPLICATE_order_singleton_num[!singleton_logical & !above_maxInd])
      }
      singleton_num_DUPLICATE <- singleton_num[DUPLICATE_order_singleton_num] 
      
      A_DUPLICATE <- A
      for(i in SeqInc(j, n)){
        if(any( singleton_logical[A$r[[i]]]) | force_GAUSS_DUPLICATES){
          A_DUPLICATE$r[[i]] <- DUPLICATE_order_singleton_num[A$r[[i]]]
          r <- order(A_DUPLICATE$r[[i]])
          A_DUPLICATE$r[[i]] <- A_DUPLICATE$r[[i]][r]
          A_DUPLICATE$x[[i]] <- A_DUPLICATE$x[[i]][r]
        }
      }
      B_DUPLICATE <- B
      for(i in seq_len(nB)){
        if(any( singleton_logical[B$r[[i]]]) | force_GAUSS_DUPLICATES){
          B_DUPLICATE$r[[i]] <- DUPLICATE_order_singleton_num[B$r[[i]]]
          r <- order(B_DUPLICATE$r[[i]])
          B_DUPLICATE$r[[i]] <- B_DUPLICATE$r[[i]][r]
          B_DUPLICATE$x[[i]] <- B_DUPLICATE$x[[i]][r]
        }
      }
      
      N_GAUSS_DUPLICATES <- 2
      if (dot == ".") {
        dot <- ":"
      } else {
        dot <- dash[N_GAUSS_DUPLICATES]
      }
    }      
      
      reduced <- FALSE
      if (j > nForced) {
        if (is.null(singleton)) {
          isSecondary <- AnyProportionalGaussInt(A$r[[j]], A$x[[j]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB)
        } else {
          subSubSec <- A$r[[j]][1] > maxInd2
          if (grepl("Space", singletonMethod)) {
            okArj <- A$r[[j]] <= maxInd
            isSecondary <- subSubSec | (AnyProportionalGaussInt(A$r[[j]][okArj], A$x[[j]][okArj], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB))
          } else {
            if (subSubSec) {
              if (length(unique(A$x[[j]])) > 1) {  # Not proportional to original sum, 
                if (!any(subUsed[A$r[[j]]])) {     # but can’r be sure after gaussian elimination of another “Not proportional to sum”.
                  subSubSec <- FALSE               # To be sure, non-overlapping restriction introduced (subUsed) 
                  subUsed[A$r[[j]]] <- TRUE
                } else {
                  # if(printInc) # "Can't-be-sure-suppression" if "AnyProportionalGaussInt(.." is FALSE
                  #   cat('@')   # More advanced method may improve
                }
              }
            }
            if (subSubSec & singletonNOTprimary) {
              if (!Any0GaussInt(A$r[[j]], B$r)) {
for (I_GAUSS_DUPLICATES in 1:N_GAUSS_DUPLICATES){        
  if(I_GAUSS_DUPLICATES == 2){
    A_TEMP <- A
    B_TEMP <- B
    eliminatedRows_TEMP <- eliminatedRows
    singleton_num_TEMP <- singleton_num
    
    A <- A_DUPLICATE
    B <- B_DUPLICATE
    eliminatedRows <- eliminatedRows_DUPLICATE
    singleton_num <- singleton_num_DUPLICATE
  }
                subSubSec <- FALSE
                for (i in SeqInc(j + 1L, n)) {
                  j_in_i <- A$r[[i]] %in% A$r[[j]]
                  if (all(j_in_i)) {
                    A$r[[i]] <- integer(0)
                    A$x[[i]] <- integer(0)
                  } else {
                  if (any(j_in_i)) {
                    A$r[[i]] <- A$r[[i]][!j_in_i]
                    A$x[[i]] <- A$x[[i]][!j_in_i]
                  }
                  }
                }
                for (i in seq_len(nB)) {
                  j_in_i <- B$r[[i]] %in% A$r[[j]]
                  if (any(j_in_i)) {
                    B$r[[i]] <- B$r[[i]][!j_in_i]
                    B$x[[i]] <- B$x[[i]][!j_in_i]
                  }
                }
                A$r[[j]] <- integer(0)
                A$x[[j]] <- integer(0)  
                isSecondary <- FALSE
                eliminatedRows[A$r[[j]]] <- TRUE
  if(I_GAUSS_DUPLICATES == 2){
    A_DUPLICATE <- A 
    B_DUPLICATE <- B 
    eliminatedRows_DUPLICATE <- eliminatedRows
    singleton_num_DUPLICATE <- singleton_num
    
    A <- A_TEMP
    B <- B_TEMP
    eliminatedRows <- eliminatedRows_TEMP
    singleton_num <- singleton_num_TEMP
  }
} # end   for (I_GAUSS_DUPLICATES in 1:N_GAUSS_DUPLICATES){         
                reduced <- TRUE
              } else {
                isSecondary <- TRUE
              }
              
            } else {
              isSecondary <- subSubSec | (AnyProportionalGaussInt(A$r[[j]], A$x[[j]], B$r, B$x, tolGauss = tolGauss, kk_2_factorsB = kk_2_factorsB))
            }
          }
        }
      } else {
        isSecondary <- FALSE
      }
      if (!isSecondary) {
       if (!reduced) { 
        ind <- A$r[[j]][1]
        

#eliminatedRows[ind] <- TRUE        
for (I_GAUSS_DUPLICATES in 1:N_GAUSS_DUPLICATES){
  if(I_GAUSS_DUPLICATES == 2){
    A_TEMP <- A
    B_TEMP <- B
    eliminatedRows_TEMP <- eliminatedRows
    singleton_num_TEMP <- singleton_num
    kk_2_factorsA_TEMP <- kk_2_factorsA
    kk_2_factorsB_TEMP <- kk_2_factorsB
    
    A <- A_DUPLICATE
    B <- B_DUPLICATE
    eliminatedRows <- eliminatedRows_DUPLICATE
    singleton_num <- singleton_num_DUPLICATE
    kk_2_factorsA <- kk_2_factorsA_DUPLICATE
    kk_2_factorsB <- kk_2_factorsB_DUPLICATE
    
    ind <- A$r[[j]][1]
    #eliminatedRows[ind] <- TRUE
  }
 
  eliminatedRows[ind] <- TRUE
  
  
        nrA[] <- NA_integer_
        nrB[] <- NA_integer_
        for (i in SeqInc(j + 1L, n)) 
          nrA[i] <- match(ind, A$r[[i]])
        for (i in seq_len(nB)) 
          nrB[i] <- match(ind, B$r[[i]])
        
        Arj <- A$r[[j]][-1L]
        Axj <- A$x[[j]][-1L]
        Axj1 <- A$x[[j]][1L]
        A$r[[j]] <- integer(0) # NA_integer_
        A$x[[j]] <- integer(0) # NA_integer_
        
        if (length(Arj) == 0L) {
          for (i in which(!is.na(nrA))) {
            if(length(A$r[[i]]) == 1L){
              A$r[[i]] <- integer(0)
              A$x[[i]] <- integer(0)
            } else {
              A$r[[i]] <- A$r[[i]][-nrA[i]]
              A$x[[i]] <- A$x[[i]][-nrA[i]]
              if (Scale2one(A$x[[i]])) {
                A$x[[i]][] <- 1L
                kk_2_factorsA[i] <- 1
              }
            }
          }
        } else {
          for (i in which(!is.na(nrA))) {
            if (length(A$x[[i]]) == 1L) {
              A$r[[i]] <- Arj
              A$x[[i]] <- Axj
              kk_2_factorsA[i] <- kk_2_factorsA[j] # Factors are inherited when all values are inherited
            } else {
              ai <- Arj
              bi <- A$r[[i]][-nrA[i]]
              ma <- match(ai, bi)
              isnama <- is.na(ma)
              ma_isnama <- ma[!isnama]
              di <- c(bi, ai[isnama])
              if (abs(A$x[[i]][nrA[i]]) == abs(Axj1)) {
                suppressWarnings({
                  if (A$x[[i]][nrA[i]] == Axj1) {
                    dx <- c(A$x[[i]][-nrA[i]], -Axj[isnama])
                    dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
                  } else {
                    dx <- c(A$x[[i]][-nrA[i]], Axj[isnama])
                    dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
                  }
                  if (DoTestMaxInt) {
                    if (!anyNA(dx)) {
                      if (max(dx) > testMaxInt) {
                        dx[1] <- NA
                        warning("testMaxInt exceeded")
                      }
                    }
                  }
                })
                
                if (anyNA(dx)) 
                {
                  dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
                  if (A$x[[i]][nrA[i]] == Axj1) {
                    dx <- as.numeric(c(A$x[[i]][-nrA[i]], -Axj[isnama]))
                    dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
                  } else {
                    dx <- as.numeric(c(A$x[[i]][-nrA[i]], Axj[isnama]))
                    dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
                  }
                  dx <- dx/kk_2_factorsA[i]    # rescale needed since change to numeric
                  kk_2_factorsA[i] <- 1
                } else {
                  if(!is.integer(dx)){
                    if(is.integer(A$x[[i]])){  # Change to numeric caused by Axj, rescale needed here also
                      dx <- dx/kk_2_factorsA[i]
                      kk_2_factorsA[i] <- 1
                    }
                  }
                }
              } else {
                kk <- ReduceGreatestDivisor(c(A$x[[i]][nrA[i]], Axj1))
                if(is.integer(kk)){
                  kk_2_factorsA[i] <- kk[2] * kk_2_factorsA[i]
                }
                suppressWarnings({
                  dx <- c(kk[2] * A$x[[i]][-nrA[i]], -kk[1] * Axj[isnama])
                  dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
                  if (DoTestMaxInt) {
                    if (!anyNA(dx)) {
                      if (max(dx) > testMaxInt) {
                        dx[1] <- NA
                        warning("testMaxInt exceeded")
                      }
                    }
                  }
                })
                if (anyNA(dx)) 
                {
                  dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
                  kk <- as.numeric(kk)
                  dx <- c(kk[2] * A$x[[i]][-nrA[i]], -kk[1] * Axj[isnama])
                  dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
                  dx <- dx/kk_2_factorsA[i]   # rescale needed since change to numeric
                  kk_2_factorsA[i] <- 1
                } else {
                  if(!is.integer(dx)){
                    if(is.integer(A$x[[i]])){      # Change to numeric caused by Axj, rescale needed here also
                      dx <- dx/kk_2_factorsA[i]
                      kk_2_factorsA[i] <- 1
                    }
                  }
                }
              }
              if(is.integer(dx)){
                rows <- (dx != 0L)
              } else {
                rows <- (abs(dx) >= tolGauss)
              }
              di <- di[rows]
              dx <- dx[rows]
              r <- order(di)
              A$r[[i]] <- di[r]
              A$x[[i]] <- dx[r]
              if (Scale2one(A$x[[i]])) {
                A$x[[i]][] <- 1L
                kk_2_factorsA[i] <- 1
              }
            }
          }
        }
        if (!is.null(singleton)) {
          okInd <- (Arj <= maxInd)
          Arj <- Arj[okInd]
          Axj <- Axj[okInd]
        }
        if (length(Arj) == 0L) {
          for (i in which(!is.na(nrB))) {
            B$r[[i]] <- B$r[[i]][-nrB[i]]
            B$x[[i]] <- B$x[[i]][-nrB[i]]
            if (Scale2one(B$x[[i]])) {
              B$x[[i]][] <- 1L
              kk_2_factorsB[i] <- 1
            }
          }
        } else {
          for (i in which(!is.na(nrB))) {
            if (length(B$x[[i]]) == 1L) {
              B$r[[i]] <- Arj
              B$x[[i]] <- Axj
              kk_2_factorsB[i] <- kk_2_factorsA[j] # Factors are inherited when all values are inherited
            } else {
              ai <- Arj
              bi <- B$r[[i]][-nrB[i]]
              ma <- match(ai, bi)
              isnama <- is.na(ma)
              ma_isnama <- ma[!isnama]
              di <- c(bi, ai[isnama])
              if (abs(B$x[[i]][nrB[i]]) == abs(Axj1)) {
                suppressWarnings({
                  if (B$x[[i]][nrB[i]] == Axj1) {
                    dx <- c(B$x[[i]][-nrB[i]], -Axj[isnama])
                    dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
                  } else {
                    dx <- c(B$x[[i]][-nrB[i]], Axj[isnama])
                    dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
                  }
                  if (DoTestMaxInt) {
                    if (!anyNA(dx)) {
                      if (max(dx) > testMaxInt) {
                        dx[1] <- NA
                        warning("testMaxInt exceeded")
                      }
                    }
                  }
                })
                if (anyNA(dx)) 
                {
                  dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
                  if (B$x[[i]][nrB[i]] == Axj1) {
                    dx <- as.numeric(c(B$x[[i]][-nrB[i]], -Axj[isnama]))
                    dx[ma_isnama] <- dx[ma_isnama] - Axj[!isnama]
                  } else {
                    dx <- as.numeric(c(B$x[[i]][-nrB[i]], Axj[isnama]))
                    dx[ma_isnama] <- dx[ma_isnama] + Axj[!isnama]
                  }
                  dx <- dx/kk_2_factorsB[i]
                  kk_2_factorsB[i] <- 1
                }
                else {
                  if(!is.integer(dx)){
                    if(is.integer(B$x[[i]])){
                      dx <- dx/kk_2_factorsB[i]
                      kk_2_factorsB[i] <- 1
                    }
                  }
                }
              } else {
                kk <- ReduceGreatestDivisor(c(B$x[[i]][nrB[i]], Axj1))
                if(is.integer(kk)){
                  kk_2_factorsB[i] <- kk[2] * kk_2_factorsB[i]
                }
                suppressWarnings({
                  dx <- c(kk[2] * B$x[[i]][-nrB[i]], -kk[1] * Axj[isnama])
                  dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
                  if (DoTestMaxInt) {
                    if (!anyNA(dx)) {
                      if (max(dx) > testMaxInt) {
                        dx[1] <- NA
                        warning("testMaxInt exceeded")
                      }
                    }
                  }
                })
                if (anyNA(dx)) 
                {
                  dot <- dash[N_GAUSS_DUPLICATES] # dot <- "-"
                  kk <- as.numeric(kk)
                  dx <- c(kk[2] * B$x[[i]][-nrB[i]], -kk[1] * Axj[isnama])
                  dx[ma_isnama] <- dx[ma_isnama] - kk[1] * Axj[!isnama]
                  dx <- dx/kk_2_factorsB[i]
                  kk_2_factorsB[i] <- 1
                } else {
                  if(!is.integer(dx)){
                    if(is.integer(B$x[[i]])){
                      dx <- dx/kk_2_factorsB[i]
                      kk_2_factorsB[i] <- 1
                    }
                  }
                }
              }
              if(is.integer(dx)){
                rows <- (dx != 0L)
              } else {
                rows <- (abs(dx) >= tolGauss)
              }
              if(!length(rows)){
                stop("Suppression method failed")
              }
              di <- di[rows]
              dx <- dx[rows]
              r <- order(di)
              B$r[[i]] <- di[r]
              B$x[[i]] <- dx[r]
              if (Scale2one(B$x[[i]])) {
                B$x[[i]][] <- 1L
                kk_2_factorsB[i] <- 1
              }
            }
          }
        }
 
 
if(I_GAUSS_DUPLICATES == 2){
    A_DUPLICATE <- A 
    B_DUPLICATE <- B 
    eliminatedRows_DUPLICATE <- eliminatedRows
    singleton_num_DUPLICATE <- singleton_num
    kk_2_factorsA_DUPLICATE <- kk_2_factorsA
    kk_2_factorsB_DUPLICATE <- kk_2_factorsB
    
    A <- A_TEMP
    B <- B_TEMP
    eliminatedRows <- eliminatedRows_TEMP
    singleton_num <- singleton_num_TEMP
    kk_2_factorsA <- kk_2_factorsA_TEMP
    kk_2_factorsB <- kk_2_factorsB_TEMP
  }
} # end   for (I_GAUSS_DUPLICATES in 1:N_GAUSS_DUPLICATES){           
       }  
        ii <- ii + 1L
      } else {
        A$r[[j]] <- integer(0)
        A$x[[j]] <- integer(0)
        secondary[j] <- TRUE
      }
    }
    if (use_iFunction) {
      sys_time2 <- Sys.time()
      if (ii-1L == m) {
        j_ <- n
      } else {
        j_ <- j
      }
      if (j_ == n) {
        iWait <- 0
      }
      if (as.numeric(difftime(sys_time2, sys_time), units = "secs") >= iWait){
        sys_time <- sys_time2
        false_ <- !secondary
        
        allEmptyDecided <- TRUE 
        if(allEmptyDecided){
          false_[SeqInc(j_+1,n)] <- (lengths(A$r) == 0)[SeqInc(j_+1,n)]
          na_ <- !(secondary | false_)  
        } else { # old code 
          false_[SeqInc(j_+1,n)] <- FALSE
          na_    <- !secondary
          na_[SeqInc(1,j_)] <- FALSE
        }
        
        iFunction(i = j_, I = n, j = ii-1L, J = m,
                  true =  SecondaryFinal(secondary = candidates[secondary], primary = main_primary, idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = primaryOld),
                  false = SecondaryFinal(secondary = candidates[false_],    primary = integer(0),   idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = integer(0)),
                  na =    SecondaryFinal(secondary = candidates[na_],       primary = integer(0),   idxDD = idxDD, idxDDunique = idxDDunique, candidatesOld = candidatesOld, primaryOld = integer(0)),
                  ...)
      }
    }
  }
  
  # cat("\n")
  # print(table(kk_2_factorsA))
  # print(table(kk_2_factorsB))
  # print(table(sapply(A$x,class)))
  # print(table(sapply(B$x,class)))
  
  if (printInc) {
    cat("\n")
    flush.console()
  }
  MessageProblematicSingletons()
  c(candidates[secondary], -unsafePrimary)
}



# Simplified version of AnyProportionalGaussInt 
Any0GaussInt <- function(r, rB) {
  for (i in seq_along(rB)) {
    ni <- length(rB[[i]])
    if (ni) {    
      if( all(rB[[i]] %in% r) )
        return(TRUE)
    }
  }
  FALSE
}




AnyProportionalGaussInt_OLD <- function(r, x, rB, xB, tolGauss,  kk_2_factorsB) {
  n <- length(r)
  if(!n){
    return(TRUE) # Empty "A-input" regarded as proportional
  }
  for (i in seq_along(rB)) {
    ni <- length(xB[[i]])
    if (ni) {    # Empty "B-input" not regarded as proportional
      if (ni == n) {
        if (identical(r, rB[[i]])) {
          if (n==1L)
            return(TRUE)
          if (identical(x, xB[[i]])) 
            return(TRUE)
          if (identical(-x, xB[[i]])) 
            return(TRUE)
          
          cx1xBi1 <- c(x[1], xB[[i]][1])
          if(is.integer(cx1xBi1)){
            kk <- ReduceGreatestDivisor(cx1xBi1)
            suppressWarnings({
              kk_2_x <- kk[2] * x 
              kk_1_xB_i <- kk[1] * xB[[i]]
            })
            if(anyNA(kk_2_x) | anyNA(kk_1_xB_i)){
              kk <- as.numeric(kk)
              kk_2_x <- kk[2] * x 
              kk_1_xB_i <- kk[1] * xB[[i]]
              
            }   
            if (identical(kk_2_x, kk_1_xB_i)) 
              return(TRUE)
            if(is.numeric(kk)){
              if( all(abs( xB[[i]] - kk_2_x/kk[1]) < tolGauss))
                return(TRUE)
            }
          }
          else {
            #if (FALSE) {
            #
            #  Possible code here to look at distribution of numeric computing errors  
            #
            #  aabb <- abs((xB[[i]] - (cx1xBi1[2]/cx1xBi1[1]) * x)/kk_2_factorsB[i])
            #  aabb <- aabb[aabb > 0 & aabb < 1e-04]
            #}
            if( all(abs(  xB[[i]] - (cx1xBi1[2]/cx1xBi1[1])* x) < tolGauss*abs(kk_2_factorsB[i]) )  )
              return(TRUE)
          }
        }
      }
    }
  }
  FALSE
}



# Reduce by Greatest common divisor (when integer input)
ReduceGreatestDivisor <- function(ab) {
  if(!is.integer(ab)){
    return(c(ab[1]/ab[2], 1))
  }
  a <- ab[1]
  b <- ab[2]
  while (TRUE) {
    r <- a%%b
    a <- b
    if (!r) 
      return(ab%/%b)
    b <- r
  }
  stop("Something wrong")
}


Scale2one <- function(x) {
  if (!length(x))
    return(FALSE)
  if (x[1] == 1L)
    return(FALSE)
  if (length(x) == 1L)
    return(TRUE)
  identical(min(x), max(x))
}




# Special version of DummyDuplicated(x, idx = TRUE, rnd = TRUE)
# Some 0’s changed to other values 
DummyDuplicatedSpec <- function(x, candidates, primary, forced) {
  
  xtu <- XprodRnd(x = x, duplic = FALSE, idx = FALSE, seed = 123)
  
  if(length(primary)) xtu[primary][xtu[primary] == 0] <- -1L   # negative values are unused
  if(length(forced))  xtu[forced][xtu[forced] == 0] <- -2L
  
  # to ensure whenEmptyUnsuppressed message as without removeDuplicated
  cand0 <- candidates[xtu[candidates] == 0]
  cand0 <- cand0[!(cand0 %in% primary)]
  cand0 <- cand0[!(cand0 %in% forced)]
  cand0 <- cand0[length(cand0)]
  xtu[cand0] <- -3L
  
  match(xtu, xtu)
}
# # Test using GaussSuppression that DummyDuplicatedSpec works as expected
# library(GaussSuppression)
# z3 <- SSBtoolsData("z3")
# set.seed(102)
# a <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, candidates = sample(1350), forced = sample(1350, size = 50), primary = sample(1350, size = 300), 
#                               singletonMethod = "none", whenEmptyUnsuppressed = warning)
# aw <- length(warnings())
# set.seed(102)
# b <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, candidates = sample(1350), forced = sample(1350, size = 50), primary = sample(1350, size = 300), 
#                               singletonMethod = "none", whenEmptyUnsuppressed = warning, removeDuplicated = FALSE)
# bw <- length(warnings())
# 
# # TRUE TRUE
# identical(a, b)
# identical(c(aw, bw), 4:3)



# Some of the code is similar to GaussSuppression:::FindDifferenceCells
# Example: mm <- ModelMatrix(SSBtoolsData("sprt_emp_withEU")[1:6, 1:2])
#          FindDiffMatrix(mm[, 5:6], mm[, c(1, 5)])
FindDiffMatrix <- function(x, y = x, max_colSums_diff = Inf) {
  xty <- As_TsparseMatrix(crossprod(x, y))
  colSums_y_xty_j_1 <- colSums(y)[xty@j + 1]
  # finds children in x and parents in y
  r <- colSums(x)[xty@i + 1] == xty@x & 
       colSums_y_xty_j_1     != xty@x & 
       (colSums_y_xty_j_1 - xty@x) <= max_colSums_diff
  child <- xty@i[r] + 1L
  parent <- xty@j[r] + 1L
  diff_matrix <- y[, parent, drop = FALSE] - 
                 x[, child, drop = FALSE]
  colnames(diff_matrix) <- parent
  diff_matrix
}



#High frequency unique values ordered last 
Order_singleton_num <- function(x) {
  y <- x[x > 0]
  tt <- table(y)
  z <- rep(0, length(x))
  z[x > 0] <- tt[as.character(y)]
  order(z, x)
}

