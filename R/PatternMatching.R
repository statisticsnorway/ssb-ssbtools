#' Find variable combinations by advanced wildcard/globbing specifications.
#' 
#' Find combinations present in an input data frame or, when input is a list, 
#' find all possible combinations that meet the requirements.
#' 
#' The final variable combinations must meet the requirements in each positive sign group 
#' and must not match the requirements in the negative sign groups.The function is implemented by 
#' calling \code{\link{WildcardGlobbing}} several times within an algorithm that uses 
#' hierarchical clustering (\code{\link{hclust}}).  
#'
#'
#' @param z list or data.frame
#' @param wg data.frame with data globbing and wildcards
#' @param useUnique Logical variable about recoding within the algorithm. By default (NULL) an automatic decision is made. 
#' @param useFactor When TRUE, internal factor recoding is used.
#' @param makeWarning When TRUE, warning is made in cases of unused variables. Only variables common to z and wg are used. 
#' @param printInfo When TRUE, information is printed during the process.
#' @param useMatrixToDataFrame When TRUE, special functions (DataFrameToMatrix/MatrixToDataFrame) 
#'             for improving speed and memory is utilized.
#'
#' @return data.frame
#' @importFrom stats dist hclust
#' @importFrom utils flush.console
#' @export
#'
#' @examples                   
#' # useUnique=NULL betyr valg ut fra antall rader i kombinasjonsfil
#' data(precip)
#' data(mtcars)
#' codes <- as.character(c(100, 200, 300, 600, 700, 101, 102, 103, 104, 134, 647, 783, 
#'                         13401, 13402, 64701, 64702))
#' 
#' 
#' # Create list input
#' zList <- list(car = rownames(mtcars), wt = as.character(1000 * mtcars$wt), 
#'               city = names(precip), code = codes)
#' 
#' # Create data.frame input
#' m <- cbind(car = rownames(mtcars), wt = as.character(1000 * mtcars$wt))
#' zFrame <- data.frame(m[rep(1:NROW(m), each = 35), ], 
#'                      city = names(precip), code = codes, stringsAsFactors = FALSE)
#' 
#' # Create globbing/wildcards input
#' wg <- data.frame(rbind(c("Merc*", ""    , ""    , "?00"  ), 
#'                        c("F*"   , ""    , ""    , "?????"), 
#'                        c(""     , "???0", "C*"  , ""     ), 
#'                        c(""     , ""    , "!Co*", ""     ), 
#'                        c(""     , ""    , "?i*" , "????2"), 
#'                        c(""     , ""    , "?h*" , "????1")), 
#'            sign = c("+", "+", "+", "+", "-", "-"), stringsAsFactors = FALSE)
#' names(wg)[1:4] <- names(zList)
#' 
#' 
#' 
#' # =================================================================== 
#' #   Finding unique combinations present in the input data frame
#' # ===================================================================
#' 
#' 
#' # Using first row of wg. Combinations of car starting with Merc 
#' # and three-digit code ending with 00
#' HierarchicalWildcardGlobbing(zFrame[, c(1, 4)], wg[1, c(1, 4, 5)])
#' 
#' # Using first row of wg. Combinations of all four variables
#' HierarchicalWildcardGlobbing(zFrame, wg[1, ])
#' 
#' # More combinations when using second row also
#' HierarchicalWildcardGlobbing(zFrame, wg[1:2, ])
#' 
#' # Less combinations when using third row also 
#' # since last digit of wt must be 0 and only cities starting with C
#' HierarchicalWildcardGlobbing(zFrame, wg[1:3, ])
#' 
#' 
#' # Less combinations when using fourth row also since city cannot start with Co
#' HierarchicalWildcardGlobbing(zFrame, wg[1:4, ])
#' 
#' # Less combinations when using fourth row also 
#' # since specific combinations of city and code are removed
#' HierarchicalWildcardGlobbing(zFrame, wg)
#' 
#' 
#' # =================================================================== 
#' #  Using list input to create all possible combinations
#' # ===================================================================
#' 
#' dim(HierarchicalWildcardGlobbing(zList, wg))
#' 
#' # same result with as.list since same unique values of each variable
#' dim(HierarchicalWildcardGlobbing(as.list(zFrame), wg))
HierarchicalWildcardGlobbing <- function(z, wg, useUnique = NULL, useFactor = FALSE, makeWarning = TRUE, printInfo = FALSE, useMatrixToDataFrame = TRUE) {
  if (any(!(names(wg) %in% c("sign", names(z))))) {
    if (makeWarning) 
      warning("wg: Unique variant of common variables used")
    
    wg <- wg[, names(wg) %in% c("sign", names(z)), drop = FALSE]
    wg <- unique(wg)
    # Tar bort rader med bare ''
    wg <- wg[rowSums(0 + as.matrix(wg[, !(names(wg) %in% "sign")] != "")) != 0, , drop = FALSE]
  }
  
  if (any(!(c("sign", names(z)) %in% names(wg)))) {
    if (makeWarning) 
      warning("z: Common variables used (unique variant)")
    z <- z[(c("sign", names(z)) %in% names(wg))[-1]]
    if (is.data.frame(z)) 
      z <- unique(z)
  }
  
  # Endrer rekkefølge i wg til samme rekkefølge som dataene i z Noe kode under forutsetter det (crossMerge) Burde vært samkjørt med det over
  signInd <- match("sign", names(wg))
  zInd <- match(names(z), names(wg))
  
  wg <- wg[, c(zInd, signInd), drop = FALSE]
  
  # x er versjon av z der elementene er data.frame med en kolonne
  x <- vector("list", length(z))
  names(x) <- names(z)
  for (i in 1:length(z)) {
    # x[[i]] = as.data.frame(unique(z[i]))
    x[[i]] <- as.data.frame(z[i])
    # if(is.data.frame(z))
    if (useFactor) 
      x[[i]][[1]] <- factor(unique(x[[i]][[1]])) else x[[i]] <- CharacterDataFrame(unique(x[[i]]))
  }
  
  # useUnique=FALSE lønner seg med kostradata , men det var før 146 sekunder mot 234 sekunder
  
  # useFactor=FALSE lønner seg også 155 mot 239 sekunder
  
  
  signNr <- which(names(wg) == "sign")
  
  dummyWG <- as.data.frame(wg != "")
  dummyWG$sign <- wg$sign != "-"
  
  rg <- RowGroups(dummyWG, returnGroups = TRUE)
  
  nGroups <- NROW(rg$groups)
  
  
  namesFalse <- rownames(rg$groups)[!rg$groups[, signNr]]
  
  
  # Gir dobbelt vekt til TRUE
  groupsMatrix <- t(rbind(as.matrix(rg$groups[, -signNr])[rg$groups[, signNr], , drop = FALSE], as.matrix(rg$groups[, -signNr])))
  
  
  # Tar bort enslige men sørger for at minst en er med
  for (i in seq_len(NROW(groupsMatrix))) {
    cTRUE <- groupsMatrix[i, ]
    cs <- colSums(groupsMatrix[, cTRUE, drop = FALSE] + 0)
    if (any(cs > 1)) {
      cTRUE[cTRUE][cs > 1] <- FALSE
      groupsMatrix <- groupsMatrix[, !cTRUE, drop = FALSE]
    }
  }
  
  
  di <- dist(groupsMatrix, method = "binary")
  
  
  # Reduserer hver variabel først med positive sign
  for (i in 1:length(z)) {
    # print(names(z)[i]) print(rg$groups$sign)
    for (j in seq_len(nGroups)[as.logical(rg$groups$sign * rg$groups[, names(z)[i]])]) # print(j)
      x[[i]] <- x[[i]][WildcardGlobbing(x[[i]], wg[rg$idx == j, names(z)[i], drop = FALSE], sign = TRUE), , drop = FALSE]
  }
  
  
  usedGroups <- rep(FALSE, NROW(rg$groups))
  nTRUE <- rowSums(rg$groups[, -signNr, drop = FALSE] + 0)
  usedGroups[nTRUE == 1] <- TRUE  # Fra hver variabel over
  
  # Reduserer hver variabel med negative sign (kun grupper med en variabel) if(FALSE){
  for (i in 1:length(z)) {
    # print(names(z)[i])
    for (j in seq_len(nGroups)[as.logical((!rg$groups$sign) * rg$groups[, names(z)[i]])]) if (nTRUE[j] == 1) 
      x[[i]] <- x[[i]][WildcardGlobbing(x[[i]], wg[rg$idx == j, names(z)[i], drop = FALSE], sign = FALSE), , drop = FALSE]
  }
  
  
  if (length(x) == 1) {
    rownames(x[[1]]) <- NULL
    return(x[[1]])
  }
  
  
  ## Må redusere negative også ved enkeltvariabler i negative grupper
  
  
  hc <- hclust(di, method = "single")
  
  y <- vector("list", NROW(hc$merge))  # clusters
  
  
  
  for (i in seq_len(NROW(hc$merge))) {
    y[[i]] <- crossMerge(hc$merge[i, 1], hc$merge[i, 2], x, y, useMatrixToDataFrame = useMatrixToDataFrame)
    
    if (printInfo) {
      cat("\n\n", paste(names(y[[i]]), collapse = ":"), "  ", NROW(y[[i]]), "elements\n")
    }
    
    if (is.data.frame(z)) {
      matchyz <- Match(y[[i]], z[, names(y[[i]]), drop = FALSE])
      y[[i]] <- y[[i]][!is.na(matchyz), , drop = FALSE]
      if (printInfo) {
        cat("           Matching combinations  ", NROW(y[[i]]), "elements\n")
      }
    }
    
    # Må plukke ut rader i rg$groups[,-signNr] som ikke er med i usedGroups og hvor alle TRUE er blant variablene i y[[i]]
    
    
    newGroups <- which(rowSums((rg$groups[, -signNr] + 0)[, !(names(rg$groups[, -signNr]) %in% names(y[[i]])), drop = FALSE]) == 0 & !usedGroups)
    
    if (length(newGroups) > 0) {
      
      usedGroups[newGroups] <- TRUE
      # rekkefølge: tar alle TRUE først og starter med de med færres true de med signNr FALSE kommer til slutt
      ord <- order(nTRUE[newGroups] + 10^4 * nTRUE[newGroups] * as.integer(!rg$groups[newGroups, signNr]))
      newGroups <- newGroups[ord]
      
      for (j in newGroups) {
        vars <- names(rg$groups[, -signNr])[as.vector(as.matrix(rg$groups[j, -signNr]))]
        
        if (is.null(useUnique)) 
          useUniqueHere <- sum(rg$idx == j) > 10  # 10 valgt gangske virkålig, men lager skille i Kostra ..
        else useUniqueHere <- useUnique
        
        if (length(vars) < dim(y[[i]])[2] & useUniqueHere) {
          
          cat(" [ RowGroups...")
          flush.console()
          rgy <- RowGroups(y[[i]][, vars, drop = FALSE], returnGroups = TRUE)
          cat("]")
          flush.console()
          selg <- WildcardGlobbing(rgy$groups, wg[rg$idx == j, vars, drop = FALSE], sign = rg$groups[j, signNr])
          sel <- selg[rgy$idx]
        } else {
          sel <- WildcardGlobbing(y[[i]][, vars, drop = FALSE], wg[rg$idx == j, vars, drop = FALSE], sign = rg$groups[j, signNr])
        }
        y[[i]] <- y[[i]][sel, , drop = FALSE]
        if (printInfo) {
          cat("          ", paste(vars, collapse = c("-", "+")[1 + as.integer(rg$groups[j, signNr])]), "  ", NROW(y[[i]]), "elements\n")
        }
        
      }
    }
    
  }
  rownames(y[[length(y)]]) <- NULL
  y[[length(y)]]
}




#' Row selection by wildcard/globbing
#' 
#' The selected rows match combined requirements for all variables. 
#' 
#' This function is used by \code{\link{HierarchicalWildcardGlobbing}}
#' and \code{\link{WildcardGlobbingVector}} and make use of 
#' \code{\link{grepl}} and \code{\link{glob2rx}}.
#' 
#'
#' @param x data.frame with character data
#' @param wg data.frame with wildcard/globbing
#' @param sign When FALSE, the result is inverted. 
#' @param invert Character to invert each single selection.
#'
#' @return Logical vector defining subset of rows. 
#' @importFrom utils glob2rx 
#' @importFrom stringr str_split 
#' @export
#'
#' @examples
#' # Create data input
#' data(precip)
#' data(mtcars)
#' x <- data.frame(car = rownames(mtcars)[rep(1:NROW(mtcars), each = 35)], city = names(precip), 
#'                 stringsAsFactors = FALSE)
#' 
#' # Create globbing/wildcards input
#' wg <- data.frame(rbind(c("Merc*", "C*"), c("F*", "??????"), c("!?????????*", "!???????*")), 
#'                  stringsAsFactors = FALSE)
#' names(wg) <- names(x)
#' 
#' # Select the following combinations:
#' # - Cars starting with Merc and cities starting with C
#' # - Cars starting with F and six-letter cities 
#' # - Cars with less than nine letters and cities with less than seven letters
#' x[WildcardGlobbing(x, wg), ]
WildcardGlobbing <- function(x, wg, sign = TRUE, invert = "!") {
  sel <- rep(FALSE, dim(x)[1])
  nCOLwg <- NCOL(wg)
  wgijLast <- rep("*", nCOLwg)
  selijLast <- vector("list", nCOLwg)
  selijLast[[1]] <- rep(TRUE, NROW(x))
  for (j in matlabColon(2, nCOLwg)) selijLast[[j]] <- selijLast[[1]]
  for (i in 1:NROW(wg)) {
    seli <- rep(TRUE, dim(x)[1])
    for (j in 1:nCOLwg) {
      if (!(wg[i, j] == wgijLast[j])) 
      {
        wgijLast[j] <- wg[i, j]
        
        wgss <- str_split(wg[i, j], invert)[[1]]
        
        if (length(wgss) == 1) 
          selijLast[[j]] <- grepl(glob2rx(wgss), x[, names(wg)[j]]) else selijLast[[j]] <- !grepl(glob2rx(wgss[2]), x[, names(wg)[j]])
      }  #else
      # cat('æ')
      seli <- seli & selijLast[[j]]
    }
    sel <- sel | seli
  }
  
  if (!sign) 
    return(!sel)
  sel
}


#' Selection of elements by wildcard/globbing
#'
#' @param x Character vector
#' @param wg Character vector with wildcard/globbing
#' @param negSign Character representing selection to be removed
#' @param invert Character to invert each single selection.
#'
#' @return vector with selected elements of x
#' @export
#'
#' @examples
#' data(precip)
#' x <- names(precip)
#' 
#' # Select the cities starting with B, C and Sa.
#' WildcardGlobbingVector(x, c("B*", "C*", "Sa*"))
#' 
#' # Remove from the selection cities with o and t in position 2 and 4, respectively.
#' WildcardGlobbingVector(x, c("B*", "C*", "Sa*", "-?o*", "-???t*"))
#' 
#' # Add to the selection cities not having six or more letters.
#' WildcardGlobbingVector(x, c("B*", "C*", "Sa*", "-?o*", "-???t*", "!??????*"))
WildcardGlobbingVector <- function(x, wg, negSign = "-", invert = "!") {
  a <- str_split(wg, negSign, simplify = TRUE, n = 2)
  sig <- !(a[, 1] == "")
  sign <- rep("+", length(sig))
  sign[!sig] <- "-"
  wg[!sig] <- a[!sig, 2]
  
  wg <- data.frame(x = wg, sign = sign)
  
  z <- data.frame(x = x)
  
  HierarchicalWildcardGlobbing(z, wg = wg)[[1]]
}

#' crossMerge
#'
#' @param ind1 ind1
#' @param ind2 ind2
#' @param x x
#' @param y y
#'
#' @return
#' @keywords internal
#'
crossMerge <- function(ind1, ind2, x, y, useMatrixToDataFrame = TRUE) {
  if (ind1 < 0) 
    z1 <- x[[-ind1]] else z1 <- y[[ind1]]
    if (ind2 < 0) 
      z2 <- x[[-ind2]] else z2 <- y[[ind2]]
      CrossCodeFrames(z1, z2, useMatrixToDataFrame = useMatrixToDataFrame)
}


