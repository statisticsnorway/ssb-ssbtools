

#' Decoding of `singletonMethod`
#' 
#' A `singletonMethod` starting with `"num"` is decoded into internal variables 
#' in \code{\link{GaussSuppression}}
#' 
#' The output variables have the following meaning, when `TRUE`:
#' * `singleton2Primary` (1st character): Published singletons are forced to be primary suppressed. 
#' * `integerUnique` (2nd character): Integer values representing the unique contributors are utilized. 
#' * `sub2Sum` (3rd character, `T` or `H`): Imaginary primary suppressed cells are made, 
#'       which are the sum of some suppressed inner cells and which can be divided into two components.
#'       At least one component is singleton contributor. The other component may be an inner cell. 
#' * `hierarchySearch` (3rd character, `H`): As `sub2Sum` above. 
#'       And in addition, the other component can be any primary suppressed published cell.
#'
#' @param singletonMethod String to be decoded 
#'
#' @return A list or `NULL`
#' @export
#'
#' @examples
#' unlist(NumSingleton("numTFF"))
#' unlist(NumSingleton("numFTT"))
#' unlist(NumSingleton("numTTH"))
NumSingleton <- function(singletonMethod) {
  if (substring(singletonMethod, 1, 3) != "num") {
    return(NULL)
  }
  singletonMethod <- paste0(singletonMethod, "FFFFF")
  s <- strsplit(substring(singletonMethod, 4, 6), split = "")[[1]]
  s <- s[c(1, 2, 3, 3)]
  if (s[3] == "H") {
    s[3] <- "T"
    s[4] <- "T"
  } else {
    s[4] <- "F"
  }
  s <- as.logical(s)
  names(s) <- c("singleton2Primary", "integerUnique", "sub2Sum", "hierarchySearch")
  if (anyNA(s)) {
    stop("Unknown singletonMethod")
  }
  as.list(s)
}
  
