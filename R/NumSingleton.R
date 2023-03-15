

#' Decoding of `singletonMethod`
#' 
#' A \code{\link{GaussSuppression}} `singletonMethod` starting with `"num"` is decoded into separate characters.
#' 
#' Any `F` means the feature is turned off.  
#' Other characters have the following meaning:
#' 
#' 1. `singleton2Primary` (1st character): 
#'   \itemize{
#'   \item `T`: All singletons are forced to be primary suppressed. 
#'   \item `t`: Non-published singletons are primary suppressed.
#'   }
#' 2. `integerUnique` (2nd character): 
#'   \itemize{
#'   \item `T`: Integer values representing the unique contributors are utilized.  Error if `singleton` not supplied as integer.  
#'   \item `t`: As `T` above, but instead of error, the feature is turned off  (as `F`) if `singleton` is not supplied as integer.   
#'   }
#' 3. `sum2` (3rd character): 
#'  \itemize{
#'   \item `T`: Imaginary primary suppressed cells are made, 
#'       which are the sum of some suppressed inner cells and which can be divided into two components.
#'       At least one component is singleton contributor. The other component may be an inner cell. 
#'   \item `H`: As `T` above. And in addition, the other component can be any primary suppressed published cell. 
#'       This method may be computationally demanding for big data.  
#'   }
#'
#' @param singletonMethod String to be decoded. If necessary, the input string is extended with `F`'s. 
#'
#' @return A character vector or `NULL`
#' @export
#'
#' @examples
#' NumSingleton("numTFF")
#' NumSingleton("numFTT")
#' NumSingleton("numttH")
NumSingleton <- function(singletonMethod) {
  if (substring(singletonMethod, 1, 3) != "num") {
    return(NULL)
  }
  singletonMethod <- paste0(singletonMethod, "FFFFF")
  s <- strsplit(substring(singletonMethod, 4, 6), split = "")[[1]]
  
  CheckChar(s[1], "1st", "FTt")
  CheckChar(s[2], "2nd", "FTt")
  CheckChar(s[3], "3rd", "FTH")
  names(s) <- c("singleton2Primary", "integerUnique", "sum2")
  s
}
  

CheckChar <- function(char, str = "1st", ok = "FT") {
  ok_char <- strsplit(ok, split = "")[[1]]
  if (!(char %in% ok_char)) {
    stop(paste(str, "character must be in: ", paste(ok_char, collapse = ", ")))
  }
  NULL
}


