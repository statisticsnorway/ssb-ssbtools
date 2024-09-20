

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
#'   \item `T`: Virtual primary suppressed cells are made, 
#'       which are the sum of some suppressed inner cells and which can be divided into two components.
#'       At least one component is singleton contributor. The other component may be an inner cell. 
#'   \item `H`: As `T` above. And in addition, the other component can be any primary suppressed published cell. 
#'       This method may be computationally demanding for big data.  
#'   }
#' 4. `elimination` (4th character): 
#'   \itemize{
#'   \item `t`: The singleton problem will be handled by methodology implemented as a part of the Gaussian elimination algorithm.
#'   \item `m`: As `t` above. And in addition, a message will be printed to inform about problematic singletons.
#'       Actual reveals will be calculated when `singleton2Primary = T` (1st character) 
#'       and when `singleton2Primary = t` yield the same result as `singleton2Primary = T`.
#'       Problematic singletons can appear since the algorithm is not perfect in the sense that the elimination of rows may cause problems. 
#'       Such problems can be a reason not to switch off `sum2`.
#'   \item `w`: As `m` above, but `warning` instead of `message`.
#'   \item `T`, `M` and `W`: As `t`, `m` and `w` above. 
#'       In addition, the gauss elimination routine is allowed to run in parallel with different sortings 
#'      so that the problem of eliminated singleton rows is reduced.  
#'   \item `f`: As `F`, which means that the elimination feature is turned off. 
#'      However, when possible, a message will provide information about actual reveals, similar to `m` above.                  
#'   }
#' 5. `combinations` (5th character): 
#'   \itemize{
#'   \item `T`: This is a sort of extension of `singleton2Primary` which is relevant when both `integerUnique` and `elimination` are used. 
#'      For each unique singleton contributor, the method seeks to protect all linear combinations of singleton cells from the unique contributor.
#'      Instead of construction new primary cells, protection is achieved as a part of the elimination procedure. 
#'      Technically this is implemented by extending the above `elimination` method.  
#'      It cannot be guaranteed that all problems are solved, and this is a reason not to turn off `singleton2Primary`. 
#'      Best performance is achieved when `elimination` is `T`, `M` or `W`. 
#'   \item `t`:  As `T`, but without the added singleton protection. 
#'      This means that protected linear combinations cannot be calculated linearly from non-suppressed cells. 
#'      However, other contributors may still be able to recalculate these combinations using their own suppressed values.
#'   }
#'
#' @param singletonMethod String to be decoded. If necessary, the input string is extended with `F`'s. 
#'
#' @return A character vector or `NULL`
#' @export
#' 
#' @references 
#' Langsrud, Ø. (2024): 
#' \dQuote{Secondary Cell Suppression by Gaussian Elimination: An Algorithm Suitable for Handling Issues with Zeros and Singletons}. 
#'  Presented at: \emph{Privacy in statistical databases}, Antibes, France. September 25-27, 2024.
#' \doi{10.1007/978-3-031-69651-0_6}
#' 
#'
#' @examples
#' NumSingleton("numTFF")
#' NumSingleton("numFtT")
#' NumSingleton("numttH")
#' NumSingleton("numTTFTT")
NumSingleton <- function(singletonMethod) {
  if (substring(singletonMethod, 1, 3) != "num") {
    return(NULL)
  }
  singletonMethod <- paste0(singletonMethod, "FFFFF")
  s <- strsplit(substring(singletonMethod, 4, 8), split = "")[[1]]
  
  CheckChar(s[1], "1st", "FTt")
  CheckChar(s[2], "2nd", "FTt")
  CheckChar(s[3], "3rd", "FTH")
  CheckChar(s[4], "4th", "FTMWftmw")
  CheckChar(s[5], "5th", "FTt")
  names(s) <- c("singleton2Primary", "integerUnique", "sum2", "elimination", "combinations")
  s
}
  

CheckChar <- function(char, str = "1st", ok = "FT") {
  ok_char <- strsplit(ok, split = "")[[1]]
  if (!(char %in% ok_char)) {
    stop(paste(str, "character must be in: ", paste(ok_char, collapse = ", ")))
  }
  NULL
}


