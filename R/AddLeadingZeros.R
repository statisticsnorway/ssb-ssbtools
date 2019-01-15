#' Add leading zeros to numbers while preserving other text
#' 
#' This function is created to fix problems caused by a serious bug in Excel. 
#' Editing csv files in that program causes leading zeros to disappear.
#'
#' @param codes Character vector
#' @param places Number of places for positive numbers. Minus sign is extra 
#' @param warningText When non-NULL, warning will be produced
#' @param viaFactor When TRUE, the algorithm uses factor coding internally. 
#' @param nWarning Number of elements to be written before ... in warnings.
#' @param removeLeadingTrailingWhitespace Remove leading and trailing whitespace
#'
#' @return
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
#'                   "7 James Bond "), 10)
#' AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
#'                   "7 James Bond "), 4)
#' AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
#'                   "7 James Bond "), 4, removeLeadingTrailingWhitespace = FALSE)
#' AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
#'                   "7 James Bond "), 4, warningText = "string changes")
#' AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
#'                   "7 James Bond "), 4, warningText = "", nWarning = 2)
AddLeadingZeros <- function(codes, places, warningText = NULL, viaFactor = TRUE, nWarning = 6, removeLeadingTrailingWhitespace = TRUE) {
  if (!is.character(codes)) {
    stop("codes is not character")
  }
  if (viaFactor) {
    codesF <- as.factor(codes)
    x <- attr(codesF, "levels")
  } else {
    x <- codes
  }
  
  xTrimChanged <- FALSE
  if (removeLeadingTrailingWhitespace) {
    xOld <- x
    x <- trimws(x)
    if (any(xOld != x)) {
      xTrimChanged <- TRUE
      if (!is.null(warningText)) {
        ux <- sort(unique(xOld[xOld != x]))
        if (length(ux) > nWarning + 2) {
          # Bad programming copy of code below
          uxEnd <- ux[length(ux)]
          ux <- ux[seq_len(nWarning + 2)]
          ux[nWarning + 2] <- uxEnd
          ux[nWarning + 1] <- "..."
        }
        warning(paste("Whitespace removed", warningText, paste(ux, collapse = ", "), sep = ": "))
      }
    }
    rm(xOld)
  }
  
  
  
  z <- suppressWarnings(as.integer(x))
  neg <- suppressWarnings(min(z, na.rm = TRUE)) < 0
  zChar <- as.character(z)
  zChar[is.na(zChar)] <- "NA"
  
  
  
  nChar <- nchar(zChar)
  if (neg) {
    negZ <- z < 0
    negZ[is.na(negZ)] <- FALSE
    z <- abs(z)
  } else negZ <- rep(FALSE, length(x))
  k <- (zChar == x) & (nChar < (places + as.integer(negZ)))
  if (sum(k)) {
    x[k] <- Number(z[k], places)
    if (sum(negZ & k)) {
      x[negZ & k] <- paste("-", x[negZ & k], sep = "")
    }
    if (!is.null(warningText)) {
      ux <- sort(unique(x[k]))
      if (length(ux) > nWarning + 2) {
        uxEnd <- ux[length(ux)]
        ux <- ux[seq_len(nWarning + 2)]
        ux[nWarning + 2] <- uxEnd
        ux[nWarning + 1] <- "..."
      }
      warning(paste(warningText, paste(ux, collapse = ", "), sep = ": "))
    }
  } else {
    if (!xTrimChanged) 
      return(codes)
  }
  if (!viaFactor) 
    return(x)
  attr(codesF, "levels") <- x
  as.character(codesF)
}


#' Adding leading zeros
#'
#' @param n  numeric vector of whole numbers
#' @param width width
#'
#' @return
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
Number <- function(n, width = 3) {
  s <- "s <- sprintf('%0d', n)"
  s <- gsub("0", as.character(width), s)
  eval(parse(text = s))
  s <- gsub(" ", "0", s)
  s
}
