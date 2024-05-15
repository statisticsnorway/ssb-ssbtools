#' Checking function inputs 
#'
#' An input vector (of length one unless `okSeveral` is `TRUE`) is checked.
#'
#' @param x Input vector to be checked
#' @param alt `NULL` or vector of allowed values
#' @param min `NULL` or minimum value (when `type` is numeric or integer)
#' @param max `NULL` or maximum value (when `type` is numeric or integer)
#' @param type One of: `"character"`, `"numeric"`, `"integer"`, `"logical"`, `"varName"`, `"varNr"`, `"varNrName"`.
#'        numeric/integer is not checked against exact class, but whether the value fit into the class.
#'        Also see data below.
#' @param data A data frame or matrix. When above type is `varNames`, `x` is checked against `colnames(data)`.
#'        When type is `varNr`, `x` is checked against column numbers.
#'        When type is `varNrName`, `x` can be either column numbers or column names.
#' @param okSeveral When `TRUE`, `length(x)>1` is allowed
#' @param okNULL When `TRUE`, `NULL` is allowed
#' @param okNA   When `TRUE`, `NA` is allowed
#' @param okDuplicates When `TRUE`, duplicated values are allowed.
#'          Default is `TRUE` if `alt` is `NULL` and if `type` does not refer to column(s) of `data`.
#'
#' @details `x` is checked according to the other input parameters.
#'          When `x` is wrong an error is produced with appropriate text.
#'          
#'  *The function was originally created in 2016 and has been included in 
#'  internal packages at Statistics Norway (SSB). Due to its widespread use, 
#'  it was beneficial to include it in this CRAN package.*
#'
#' @return NULL
#'
#' @author Øyvind Langsrud
#'
#' @export
#'
#' @examples
#'a <- c("no", "yes")
#'b <- c(3.14, 4, 5)
#'z <- data.frame(A = a, B = b[1:2], C = TRUE)
#'
#'# Lines causing error are embedded in 'try'
#'
#'try(CheckInput(a, type = "character"))
#'CheckInput(a, type = "character", alt = c("no", "yes", "dontknow"), okSeveral = TRUE)
#'try(CheckInput("yesno", type = "character", alt = c("no", "yes", "dontknow")))
#'CheckInput(a[1], type = "character", alt = c("no", "yes", "dontknow"))
#'
#'try(CheckInput(b, type = "integer", max = 100, okSeveral = TRUE))
#'try(CheckInput(b, type = "numeric", min = 4, okSeveral = TRUE))
#'CheckInput(b, type = "numeric", max = 100, okSeveral = TRUE)
#'try(CheckInput(b, type = "numeric", alt = 1:10, okSeveral = TRUE))
#'CheckInput(b[2], type = "numeric", alt = 1:10)
#'
#'try(CheckInput("TRUE", type = "logical"))
#'CheckInput(TRUE, type = "logical")
#'
#'try(CheckInput("A", type = "varName"))
#'CheckInput("A", type = "varName", data = z)
#'CheckInput(c("A", "B"), type = "varNrName", data = z, okSeveral = TRUE)
#'try(CheckInput("ABC", type = "varNrName", data = z))
#'try(CheckInput(5, type = "varNrName", data = z))
#'CheckInput(3, type = "varNr", data = z)
#'CheckInput(2:3, type = "varNr", data = z, okSeveral = TRUE)
CheckInput <- function(x, alt = NULL, min = NULL, max = NULL, type = "character", data = NULL,
                       okSeveral = FALSE, okNULL = FALSE, okNA = FALSE,
                       okDuplicates = is.null(alt) & !(type %in% c("varName", "varNr", "varNrName"))){

  types <- c("character", "numeric", "integer", "logical", "varName", "varNr", "varNrName")

  xInput <- get_args()$x
  xInput <- deparse(xInput)  # These two lines cannot be written as one

  if(is.null(x)){
    if(okNULL) return()
    else stop(paste(xInput, ": NULL input not not allowed"))
  }

  if (type %in% c("varName", "varNr", "varNrName")){
    if (is.null(data))
      stop(paste(xInput, ": data missing"))
    if(is.list(x)) return() # No further checking here if complicated list input
  }

  if (!okSeveral & length(x) > 1)
    stop(paste(xInput, ": Input must be of length 1"))

  if (length(x) == 0)
    stop(paste(xInput, ": Input is of length 0"))

  if (any(is.na(x)))
    if(!okNA) stop(paste(xInput, ": NA input"))

  if(!okDuplicates & any(duplicated(x)))
    stop(paste(xInput, ": Duplicates not allowed"))

  if (!(type %in% types))
    stop(paste(xInput, ": Type must be in:", paste(types, collapse = ", ")))

  if (type == "varNrName") {
    if (is.character(x))
      type <- "varName" else type <- "varNr"
  }

  if (type == "varName") {
    type <- "character"
    alt <- colnames(data)
    if (is.null(alt))
      stop(paste(xInput, ": No colnames in data"))
  }

  if (type == "varNr") {
    type <- "integer"
    min <- 1
    max <- NCOL(data)
  }

  if (type == "integer") {
    if (!suppressWarnings(identical(as.numeric(as.integer(x)), as.numeric(x))))
      stop(paste(xInput, ": Input is not in accordance with integer"))
    type <- "numeric"
  }

  if (any(c(!is.null(min), !is.null(max)))) {
    if (type == "numeric") {
      if (!is.null(alt))
        warning(paste(xInput, ": alt not used when min/max is input"))
      if (is.null(min))
        min <- -Inf
      if (is.null(max))
        max <- Inf
      if (any(x < min, na.rm = TRUE) | any(x > max, na.rm = TRUE))
        stop(paste(xInput, ": Input must be within the interval [", min, ",", max, "]"))
      return()
    } else warning(paste(xInput, ": min/max not used"))
  }

  if (any(!is.na(x))){  # No check if only NA
    if (type == "numeric" & !is.numeric(x))
      stop(paste(xInput, ": Input must be numeric or integer"))
    if (type == "character" & !is.character(x))
      stop(paste(xInput, ": Input must be character"))
    if (type == "logical" & !is.logical(x))
      stop(paste(xInput, ": Input must be logical"))
  }

  if (!is.null(alt))
    if (any(!(x %in% alt)))
      stop(paste(xInput, ": Input must be in:", paste(HeadEnd(alt, n = 8), collapse = ", ")))

  return()
}

# Based on the discussion at http://stackoverflow.com/questions/17256834/getting-the-arguments-of-a-parent-function-in-r-with-names
get_args <- function() {
  cl <- sys.call(-1)
  # f <- get(as.character(cl[[1]]), mode='function', sys.frame(-2))
  f <- eval(cl[[1]], parent.frame())
  cl <- match.call(definition = f, call = cl)
  as.list(cl)[-1]
}

# HeadEnd(1:1000) '1' '2' '3' '4' '...'  '1000'
HeadEnd <- function(x, n = 4L) {
  x <- as.character(x)
  if (length(x) > (n + 2))
    x <- c(head(x, n = n), "...", tail(x, n = 1))
  x
}


#' @rdname CheckInput
#' @export
#' @note `check_input` and `CheckInput` are identical
check_input <- CheckInput

